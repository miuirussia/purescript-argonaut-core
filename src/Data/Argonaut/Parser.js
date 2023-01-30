const codePoints = {
  "\\\"": "\"",
  "\\\\": "\\",
  "\\/": "/",
  "\\b": "\b",
  "\\f": "\f",
  "\\n": "\n",
  "\\r": "\r",
  "\\t": "\t",
};

function isNumberStart(char) {
  return Boolean(char.match(/^(-|\d)$/));
}

function isWhitespace(char) {
  return [" ", "\n", "\r", "\t"].includes(char);
}

function skipSpaces(source, startIndex) {
  let i;
  for (i = startIndex; i < source.length; i++) {
    const char = source[i];
    if (!isWhitespace(char)) {
      break;
    }
  }
  return i;
}

function skipUntil(source, startIndex, endChar) {
  let endChars = Array.isArray(endChar) ? endChar : [endChar];

  const i = skipSpaces(source, startIndex);

  const char = source[i];

  if (endChars.includes(char)) {
    return i;
  } else {
    throw new SyntaxError(
      `Unexpected character: "${char}" ` +
      `at position: ${i}`
    );
  }
}

function nextContext(context, nextIndex, nextKey) {
  const newContext = Object.assign(context, { index: nextIndex });

  if (nextKey) {
    newContext.keys = [
      ...context.keys,
      nextKey,
    ];
  }

  return newContext;
}

function identity(s) {
  return s;
}

function parseNumber(context) {
  const { source, index } = context;

  let isNegative = false;
  let integer = "0";
  let fraction = "";
  let isExponentNegative = false;
  let exponent = "";

  let i = index;

  if (source[i] === "-") {
    isNegative = true;
    i++;
  }

  if (source[i] === "0") {
    i++;
  } else if (source[i].match(/^[1-9]$/)) {
    integer = source[i];
    i++;

    while (i < source.length) {
      if (source[i].match(/^\d$/)) {
        integer += source[i];
        i++;
      } else {
        break;
      }
    }
  } else {
    throw new SyntaxError(
      `Failed to parse number at position: ${i}`
    );
  }

  if (source[i] === ".") {
    i++;

    while (i < source.length) {
      if (source[i].match(/^\d$/)) {
        fraction += source[i];
        i++;
      } else {
        break;
      }
    }
  }

  if (["e", "E"].includes(source[i])) {
    i++;

    if (source[i] === "+") {
      i++;
    } else if (source[i] === "-") {
      isExponentNegative = true;
      i++;
    }

    const exponentStartIndex = i;

    while (i < source.length) {
      if (source[i].match(/^\d$/)) {
        exponent += source[i];
        i++;
      } else {
        break;
      }
    }

    if (exponent.length === 0) {
      throw new SyntaxError(
        "Failed to parse number's exponent value " +
        `at position: ${exponentStartIndex}`
      );
    }
  }

  let value = context.createNumber(
    (isNegative ? "-" : "") + integer +
    (fraction ? `.${fraction}` : "") +
    (exponent ? `e${isExponentNegative ? "-" : ""}${exponent}` : "")
  );

  return {
    value,
    endIndex: i,
  };
}

function parseObject(context) {
  function isConstructorPrototype(key) {
    if (key !== "prototype") {
      return false;
    }

    const parentKey = (context.keys.length > 0 ? context.keys[context.keys.length - 1] : undefined);

    return (parentKey === "constructor");
  }

  function isAllowedKey(key) {
    return key !== "__proto__" && !isConstructorPrototype(key);
  }

  const { source, index } = context;

  let object = {};

  let i = (index + 1);
  let expectKeypair = false;

  while (i < source.length) {
    i = skipUntil(source, i, ["\"", "}"]);

    if (source[i] === "}" && !expectKeypair) {
      i++;
      break;
    }

    let result = parseString(nextContext(context, i));
    const key = result.value;
    i = result.endIndex;
    i = skipUntil(source, i, ":") + 1;

    if (key === "__proto__") {
      throw new SyntaxError(
        "Forbidden object property name: \"__proto__\""
      );
    } else if (isConstructorPrototype(key)) {
      throw new SyntaxError(
        "Forbidden object property path: \"constructor.prototype\""
      );
    }

    i = skipSpaces(source, i);
    result = parseValue(
      nextContext(context, i, key)
    );

    if (result.value !== undefined && isAllowedKey(key)) {
      object[key] = result.value;
    }
    i = result.endIndex;
    i = skipUntil(source, i, [",", "}"]);
    if (source[i] === ",") {
      expectKeypair = true;
      i++;
    } else if (source[i] === "}") {
      i++;
      break;
    }
  }

  return {
    value: context.createObject(object),
    endIndex: i,
  };
}

function parseString(context) {

  const { source, index } = context;

  let value = "";

  let i = (index + 1);

  while (i < source.length) {
    const char = source[i];

    if (char === "\\") {
      const twoChars = source.substring(i, i + 2);
      const codepoint = codePoints[twoChars];
      if (codepoint) {
        value += codepoint;
        i += 2;
      } else if (twoChars === "\\u") {
        const charHex = source.substring(i + 2, i + 6);
        value += String.fromCharCode(parseInt(charHex, 16));
        i += 6;
      } else {
        throw new SyntaxError(
          `Unknown escape sequence: "${twoChars}"`
        );
      }
    } else if (char === "\"") {
      i++;
      break;
    } else {
      value += char;
      i++;
    }
  }
  return { value, endIndex: i };
}

function parseArray(context) {
  const { source, index } = context;

  const array = [];

  let i = (index + 1);
  let expectElement = false;
  let elementIndex = 0;

  while (i < source.length) {
    i = skipSpaces(source, i);

    if (source[i] === "]" && !expectElement) {
      i++;
      break;
    }

    const result = parseValue(
      nextContext(context, i, elementIndex.toString())
    );

    array.push(result.value);
    i = result.endIndex;
    i = skipUntil(source, i, [",", "]"]);
    if (source[i] === ",") {
      expectElement = true;
      elementIndex++;
      i++;
    } else if (source[i] === "]") {
      i++;
      break;
    }
  }

  return {
    value: context.createArray(array),
    endIndex: i,
  };
}

function parseKeyword(context, keyword) {
  const { source, index } = context;

  const endIndex = (index + keyword.length);

  const slice = source.substring(index, endIndex);

  if (slice !== keyword) {
    throw new SyntaxError(
      `Failed to parse value at position: ${index}`
    );
  }

  let value = (keyword === "true" ? context.createBoolean(true) : keyword === "false" ? context.createBoolean(false) : context.createNull(null));

  return {
    value,
    endIndex,
  };
}

function parseValue(context) {
  const { source, index } = context;
  let result;
  let i = skipSpaces(source, index);
  const newContext = nextContext(context, i);

  if (isNumberStart(source[i])) {
    result = parseNumber(newContext);
  } else {
    switch (source[i]) {
      case "\"": {
        result = parseString(newContext);
        result.value = newContext.createString(result.value);
        break;
      }
      case "[": {
        result = parseArray(newContext);
        break;
      }
      case "{": {
        result = parseObject(newContext);
        break;
      }
      case "t": {
        result = parseKeyword(newContext, "true");
        break;
      }
      case "f": {
        result = parseKeyword(newContext, "false");
        break;
      }
      case "n": {
        result = parseKeyword(newContext, "null");
        break;
      }
      default: {
        throw new SyntaxError(
          `Unexpected character: "${source[i]}" ` +
          `at position: ${i}`
        );
      }
    }
  }

  return result;
}

function parseJson(source, createNumber, createString, createArray, createObject, createBoolean, createNull) {
  const result = parseValue({
    source,
    index: 0,
    keys: [],
    createNumber,
    createString,
    createArray,
    createObject,
    createBoolean,
    createNull,
  });

  const endIndex = skipSpaces(source, result.endIndex);

  if (endIndex <= (source.length - 1)) {
    throw new SyntaxError(
      "Unexpected extra characters after the parsed data: " +
      `"${source.substring(endIndex, endIndex + 16)}…" ` +
      `at position: ${endIndex}`
    );
  }

  return result.value;
}

export function _jsonParser(Left, Right, s) {
  try {
    return Right(parseJson(s));
  } catch (e) {
    return Left(e.message);
  }
}
