const fs = require("fs");
const isProduction = process.env.NODE_ENV === "production";
const scriptSrc = isProduction ? "main.min.js" : "main.js";
const htmlTemplate = fs.readFileSync("./index.html", "utf8");
const outputHtml = htmlTemplate.replace(/main\.js/g, scriptSrc);
fs.writeFileSync("./dist/index.html", outputHtml);
fs.copyFileSync("./style.css", "./dist/style.css");
