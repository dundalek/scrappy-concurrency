module.exports = function(config) {
  config.set({
    browsers: ['ChromeHeadless'],
    // browsers: ['Chrome'],
    // The directory where the output file lives
    basePath: 'target',
    // The file itself
    files: [
      'ci.js',
      {
        pattern: '**/*.js.map',
        included: false
      }],
    frameworks: ['cljs-test'],
    plugins: ['karma-cljs-test', 'karma-chrome-launcher'],
    colors: true,
    logLevel: config.LOG_INFO,
    client: {
      args: ["shadow.test.karma.init"],
      singleRun: true
    }
  })
};
