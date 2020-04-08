var staticDataStorage = {
    fetchLatestUpdates: function() {

    },
    fetchTimelineOfInterventions: function() {
        
    }
}

var todoStorage = {
    fetch: function() {
      var todos = JSON.parse(localStorage.getItem(STORAGE_KEY) || "[]");
      todos.forEach(function(todo, index) {
        todo.id = index;
      });
      todoStorage.uid = todos.length;
      return todos;
    },
    save: function(todos) {
      localStorage.setItem(STORAGE_KEY, JSON.stringify(todos));
    }
  };