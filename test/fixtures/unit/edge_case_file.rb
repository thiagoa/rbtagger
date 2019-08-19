# This module is a dummy module for testing.
# These comments are part of the test. See this:
#
#     module Foo
#       class Bar
#       end
#     end
module Foo
  module Bar
    def bar
      @foo_class ||= Object
    end

    :where_the_error_could_happen
  end
end

module Bar
  class << self
    :singleton_class
  end
end
