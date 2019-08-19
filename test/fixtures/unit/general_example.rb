:zero_level_nesting

module One
  :one_level_nesting

  module Two
    def self.test
      :first_two_level_nesting
    end
  end

  module Three
    :second_two_level_nesting

    class Four < Object
      :three_level_nesting
    end
  end
end

module Five
  :second_one_level_nesting

  class Six
    :third_two_level_nesting
  end
end

:second_zero_level_nesting
