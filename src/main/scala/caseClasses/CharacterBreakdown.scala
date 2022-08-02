package caseClasses

case class CharacterBreakdown(character: String, 
                              fullComponentString: String, 
                              firstComponent: String, 
                              componentList: List[ComponentInfo],
                              errorMessage: String)
