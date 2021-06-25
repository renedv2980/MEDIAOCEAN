*          DATA SET GAICE02    AT LEVEL 024 AS OF 08/22/00                      
*          DATA SET GAICE02    AT LEVEL 022 AS OF 05/04/83                      
*PHASE TB1302A                                                                  
         TITLE 'ICE CREAM. SCREENS'                                             
SCREENS  CSECT                                                                  
*                                                                               
INDEX    DC    AL2(INSTR1-*),CL6'INSTR1'                                        
         DC    AL2(INSTR2-*),CL6'INSTR2'                                        
         DC    AL2(STATUS-*),CL6'STATUS'                                        
         DC    AL2(DAYSUM-*),CL6'DAYSUM'                                        
         DC    AL2(ENDSUM-*),CL6'ENDSUM'                                        
         DC    8X'FF'                                                           
*                                                                               
*        FIRST INSTRUCTIONS SCREEN                                              
*                                                                               
INSTR1   DS    0H'0'                                                            
         DC    AL2(INSTR1ND-(*+2))   LENGTH OF INSTRUCTIONS                     
 DC C'WELCOME TO ++++++ ICE-CREAM +++++++',X'00',X'00'                          
 DC C'THIS IS A GAME FOR UP TO THREE PLAYERS, IN WHICH THE AIM IS TO M'         
 DC C'AKE A FORTUNE IN "ICE-CREAM VENDING".',X'00',X'00'                        
 DC C'EACH DAY, YOU WILL BE TOLD THE DAY OF WEEK AND GIVEN A WEATHER F'         
 DC C'ORECAST. USING YOUR BUSINESS JUDGEMENT, YOU CAN DECIDE WHERE TO '         
 DC C'POSITION YOUR VAN(S), HOW MUCH ICE-CREAM EACH VAN SHOULD CARRY, '         
 DC C'AND HOW MUCH YOU ARE GOING TO CHARGE THE PUNTERS FOR A CORNET'            
 DC X'00',X'00'                                                                 
 DC C'OBVIOUSLY, THE AMOUNT YOU CHARGE HELPS TO DETERMINE HOW MUCH INC'         
 DC C'OME YOU WILL RECEIVE, BUT IT ALSO AFFECTS HOW MUCH YOU WILL SELL'         
 DC C' I.E. IF YOU TRY TO CHARGE FIVE POUNDS A CORNET, ..... WELL TRY '         
 DC C'IT AND SEE WHAT HAPPENS!!!.',X'00',X'00'                                  
 DC C'OTHER FACTORS AFFECT YOUR SALES - THE DAY OF WEEK, THE WEATHER, '         
 DC C'ANY SPECIAL CEREMONIES, PAGEANTS OR WHATEVER (WHICH MAY ONLY AFF'         
 DC C'ECT ONE LOCATION). AND, IF THERE IS MORE THAN ONE PLAYER, A CERT'         
 DC C'AIN AMOUNT OF HEALTHY COMPETITION MAY OCCUR, CERTAINLY PRICE WIL'         
 DC C'L DECIDE WHO GETS THE MOST BUSINESS.',X'00',X'00'                         
 DC C'VERY OCCASIONALLY, THINGS CAN GO WRONG AND YOU COULD LOSE SOME '          
 DC C'OR ALL OF YOUR BUSINESS IN A DAY. YOU WILL BE TOLD IF THIS HAPPE'         
 DC C'NS BUT IT IS VERY RARE.'                                                  
INSTR1ND EQU   *                                                                
*                                                                               
*        SECOND INSTRUCTIONS SCREEN                                             
*                                                                               
INSTR2   DS    0H'0'                                                            
         DC    AL2(INSTR2ND-(*+2))   LENGTH OF INSTRUCTIONS                     
 DC C'TO BEGIN WITH, EACH PLAYER IS GIVEN A SUM OF MONEY AND ONE VAN. '         
 DC C'THE BANK ALSO ALLOWS HIM A CERTAIN AMOUNT OF CREDIT. EACH DAY, H'         
 DC C'E HAS TO PAY THE GOING RATE FOR WHOLE-SALE ICE-CREAM, WHICH IS S'         
 DC C'OLD BY THE GALLON (ENOUGH FOR SIXTY CORNETS).',X'00',X'00'                
 DC C'AS THE GAME PROGRESSES, THE PRICE OF WHOLE-SALE ICE-CREAM WILL V'         
 DC C'ARY A LITTLE TO REFLECT THE MARKET, ALSO, A PLAYERS CREDIT LIMIT'         
 DC C' WILL VARY ACCORDING TO THE BANK MANAGER''S OPINION OF HIS BUSIN'         
 DC C'ESS ABILITY.',X'00',X'00'                                                 
 DC C'A POOL OF SECOND-HAND VANS WILL BE OFFERED FOR SALE, IF THE PLAY'         
 DC C'ER CAN AFFORD TO PAY THE GOING RATE (WHICH OF COURSE VARIES ACC'          
 DC C'ORDING TO THE MARKET). IF A PLAYER HAS MORE THAN ONE VAN, HE MAY'         
 DC C' CHOOSE TO SELL, IN WHICH CASE THE PRICE OFFERED REFLECTS THE AV'         
 DC C'AILABILITY OF SECOND-HAND VANS. BEAR IN MIND WHEN MAKING A DECIS'         
 DC C'ION THAT VANS COST $150 A DAY EACH TO RUN.',X'00',X'00'                   
 DC C'THERE IS NO CLEAR DEFINITION OF A WINNER - WAS HOWARD HUGHES A W'         
 DC C'INNER OR STILL IN THE GAME ..?? LOSERS, ON THE OTHER HAND, ARE T'         
 DC C'HOSE WHO FIND THEMSELVES OVERDRAWN BEYOND THEIR CREDIT LIMIT, UN'         
 DC C'ABLE TO SELL VANS AND UNABLE TO BUY WHOLESALE ICE-CREAM. I.E. BA'         
 DC C'NKRUPT.',X'00',X'00'                                                      
 DC C'PLEASE ENTER THE NUMBER OF PLAYERS - 1, 2 OR 3, OR ENTER HELP AG'         
 DC C'AIN.'                                                                     
INSTR2ND EQU   *                                                                
*                                                                               
*        PLAYER STATUS SCREEN                                                   
*                                                                               
STATUS   DS    0H'0'                                                            
         DC    AL2(STATUSND-(*+2))   LENGTH OF INSTRUCTIONS                     
 DC X'00',C'TODAY IS ',10X'FF',C' ',98X'FF',X'00'                               
 DC X'FE01',C'THERE ARE ',2X'FF',C' VANS FOR SALE TODAY. ',X'FEFE'              
 DC X'FE02',C'YOU ',5X'FF',C' BUY THEM FOR  ',5X'FF',C' EACH.',X'FEFE'          
 DC X'FE03',C'.... IF YOU COULD AFFORD IT !! ',X'FEFE'                          
 DC X'FE04',C'YOU CAN SELL VANS FOR ',5X'FF',C' EACH.',X'FEFE'                  
 DC X'FE05',X'00',X'FEFE'                                                       
 DC C'CURRENTLY, YOU OWN ',2X'FF',C' VANS. YOU ',13X'FF',C' ',11X'FF'           
 DC C', AND YOU HAVE A CREDIT LIMIT OF ',11X'FF',C'. '                          
 DC X'FE06',C'YESTERDAY, YOU MADE A ',6X'FF',C' OF ',11X'FF',C'.'               
 DC X'FEFE',X'00'                                                               
 DC C'WHOLESALE ICE CREAM WILL COST YOU ',6X'FF',C' PER GALLON TODAY'           
 DC C', EACH VAN CAN CARRY UP TO 50 GALLONS, ENOUGH TO MAKE 3000'               
 DC C' CONES. REMEMBER EACH VAN COSTS $150 PER DAY IN OVERHEADS '               
 DC C'EVEN IF IT ISN''T WORKING.',X'00'                                         
 DC X'FE07',C'YOU MIGHT LIKE TO KNOW THAT ',X'FEFE',93X'FF'                     
STATUSND EQU   *                                                                
*                                                                               
*        DAILY SUMMARY SCREEN                                                   
*                                                                               
DAYSUM   DS    0H'0'                                                            
         DC    AL2(DAYSUMND-(*+2))   LENGTH OF SCREEN                           
 DC C'DAILY SUMMARY',2X'00',C'TRADING FOR ',10X'FF',C' - ',15X'FF'              
 DC 2X'00',C'WEATHER WAS FORECAST AS ',9X'FF',C', '                             
 DC X'FE01',C'TRADING TODAY WAS GENERALLY AS EXPECTED, AND NO ADVERSE '         
 DC C'PROBLEMS AROSE. ',X'FEFE'                                                 
 DC X'FE02',74X'FF',X'FEFE'                                                     
 DC X'FE03',23X'FF',X'FEFE'                                                     
 DC X'FE04',10X'FF',C' LOST SOME BUSINESS BECAUSE ',74X'FF',X'FEFE'             
 DC X'FE05',10X'FF',C' CAME UNSTUCK WHEN ',74X'FF',X'FEFE'                      
 DC X'FE06',10X'FF',C' HAD PROBLEMS WHEN ',74X'FF',X'FEFE'                      
DAYSUMND EQU   *                                                                
*                                                                               
*        END OF GAME SCREEN                                                     
*                                                                               
ENDSUM   DS    0H'0'                                                            
         DC    AL2(ENDSUMND-(*+2))   LENGTH OF SCREEN                           
 DC X'00'                                                                       
 DC X'00'                                                                       
 DC C' EVERYONE SEEMS TO HAVE GONE BANKRUPT !!!!'                               
 DC 5X'00'                                                                      
 DC C'IF YOU WANT TO START A NEW GAME JUST PRESS ENTER'                         
ENDSUMND EQU   *                                                                
*                                                                               
SCREENND EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024GAICE02   08/22/00'                                      
         END                                                                    
