*          DATA SET SPREPM7041 AT LEVEL 003 AS OF 11/07/83                      
*PHASE SPM7041,+0,NOAUTO                                                        
         TITLE 'SPREPM7041 - SPOT LENGTH TOTAL LINES'                           
         PRINT NOGEN                                                            
SPM704   CSECT                                                                  
         USING *,11                                                             
         USING *,11,9                                                           
*                                  SPM7041   SEE                                
*                                    ROW   SPM704                               
*                                    NUM     ROW                                
*                                  ------- ------                               
         DC    A(ROWDEF01)           129      4                                 
         DC    A(ROWDEF02)           130      8PART TOTAL                       
         DC    A(ROWDEF03)           131     12PART GROUP TOTAL                 
         DC    A(ROWDEF04)           132     16AL                               
         DC    A(ROWDEF05)           133     20                                 
         DC    A(ROWDEF06)           134     24                                 
         DC    A(ROWDEF07)           135     28                                 
         DC    A(ROWDEF08)           136     32                                 
         DC    A(ROWDEF09)           137     36                                 
         DC    A(ROWDEF10)           138     40                                 
         DC    A(ROWDEF11)           139     44                                 
         DC    A(ROWDEF12)           140     48                                 
         DC    A(ROWDEF13)           141     52 - PRIMARY DEMO                  
         DC    A(ROWDEF14)           142     53                                 
         DC    A(ROWDEF15)           143     54                                 
         DC    A(ROWDEF16)           144     55                                 
         DC    A(ROWDEF17)           145     56 - CLIENT                        
         DC    A(ROWDEF18)           146     57                                 
         DC    A(ROWDEF19)           147     58                                 
         DC    A(ROWDEF20)           148     59                                 
         DC    A(ROWDEF21)           149     60 - PRIMARY DEMO                  
         DC    A(ROWDEF22)           150     61                                 
         DC    A(ROWDEF23)           151     62                                 
         DC    A(ROWDEF24)           152     63                                 
         DC    A(ROWDEF25)           153     64 - CLIENT                        
         DC    A(ROWDEF26)           154     65                                 
         DC    A(ROWDEF27)           155     66                                 
         DC    A(ROWDEF28)           156     67                                 
         DC    A(ROWDEF29)           157     68ONDARY DEMO SUMMARIES            
         DC    A(ROWDEF30)           158     69                                 
         DC    A(ROWDEF31)           159     70                                 
         DC    A(ROWDEF32)           160     71                                 
         DC    A(ROWDEF33)           161     72                                 
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF01 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+2,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'81'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
         SPACE 2                                                                
ROWDEF02 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'82'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF03 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'83'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF04 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'84'                                                            
         DC    X'FF'                                                            
ROWDEF05 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'85'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF06 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'00'                                                            
         MEXT  X'86'                                                            
         DC    X'FF'                                                            
ROWDEF07 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF3)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FF'                                                            
         MEXT  X'00'                                                            
         MEXT  X'87'                                                            
         DC    X'FF'                                                            
ROWDEF08 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'00'                                                            
         MEXT  X'88'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF09 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'00'                                                            
         MEXT  X'89'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF10 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'00'                                                            
         MEXT  X'8A'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF11 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'00'                                                            
         MEXT  X'8B'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF12 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'00'                                                            
         MEXT  X'8C'               RECORD NUMBER                                
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF13 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'00'                                                            
         MEXT  X'8D'               RECORD NUMBER                                
         DC    X'FF'                                                            
ROWDEF14 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+2,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF'                                                            
         MEXT  X'8E'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF15 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF'                                                            
         MEXT  X'8F'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF16 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF'                                                            
         MEXT  X'90'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF17 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FF'                                                            
         MEXT  X'91'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF18 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FF'                                                            
         MEXT  X'92'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF19 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FF'                                                            
         MEXT  X'93'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF20 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FF'                                                            
         MEXT  X'94'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF21 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FF'                                                            
         MEXT  X'95'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF22 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FF'                                                            
         MEXT  X'96'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF23 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FF'                                                            
         MEXT  X'97'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
ROWDEF24 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF2)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  WORK+2,3                                                         
         MEXT  BRAND                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FE'                                                            
         MEXT  X'98'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF25 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'01'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FE'                                                            
         MEXT  X'99'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF26 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  WORK,2                                                           
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FE'                                                            
         MEXT  X'9A'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF27 DC    A(PRNTDEF1)                                                      
         DC    A(COLDEF5)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1                                                           
         MEXT  X'02'                                                            
         MEXT  X'FFFF'                                                          
         MEXT  PRIMARY                                                          
         MEXT  X'00'                                                            
         MEXT  X'FFFFFF'                                                        
         MEXT  X'FF'                                                            
         MEXT  X'FFFFFFFF'                                                      
         MEXT  X'FFFFFFFF'                                                      
         MEXT  SPOT-LENGTH                                                      
         MEXT  X'FE'                                                            
         MEXT  X'9B'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF28 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FE'                                                            
         MEXT  X'9C'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF29 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'01'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FF'                                                            
         MEXT  X'9C'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF30 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FE'                                                            
         MEXT  X'9E'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF31 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  WORK,2              AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FE'                                                            
         MEXT  X'9F'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF32 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF6)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'01'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FE'                                                            
         MEXT  X'A0'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
ROWDEF33 DC    A(PRNTDEF2)                                                      
         DC    A(COLDEF7)                                                       
         DC    AL1(1)                                                           
         DC    7X'00'                                                           
         MEXT  HALF,1              RECORD CODE                                  
         MEXT  X'02'                                                            
         MEXT  X'FFFF'             AGENCY CODE                                  
         MEXT  PRIMARY             PRIMARY DEMO                                 
         MEXT  X'02'               SUMMARY CODE                                 
         MEXT  X'FFFFFF'           ALPHA PROD                                   
         MEXT  X'FF'               NUMERIC PROD                                 
         MEXT  X'FFFFFFFF'         SUB DAYPART                                  
         MEXT  X'FFFFFFFF'         DAYPART                                      
         MEXT  SPOT-LENGTH         SPOT LENGTH                                  
         MEXT  X'FE'                                                            
         MEXT  X'A1'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
*              COLUMN DEFINITIONS                                               
         SPACE 3                                                                
COLDEF1  MEXT  GOAL-DEM1,PERIOD    1                                            
         MEXT  GOAL-DEM1EQV,PERIOD 2                                            
         MEXT  GOAL-$,PERIOD       3                                            
         MEXT  GOAL-$EQV,PERIOD    4                                            
         MEXT  LOCKIN-DEM1,PERIOD  5                                            
         MEXT  LOCKIN-DEM1EQV,PERIOD                                            
         MEXT  LOCKIN-$,PERIOD     7                                            
         MEXT  LOCKIN-$EQV,PERIOD  8                                            
         MEXT  LOCKIN-SPOTS,PERIOD 9                                            
         MEXT  BUY-DEM1,PERIOD     10                                           
         MEXT  BUY-DEM1EQV,PERIOD  11                                           
         MEXT  BUY-$,PERIOD        12                                           
         MEXT  BUY-$EQV,PERIOD     13                                           
         MEXT  BUY-SPOTS,PERIOD    14                                           
         MEXT  BUY-DEM2,PERIOD     15                                           
         MEXT  BUY-DEM2EQV,PERIOD  16                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF2  MEXT  GOAL-DEM1,PERIOD    1         (NO EQUIVS)                        
         MEXT  ZERO                2                                            
         MEXT  GOAL-$,PERIOD       3                                            
         MEXT  ZERO                4                                            
         MEXT  LOCKIN-DEM1,PERIOD  5                                            
         MEXT  ZERO                6                                            
         MEXT  LOCKIN-$,PERIOD     7                                            
         MEXT  ZERO                8                                            
         MEXT  LOCKIN-SPOTS,PERIOD 9                                            
         MEXT  BUY-DEM1,PERIOD     10                                           
         MEXT  ZERO                11                                           
         MEXT  BUY-$,PERIOD        12                                           
         MEXT  ZERO                13                                           
         MEXT  BUY-SPOTS,PERIOD    14                                           
         MEXT  BUY-DEM2,PERIOD     15                                           
         MEXT  ZERO                16                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF3  MEXT  ZERO                1         (DOLLARS AND SPOTS ONLY)           
         MEXT  ZERO                2                                            
         MEXT  GOAL-$,PERIOD       3                                            
         MEXT  ZERO                4                                            
         MEXT  ZERO                5                                            
         MEXT  ZERO                6                                            
         MEXT  LOCKIN-$,PERIOD     7                                            
         MEXT  ZERO                8                                            
         MEXT  LOCKIN-SPOTS,PERIOD 9                                            
         MEXT  ZERO                10                                           
         MEXT  ZERO                11                                           
         MEXT  BUY-$,PERIOD        12                                           
         MEXT  ZERO                13                                           
         MEXT  BUY-SPOTS,PERIOD    14                                           
         DC    X'FF'                                                            
         EJECT                                                                  
COLDEF4  MEXT  GOAL-DEM1,PERIOD                                                 
         MEXT  GOAL-DEM1EQV,PERIOD                                              
         MEXT  GOAL-$,PERIOD                                                    
         MEXT  GOAL-$EQV,PERIOD                                                 
         MEXT  LOCKIN-DEM1,PERIOD                                               
         MEXT  LOCKIN-DEM1EQV,PERIOD                                            
         MEXT  LOCKIN-$,PERIOD                                                  
         MEXT  LOCKIN-$EQV,PERIOD                                               
         MEXT  LOCKIN-SPOTS,PERIOD                                              
         MEXT  BUY-DEM1,PERIOD                                                  
         MEXT  BUY-DEM1EQV,PERIOD                                               
         MEXT  BUY-$,PERIOD                                                     
         MEXT  BUY-$EQV,PERIOD                                                  
         MEXT  BUY-SPOTS,PERIOD                                                 
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF5  MEXT  GOAL-DEM1,PERIOD                                                 
         MEXT  ZERO                                                             
         MEXT  GOAL-$,PERIOD                                                    
         MEXT  ZERO                                                             
         MEXT  LOCKIN-DEM1,PERIOD                                               
         MEXT  ZERO                                                             
         MEXT  LOCKIN-$,PERIOD                                                  
         MEXT  ZERO                                                             
         MEXT  LOCKIN-SPOTS,PERIOD                                              
         MEXT  BUY-DEM1,PERIOD                                                  
         MEXT  ZERO                                                             
         MEXT  BUY-$,PERIOD                                                     
         MEXT  ZERO                                                             
         MEXT  BUY-SPOTS,PERIOD                                                 
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF6  MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM1,TOTAL                                                   
         MEXT  BUY-DEM1EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM2,TOTAL                                                   
         MEXT  BUY-DEM2EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM3,TOTAL                                                   
         MEXT  BUY-DEM3EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM4,TOTAL                                                   
         MEXT  BUY-DEM4EQV,TOTAL                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
COLDEF7  MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM5,TOTAL                                                   
         MEXT  BUY-DEM5EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM6,TOTAL                                                   
         MEXT  BUY-DEM6EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM7,TOTAL                                                   
         MEXT  BUY-DEM7EQV,TOTAL                                                
         MEXT  BUY-$,TOTAL                                                      
         MEXT  BUY-$EQV,TOTAL                                                   
         MEXT  BUY-DEM8,TOTAL                                                   
         MEXT  BUY-DEM8EQV,TOTAL                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
*              PRINT DEFINITION TABLES                                          
         SPACE 3                                                                
PRNTDEF1 MEDIT DAYPART,1,KEY,11                                                 
         MEDIT CPM,12,COL,1,3                                                   
         MEDIT CPM,39,COL,5,7                                                   
         MEDIT NUMBER,59,COL,9                                                  
         MEDIT CPM,133,COL,10,12                                                
         MEDIT IMPS,118,COL,12,15                                               
         MEDIT NUMBER,102,COL,14                                                
         MEDIT PERCENT,113,COL,14,9                                             
         MEDIT PERCENT,157,COL,10,1                                             
         MEDIT PERCENT,161,COL,12,3                                             
         DC    X'FF'                                                            
PRNTDEF2 MEDIT DAYPART,1,KEY,11                                                 
         MEDIT CPM,11,COL,3,1                                                   
         MEDIT CPM,36,COL,7,5                                                   
         MEDIT CPM,61,COL,11,9                                                  
         MEDIT CPM,86,COL,15,13                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPGENFILE                                                      
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPM704111/07/83'                                      
         END                                                                    
