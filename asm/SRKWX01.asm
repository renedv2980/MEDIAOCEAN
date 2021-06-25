*          DATA SET SRKWX01    AT LEVEL 002 AS OF 08/22/00                      
*PHASE T14701A                                                                  
         TITLE '$KWX - HELP=YES MESSAGE'                                        
T14701   CSECT                                                                  
HELPMESS DS    0CL78                                                            
         DC    CL39'AUTHORIZATION - Input a code that is va'                    
         DC    CL39'lid for this terminal and user.        '                    
         DC    CL39'SEND TO       - Input one or more addre'                    
         DC    CL39'ssee codes and/or addressee list codes,'                    
         DC    CL39'                seperated by ''+'' or ''-'''                
         DC    CL39', eg. ''LISTA+LISTB-NAMX-NAMY+LISTC''. '                    
         DC    CL39'THE MESSAGE   - Input up to 19 message '                    
         DC    CL39'lines and position the cursor past the '                    
         DC    CL39'                end of the message befo'                    
         DC    CL39're pressing ''ENTER''.'                                     
         DC    CL39'The keywords below cause special action'                    
         DC    CL39's if input in Col.1 of any message line'                    
         DC    CL39'1 CHECK=YES/RHS will, if input on the l'                    
         DC    CL39'ine after a ''REPORT='' string, cause the'                  
         DC    CL39'                first few requested rep'                    
         DC    CL39'ort lines to be displayed for checking.'                    
         DC    CL39'2 KWX=KKK,9999                         '                    
         DC    CL39'                                       '                    
         DC    CL39'     KKK,9999   is the id of a sender''s'                   
         DC    CL39' copy of a kwx on the user''s print    '                    
         DC    CL39'                queue. The effect is to'                    
         DC    CL39' re-display the KWX so that it can be  '                    
         DC    CL39'                sent again, after modif'                    
         DC    CL39'ication if desired.                    '                    
         DC    CL39'3 REPORT=RRR,9999(,PAGE=9(-9),LINE=9(-9'                    
         DC    CL39'),FORCE,DISPLAY,NEWPAGE,SENDER)        '                    
         DC    CL39'     RRR,9999   is the id of a report o'                    
         DC    CL39'n the user''s print queue.             '                    
         DC    CL39'     PAGE=9(-9) specifies the single pa'                    
         DC    CL39'ge or first and last pages required.   '                    
         DC    CL39'     LINE=9(-9) specifies the single li'                    
         DC    CL39'ne or first and last lines required.   '                    
         DC    CL39' '                                                          
         DC    CL39' '                                                          
         DC    CL39'(Enter ''SHOWME2'' in the authorization f'                  
         DC    CL39'ield to obtain the next help screen)'                       
*                                                                               
*              PAGE 2                                                           
*                                                                               
         DC    CL39'(3 ''REPORT=...'' continued)'                               
         DC    CL39' '                                                          
         DC    CL39'     FORCE      will force printing req'                    
         DC    CL39'ardless of report size.                '                    
         DC    CL39'     DISPLAY    causes the specified re'                    
         DC    CL39'port subset to be displayed in full.   '                    
         DC    CL39'     NEWPAGE    will cause a throw to n'                    
         DC    CL39'ew page before and after the report.   '                    
         DC    CL39'     SENDER     will cause the report t'                    
         DC    CL39'o be appended to the sender''s copy.    '                   
         DC    CL39'4 SPACE=2 OR 3  Will cause double  or t'                    
         DC    CL39'riple line spacing of the message.     '                    
         DC    CL39'5 STATUS=OK     will cause the precedin'                    
         DC    CL39'g message, containing ''TEXT='', to be '                    
         DC    CL39'                sent without first bein'                    
         DC    CL39'g displayed in expanded form.          '                    
         DC    CL39'6 TEXT=TTTTTTTTTT(,A=B,C=D ETC.,NOSHIFT'                    
         DC    CL39')                                      '                    
         DC    CL39'     TTTTTTTTTT is the id of some libra'                    
         DC    CL39'ry text belonging to the user.         '                    
         DC    CL39'     A=B,C=D    Are substitution reques'                    
         DC    CL39'ts (replace all instances of A with B).'                    
         DC    CL39'     NOSHIFT    indicates that substitu'                    
         DC    CL39'tion is to be carried out without      '                    
         DC    CL39'                the remainder of a line'                    
         DC    CL39' being shifted to left or right.'                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRKWX01   08/22/00'                                      
         END                                                                    