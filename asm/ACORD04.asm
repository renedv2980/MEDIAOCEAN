*          DATA SET ACORD04    AT LEVEL 011 AS OF 10/17/18                      
*PHASE T60F04A                                                                  
         TITLE 'ACORD04 - PRODUCTION ORDERS - HELP'                             
ACRDS04  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ORD4**                                                       
         USING ORDWORKD,R9                                                      
         USING TWAD,R8                                                          
*                                                                               
CLEAR    LA    R3,APOFRSTH         CLEAR NARRATIVE LINES                        
         SR    R4,R4                                                            
         LA    R5,APOFINLH                                                      
CLEAR2   IC    R4,0(R3)                                                         
         SH    R4,=H'9'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)                                                    
         OI    6(R3),X'80'                                                      
CLEAR3   IC    R4,0(R3)                                                         
         BXLE  R3,R4,*+8                                                        
         B     DISPLAY                                                          
         TM    1(R3),X'20'                                                      
         BNO   CLEAR2                                                           
         B     CLEAR3                                                           
         EJECT                                                                  
* DISPLAY ACTIONS FROM ACTION TABLE PLUS SPECIAL LINES FOR EDIT                 
*                                                                               
DISPLAY  LA    R3,APOFRSTH         SET UP INTRO                                 
         MVC   8(L'INTRO,R3),INTRO                                              
         IC    R4,0(R3)                                                         
         AR    R3,R4                                                            
         TM    1(R3),X'20'                                                      
         BO    *-10                                                             
         L     R6,AACTNTAB                                                      
         LA    R6,ACTDLEN(R6)      POINT TO 2ND ACTION TAB ENTRY                
         USING ACTD,R6                                                          
         MVI   FLAG1,0             SWITCH FOR 2-UP DISPLAY                      
*                                                                               
DISP2    CLI   0(R6),X'FF'         LOOP FOR AN ACTION                           
         BE    DISP10                                                           
         CLC   ACTDSHT,=C'ED'      SKIP EDIT                                    
         BE    DISP5                                                            
         LA    R2,8(R3)                                                         
         CLI   FLAG1,0                                                          
         BE    *+8                                                              
         LA    R2,42(R3)                                                        
         MVC   0(2,R2),ACTDSHT                                                  
         MVC   3(L'ACTDNAME,R2),ACTDNAME                                        
         CLI   FLAG1,0                                                          
         BE    DISP4                                                            
         IC    R4,0(R3)                                                         
         AR    R3,R4                                                            
         TM    1(R3),X'20'                                                      
         BO    *-10                                                             
DISP4    XI    FLAG1,1             SWITCH                                       
DISP5    LA    R6,ACTDLEN(R6)                                                   
         B     DISP2                                                            
*                                                                               
DISP10   LA    R1,EDITHELP                                                      
         LA    R0,3                                                             
         CLI   FLAG1,0                                                          
         BE    DISP15                                                           
DISP12   IC    R4,0(R3)                                                         
         AR    R3,R4                                                            
         TM    1(R3),X'20'                                                      
         BO    *-10                                                             
DISP15   MVC   8(L'EDITHELP,R3),0(R1)                                           
         LA    R1,L'EDITHELP(R1)                                                
         BCT   R0,DISP12                                                        
         B     OKEND                                                            
*                                                                               
*                                                                               
INTRO    DS    0CL55                                                            
         DC    C'ACTIONS AVAILABLE, WITH 2-CHARACTER ABBREVIATIONS, AR'         
         DC    C'E -'                                                           
*                                                                               
EDITHELP DS    0CL60                                                            
         DC    C'ED EDIT,INS,N(-N)               - FOR LINE INSERTION '         
         DC    C'       '                                                       
         DC    C'ED EDIT,DEL,N(-N)               - FOR LINE DELETION  '         
         DC    C'       '                                                       
         DC    C'ED EDIT,REP,(N-N,)OLD=X,NEW=Y   - FOR CHARACTER SUBST'         
         DC    C'ITUTION'                                                       
         EJECT                                                                  
* EXIT BACK TO ROOT                                                             
*                                                                               
OKEND    MVC   FERN,HELPFERN       RESTORE FERN                                 
         XC    ORDNUMB,ORDNUMB                                                  
         XC    LORDNUM,LORDNUM     ACCEPT DELETES                               
         CLI   FERN,OK                                                          
         BNE   *+10                                                             
         MVC   MSG(29),=C'ACTION COMPLETED - ENTER NEXT'                        
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
* ACORDDSECT                                                                    
       ++INCLUDE ACORDDSECT                                                     
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACORD04   10/17/18'                                      
         END                                                                    
