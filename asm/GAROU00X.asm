*          DATA SET GAROU00X   AT LEVEL 075 AS OF 05/01/02                      
*PHASE TB1900A                                                                  
*INCLUDE RANDOM                                                                 
         TITLE 'ROULETTE'                                                       
ROULETTE CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ROULWRKX-ROULWRK,GAROU00,RR=R2                                   
         USING ROULWRK,RC          COVER TEMP WORKING STORAGE                   
         L     RA,4(R1)                                                         
         USING TB19FFD,RA          COVER TWA                                    
         ST    R2,RELO                                                          
*                                                                               
         XC    ROUHDR,ROUHDR                                                    
         OI    ROUHDRH+6,X'80'     TURN HEADER ON                               
         CLI   16(RA),0            IS THIS THE FIRST BET?                       
         BNE   CKBANK                                                           
         MVC   CURRBANK,STRTBANK   INITIALIZE BANKROLL                          
         MVI   16(RA),1            TURN ON 'IN-PROGRESS' FLAG                   
*                                                                               
CKBANK   L     R2,CURRBANK                                                      
         LTR   R2,R2                                                            
         BNZ   CKODD                                                            
         MVC   CURRBANK,STRTBANK                                                
         EDIT  (4,CURRBANK),(6,ROUBAMT)                                         
         OI    ROUBAMTH+6,X'80'    TURN ON REFRESHED BANKROLL                   
*                                                                               
CKODD    XC    BINODD,BINODD                                                    
         CLI   ROUODDH+5,0         IF NO ODD WAGER MADE, MOVE ON                
         BE    CKEVEN                                                           
         MVC   ROUHDR,=CL60'INVALID INPUT -- PLEASE REENTER'                    
         BAS   RE,CURSOFF                                                       
         OI    ROUODDH+6,X'40'     PUT CURSOR ON ODD                            
         OI    ROUHDRH+6,X'80'     TURN HEADER ON                               
         TM    ROUODDH+4,X'08'     VALID NUMERICS?                              
         BZ    GOODBYE                                                          
         XC    DOUBLE,DOUBLE                                                    
         ZIC   R1,ROUODDH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DOUBLE,ROUODD(0)                                                 
         SR    R2,R2                                                            
         CVB   R2,DOUBLE                                                        
         ST    R2,BINODD                                                        
*                                                                               
CKEVEN   XC    BINEVEN,BINEVEN                                                  
         CLI   ROUEVENH+5,0        IF NO EVEN WAGER MADE, MOVE ON               
         BE    CKEXACT                                                          
         MVC   ROUHDR,=CL60'INVALID INPUT -- PLEASE REENTER'                    
         BAS   RE,CURSOFF                                                       
         OI    ROUEVENH+6,X'40'    PUT CURSOR ON EVEN                           
         OI    ROUHDRH+6,X'80'     TURN HEADER ON                               
         TM    ROUEVENH+4,X'08'    VALID NUMERICS?                              
         BZ    GOODBYE                                                          
         XC    DOUBLE,DOUBLE                                                    
         ZIC   R1,ROUEVENH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DOUBLE,ROUEVEN(0)                                                
         SR    R2,R2                                                            
         CVB   R2,DOUBLE                                                        
         ST    R2,BINEVEN                                                       
*                                                                               
CKEXACT  XC    BINEXACT,BINEXACT                                                
         CLI   ROUXACTH+5,0        IF NO EXACT WAGER MADE, MOVE ON              
         BE    CKNUM                                                            
         MVC   ROUHDR,=CL60'INVALID INPUT -- PLEASE REENTER'                    
         BAS   RE,CURSOFF                                                       
         OI    ROUXACTH+6,X'40'    PUT CURSOR ON EXACT                          
         OI    ROUHDRH+6,X'80'     TURN HEADER ON                               
         TM    ROUXACTH+4,X'08'    VALID NUMERICS?                              
         BZ    GOODBYE                                                          
         XC    DOUBLE,DOUBLE                                                    
         ZIC   R1,ROUXACTH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DOUBLE,ROUXACT(0)                                                
         SR    R2,R2                                                            
         CVB   R2,DOUBLE                                                        
         ST    R2,BINEXACT                                                      
         B     CKNUM                                                            
*                                                                               
CKNUM    L     R2,BINEXACT                                                      
         LTR   R2,R2                                                            
         BZ    SUBTOTAL                                                         
         MVC   ROUHDR,=CL60'INVALID INPUT -- PLEASE REENTER'                    
         OI    ROUHDRH+6,X'80'     TURN ON HEADER                               
         BAS   RE,CURSOFF                                                       
         OI    ROUNUMH+6,X'40'     PUT CURSOR ON NUM                            
         CLI   ROUNUMH+5,0                                                      
         BE    GOODBYE                                                          
         TM    ROUNUMH+4,X'08'     VALID NUMERICS?                              
         BZ    GOODBYE                                                          
         ZIC   R1,ROUNUMH+5        NUM LENGTH IN R1                             
         BCTR  R1,0                DO THIS FOR EX                               
         XC    DOUBLE,DOUBLE                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DOUBLE,ROUNUM(0)                                                 
         SR    R2,R2                                                            
         CVB   R2,DOUBLE                                                        
         ST    R2,BINNUM                                                        
         MVC   ROUHDR,=CL60'NUMBER SELECTED BY YOU IS OUT OF RANGE -- P*        
               LEASE REENTER'                                                   
         OI    ROUHDRH+6,X'80'     TURN ON HEADER                               
         BAS   RE,CURSOFF                                                       
         OI    ROUNUMH+6,X'40'     PUT CURSOR ON NUM                            
         LTR   R2,R2                                                            
         BM    GOODBYE                                                          
         C     R2,=F'36'                                                        
         BH    GOODBYE                                                          
*                                                                               
SUBTOTAL L     R2,BINODD                                                        
         A     R2,BINEVEN                                                       
         A     R2,BINEXACT                                                      
         MVC   ROUHDR,=CL60'INSUFFICIENT FUNDS -- PLEASE REENTER YOUR W*        
               AGER(S)'                                                         
         OI    ROUHDRH+6,X'80'     TURN ON HEADER                               
         BAS   RE,CURSOFF                                                       
         OI    ROUODDH+6,X'40'     PUT CURSOR ON ODD                            
         C     R2,CURRBANK                                                      
         BH    GOODBYE                                                          
*                                                                               
         GOTO1 =V(RANDOM),DMCB,36,,RR=RELO                                      
         L     R9,4(R1)            R9 HAS RANDOM BINARY NUMBER                  
         ST    R9,RANDBIN                                                       
         LTR   R9,R9                                                            
         BZ    RANDZERO            THE RANDOM NUMBER GENERATED = 0              
*                                  BOTH ODD AND EVEN WAGERS LOSE                
         D     R8,=F'2'                                                         
         LTR   R8,R8                                                            
         BZ    EVENWIN                                                          
*                                                                               
         L     R2,CURRBANK         'ODDWIN'                                     
         A     R2,BINODD                                                        
         S     R2,BINEVEN                                                       
         B     PAYEXACT                                                         
*                                                                               
EVENWIN  L     R2,CURRBANK                                                      
         A     R2,BINEVEN                                                       
         S     R2,BINODD                                                        
         B     PAYEXACT                                                         
*                                                                               
RANDZERO L     R2,CURRBANK                                                      
         S     R2,BINODD                                                        
         S     R2,BINEVEN                                                       
*                                                                               
PAYEXACT L     R9,RANDBIN                                                       
         C     R9,BINNUM                                                        
         BE    EXACTWIN                                                         
*                                                                               
         S     R2,BINEXACT         'EXACTLOSE'                                  
         ST    R2,CURRBANK                                                      
         BAS   RE,OUTCOME                                                       
         B     GOODBYE                                                          
*                                                                               
EXACTWIN L     R1,BINEXACT                                                      
         MH    R1,=H'35'                                                        
         AR    R2,R1                                                            
         ST    R2,CURRBANK                                                      
         BAS   RE,OUTCOME                                                       
         B     GOODBYE                                                          
*                                                                               
CURSOFF  NI    ROUODDH+6,X'BF'                                                  
         NI    ROUEVENH+6,X'BF'                                                 
         NI    ROUXACTH+6,X'BF'                                                 
         NI    ROUNUMH+6,X'BF'                                                  
         BR    RE                                                               
*                                                                               
OUTCOME  ST    RE,OUTRETN                                                       
         EDIT  (4,CURRBANK),(6,ROUBAMT),ZERO=NOBLANK,ALIGN=LEFT                 
         EDIT  (4,RANDBIN),(2,TEMP),ZERO=NOBLANK                                
         OI    ROUBAMTH+6,X'80'    TURN ON BANKROLL                             
         MVC   ROUHDR,=CL60'THE ROULETTE WHEEL STOPPED AT '                     
         MVC   ROUHDR+30(2),TEMP                                                
         OI    ROUHDRH+6,X'80'     TURN ON HEADER                               
         BAS   RE,CURSOFF                                                       
         L     RE,OUTRETN                                                       
         OI    ROUODDH+6,X'40'     PUT CURSOR ON ODD                            
         BR    RE                                                               
*                                                                               
GOODBYE  XIT1                                                                   
         EJECT                                                                  
STRTBANK DC    F'100'                                                           
RELO     DS    F                                                                
         EJECT                                                                  
ROULWRK  DSECT                                                                  
*                                                                               
DMCB     DS    6F                                                               
DUB      DS    D                                                                
DOUBLE   DS    D                                                                
RANDBIN  DS    F                                                                
OUTRETN  DS    F                                                                
BINODD   DS    F                                                                
BINEVEN  DS    F                                                                
BINEXACT DS    F                                                                
BINNUM   DS    F                                                                
WORK     DS    CL17                                                             
TEMP     DS    CL2                                                              
ROULWRKX EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE GAROUFFD                                                       
CURRBANK DS    F                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075GAROU00X  05/01/02'                                      
         END                                                                    
