*          DATA SET GAROU00    AT LEVEL 038 AS OF 08/13/93                      
*PHASE MSELROU0                                                                 
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
         OI    ROUHDRH+6,X'80'                                                  
         CLI   16(RA),0            IS THIS THE FIRST BET?                       
         BNE   CKINPUT1                                                         
         MVC   CURRBANK,STRTBANK   INITIALIZE BANKROLL                          
         MVI   16(RA),1            TURN ON 'IN-PROGRESS' FLAG                   
*                                                                               
CKINPUT1 CLI   17(RA),0                                                         
         BNE   VALEXACT                                                         
         CLI   ROUTYPH+5,0         NO TYPE INPUT?                               
         BE    GOODBYE                                                          
         TM    ROUTYPH+4,X'08'     VALID NUMERICS?                              
         BZ    GOODBYE                                                          
         CLI   ROUTYP,C'0'         TYPE TOO LOW?                                
         BNH   GOODBYE                                                          
         CLI   ROUTYP,C'4'         TYPE TOO HIGH?                               
         BNL   GOODBYE                                                          
*                                                                               
         CLI   ROUWAMTH+5,0        NO WAGER INPUT?                              
         BE    GOODBYE                                                          
         TM    ROUWAMTH+4,X'08'    VALID NUMERICS?                              
         BZ    GOODBYE                                                          
         ZIC   R1,ROUWAMTH+5       WAGER LENGTH IN R1                           
         BCTR  R1,0                DECREMENT R1 FOR EX                          
         XC    DOUBLE,DOUBLE                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DOUBLE,ROUWAMT(0)                                                
         CVB   R2,DOUBLE           ROUWAMT NOW IN R2                            
         ST    R2,CURRWAGE                                                      
         C     R2,CURRBANK                                                      
         BH    GOODBYE             WAGER > BANKROLL -- EXIT                     
*                                                                               
         CLI   ROUTYP,C'3'                                                      
         BE    EXACT                                                            
         CLI   ROUTYP,C'2'                                                      
         BE    EVEN                                                             
*                                                                               
         GOTO1 =V(RANDOM),DMCB,36,,RR=RELO                                      
*                                  ROUTYP = C'1'                                
         L     R9,4(R1)            R9 HAS RANDOM BINARY NUMBER                  
         LTR   R9,R9                                                            
         ST    R9,RANDBIN                                                       
         BZ    LOSE                                                             
         D     R8,=F'2'                                                         
         LTR   R8,R8               R8 HAS REMAINDER                             
         BNZ   WIN                                                              
         B     LOSE                                                             
*                                                                               
EVEN     GOTO1 =V(RANDOM),DMCB,36,,RR=RELO                                      
         L     R9,4(R1)            R9 HAS RANDOM BINARY NUMBER                  
         LTR   R9,R9                                                            
         ST    R9,RANDBIN                                                       
         BZ    LOSE                                                             
         D     R8,=F'2'                                                         
         LTR   R8,R8               R8 HAS REMAINDER                             
         BZ    WIN                                                              
         B     LOSE                                                             
*                                                                               
WIN      L     R2,CURRWAGE         1:1 PAYOFF--ADD WAGER TO BANKROLL            
         A     R2,CURRBANK                                                      
         ST    R2,CURRBANK                                                      
         EDIT  (4,CURRBANK),(4,ROUBAMT)                                         
         OI    ROUBAMTH+6,X'80'    TURN ON XMIT BIT                             
         EDIT  (4,RANDBIN),(2,TEMP)                                             
         MVC   ROUHDR+30(2),TEMP                                                
         MVC   ROUHDR+33(8),=C'YOU WIN!'                                        
         MVC   ROUHDR(30),=C'THE ROULETTE WHEEL STOPPED AT:'                    
         OI    ROUHDRH+6,X'80'     TURN ON XMIT BIT                             
         OI    ROUTYPH+6,X'40'     REPOSITION CURSOR                            
         B     GOODBYE                                                          
*                                                                               
LOSE     L     R2,CURRBANK                                                      
         S     R2,CURRWAGE                                                      
         ST    R2,CURRBANK                                                      
         EDIT  (4,CURRBANK),(4,ROUBAMT)                                         
         OI    ROUBAMTH+6,X'80'    TURN ON XMIT BIT                             
         EDIT  (4,RANDBIN),(2,TEMP)                                             
         MVC   ROUHDR+30(2),TEMP                                                
         MVC   ROUHDR+33(9),=C'YOU LOSE!'                                       
         MVC   ROUHDR(30),=C'THE ROULETTE WHEEL STOPPED AT:'                    
         OI    ROUHDRH+6,X'80'     TURN ON XMIT BIT                             
         OI    ROUTYPH+6,X'40'     REPOSITION CURSOR                            
         B     GOODBYE                                                          
*                                                                               
EXACT    NI    ROUSELNH+1,X'F0'    MAKE ROUSELN VISIBLE                         
         NI    ROUNUMBH+1,X'F0'    MAKE ROUNUMB VISIBLE                         
         OI    ROUNUMBH+6,X'40'    REPOSITION CURSOR                            
         OI    ROUSELNH+6,X'80'    TURN ON XMIT BIT                             
         OI    ROUNUMBH+6,X'80'    TURN ON XMIT BIT                             
         MVI   17(RA),1            TURN ON FLAG TO INDICATE                     
*                                  THAT WE HAVE BEEN HERE BEFORE                
         B     GOODBYE                                                          
*                                                                               
VALEXACT CLI   ROUNUMBH+5,0        NO NUMBER PROVIDED?                          
         BE    GOODBYE                                                          
         TM    ROUNUMBH+4,X'08'    VALID NUMERICS?                              
         BZ    GOODBYE                                                          
         ZIC   R1,ROUNUMBH+5       NUMBER LENGTH IN R1                          
         BCTR  R1,0                DECREMENT R1 FOR EX                          
         XC    DOUBLE,DOUBLE                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DOUBLE,ROUNUMB(0)                                                
         CVB   R3,DOUBLE           ROUNUMB NOW IN R3                            
         LTR   R3,R3                                                            
         BM    GOODBYE                                                          
         C     R3,=F'36'                                                        
         BNL   GOODBYE                                                          
*                                                                               
         GOTO1 =V(RANDOM),DMCB,36,,RR=RELO                                      
         L     R9,4(R1)                                                         
         ST    R9,RANDBIN                                                       
         C     R3,RANDBIN          DID USER-SELECTED NUMBER WIN?                
         BNE   XACTLOSE                                                         
*                                                                               
         L     R2,CURRWAGE                                                      
         MH    R2,=H'35'           35:1 PAYOFF                                  
         A     R2,CURRBANK                                                      
         ST    R2,CURRBANK                                                      
         EDIT  (4,CURRBANK),(4,ROUBAMT)                                         
         OI    ROUBAMTH+6,X'80'    TURN ON XMIT BIT                             
         EDIT  (4,RANDBIN),(2,TEMP)                                             
         MVC   ROUHDR+30(2),TEMP                                                
         MVC   ROUHDR+33(8),=C'YOU WIN!'                                        
         MVC   ROUHDR(30),=C'THE ROULETTE WHEEL STOPPED AT:'                    
         OI    ROUHDRH+6,X'80'     TURN ON XMIT BIT                             
         OI    ROUTYPH+6,X'40'     REPOSITION CURSOR                            
         NI    ROUSELNH+1,X'F0'    MAKE ROUSELN INVISIBLE                       
         OI    ROUSELNH+1,X'0C'                                                 
         NI    ROUNUMBH+1,X'F0'    MAKE ROUNUMB INVISIBLE                       
         OI    ROUNUMBH+1,X'0C'                                                 
         OI    ROUSELNH+6,X'80'    TURN ON XMIT BIT                             
         OI    ROUNUMBH+6,X'80'                                                 
         MVI   17(RA),0            TURN OFF FLAG SIGNIFYING                     
*                                  THAT WE HAVE VISITED EXACT                   
         B     GOODBYE                                                          
*                                                                               
XACTLOSE L     R2,CURRBANK                                                      
         S     R2,CURRWAGE                                                      
         ST    R2,CURRBANK                                                      
         EDIT  (4,CURRBANK),(4,ROUBAMT)                                         
         OI    ROUBAMTH+6,X'80'    TURN ON XMIT BIT                             
         EDIT  (4,RANDBIN),(2,TEMP)                                             
         MVC   ROUHDR+30(2),TEMP                                                
         MVC   ROUHDR+33(9),=C'YOU LOSE!'                                       
         MVC   ROUHDR(30),=C'THE ROULETTE WHEEL STOPPED AT:'                    
         OI    ROUHDRH+6,X'80'     TURN ON XMIT BIT                             
         OI    ROUTYPH+6,X'40'     REPOSITION CURSOR                            
         NI    ROUSELNH+1,X'F0'    MAKE ROUSELN INVISIBLE                       
         OI    ROUSELNH+1,X'0C'                                                 
         NI    ROUNUMBH+1,X'F0'    MAKE ROUNUMB INVISIBLE                       
         OI    ROUNUMBH+1,X'0C'                                                 
         OI    ROUSELNH+6,X'80'    TURN ON XMIT BIT                             
         OI    ROUNUMBH+6,X'80'                                                 
         MVI   17(RA),0            TURN OFF FLAG SIGNIFYING                     
*                                  THAT WE HAVE VISITED EXACT                   
GOODBYE  OI    ROUTYPH+6,X'40'     REPOSITION CURSOR                            
         XC    ROUTYP,ROUTYP                                                    
         XC    ROUWAMT,ROUWAMT                                                  
         XC    ROUNUMB,ROUNUMB                                                  
         OI    ROUTYPH+6,X'80'                                                  
         OI    ROUWAMTH+6,X'80'                                                 
         OI    ROUNUMBH+6,X'80'                                                 
         XIT1                                                                   
         EJECT                                                                  
STRTBANK DC    F'10'                                                            
RELO     DS    F                                                                
         EJECT                                                                  
ROULWRK  DSECT                                                                  
*                                                                               
DMCB     DS    6F                                                               
DUB      DS    D                                                                
DOUBLE   DS    D                                                                
RANDBIN  DS    F                                                                
WORK     DS    CL17                                                             
TEMP     DS    CL2                                                              
ROULWRKX EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE GAROUFFD                                                       
CURRWAGE DS    F                                                                
CURRBANK DS    F                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038GAROU00   08/13/93'                                      
         END                                                                    
