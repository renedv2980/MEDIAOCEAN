*          DATA SET SPREPFXWSR AT LEVEL 001 AS OF 04/01/97                      
*PHASE SPFX02W                                                                  
********************************************************************            
*        R3 --- ALWAYS POINTS TO KEY                                            
*        R4 --- ALWAYS POINTS TO KEY                                            
********************************************************************            
      TITLE 'SPF02  - CHANGE ALL WESTERN RADIO TO RTG SERVICE CODE 1'           
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
********************************************************************            
RELO     DC    A(0)                                                             
*==================================================================*            
*                START THE FIX                                     *            
*==================================================================*            
FX       DS    0H                                                               
         LA    R3,KEY              R3 ALWAYS POINTS TO KEY                      
         LA    R4,RECORD           R4 ALWAYS POINTS TO THE RECORD               
         ST    R4,AREC                                                          
         XC    COUNT,COUNT                                                      
*                                                                               
         XC    KEY,KEY                                                          
         USING CLTRECD,R4                                                       
         MVC   KEY(2),=X'0012'                                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     FX10                                                             
*                                                                               
FX05     GOTO1 SEQ                                                              
*                                                                               
FX10     DS    0H                                                               
         CLC   KEY(2),KEYSAVE      STILL AN '00X2' RECORD?                      
         BNE   FX100               NO                                           
*                                                                               
         OC    KEY+4(9),KEY+4      CLIENT HEADER RECORD?                        
         BNZ   FX05                NO                                           
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         MVI   CPROF+3,C'1'       NEW RATING SERVICE CODE                       
*                                                                               
         CLI   RCWRITE,C'N'        WRITE CARD IN JCL = NO?                      
         BE    FX20                                                             
         GOTO1 PUT                 CHANGE RECORD                                
*                                                                               
FX20     DS    0H                                                               
         L     R5,COUNT                                                         
         C     R5,=F'5'            PRINT ONLY 5 KEYS OUT                        
         BH    FX90                                                             
*                                  PRINT OUT KEY                                
         GOTO1 HEXOUT,DMCB,(R4),P,13,=C'TOG'                                    
         MVC   P+30(L'CPROF),CPROF    PRINT OUT CPROF                           
         GOTO1 REPORT                                                           
*                                                                               
FX90     AF    COUNT,=F'1'         INCREMENT COUNTER                            
         B     FX05                                                             
*                                                                               
FX100    EDIT  COUNT,(10,P+20),ZERO=NOBLANK,ALIGN=LEFT                          
         MVC   P(18),=C'# OF RECS CHANGED '                                     
         GOTO1 REPORT                                                           
*                                                                               
FXEND    B     EXIT                                                             
         EJECT                                                                  
*==============================================================*                
COUNT    DS    F                                                                
RECORD   DS    2000X                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPFXWSR04/01/97'                                      
         END                                                                    
