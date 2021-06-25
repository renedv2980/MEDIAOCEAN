*          DATA SET TYB        AT LEVEL 010 AS OF 04/01/85                      
*PHASE TYB,+0                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
         MACRO                                                                  
&NAME    CLEAN &I,&L,&C                                                         
         LCLA  &L1                                                              
         LCLC  &L2                                                              
&L1      SETA  &L-1                                                             
         MVI   &I,&C                                                            
         AIF   (T'&I EQ 'U').SS1                                                
         MVC   &I+1(&L1),&I                                                     
         AGO   .SS                                                              
.SS1     ANOP                                                                   
         AIF   ('&I'(K'&I-2,1) NE 'R').SS                                       
&L2      SETC  '1+'.'&I'(1,K'&I-3).'&L1,'.'&I'(K'&I-2,3)                        
         MVC   &L2,&I                                                           
.SS      MEND                                                                   
*                                                                               
         EJECT                                                                  
         PRINT NOGEN                                                            
TYB      NMOD1 0,**TYBB**                                                       
         LM    R3,R5,0(R1)                                                      
         N     R3,=X'0FFFFFFF'                                                  
         ABIN  D,0(R3),4,KEEP=Y,RD=N                                            
         C     R0,=F'0'                                                         
         BNE   *+8                                                              
         O     R3,=X'80000000'                                                  
         LTR   R5,R5                                                            
         BZ    TYB1                                                             
         L     R5,0(R5)                                                         
         ABIN  A,0(R3),(R5),KEEP=Y                                              
         B     TYB2                                                             
TYB1     ABIN  A,1900,0(R3)                                                     
TYB2     CLC   0(4,R3),=F'50'                                                   
         BH    *+8                                                              
         AH    R1,=H'100'                                                       
         LR    R6,R1                                                            
*  ALGORITHM                                                                    
         ABIN  S,(R6),1900                                                      
         ABIN  D,(R6),4,RD=N,OT=(R2)                                            
         CL    R3,=X'80000000'                                                  
         BL    *+6                                                              
         BCTR  R2,0                                                             
         ABIN  A,(R6),(R2)                                                      
         ABIN  D,(R6),7,RD=N,KEEP=Y                                             
         LR    R8,R0                                                            
         LA    R8,1(R8)                                                         
*  YEAR TABLE                                                                   
         SR    RC,RC                                                            
         CLEAN 0(R4),48,X'00'                                                   
         LR    R9,R4                                                            
         LA    R6,12                                                            
         LA    R2,=X'1F1C1F1E1F1E1F1F1E1F1E1F'                                  
TBB      ABIN  A,(12),1                                                         
         STC   RC,0(R9)                                                         
         MVC   1(1,R9),0(R2)                                                    
         CH    RC,=H'2'                                                         
         BNE   TBB1                                                             
         CL    R3,=X'80000000'                                                  
         BL    TBB1                                                             
         ABIN  A,(1,1(R9)),1                                                    
TBB1     STC   R8,2(R9)                                                         
         ABIN  A,(8),(1,1(R9))                                                  
         ABIN  S,(8),2                                                          
         ABIN  D,(8),7,RD=N,KEEP=Y                                              
         LR    R8,R0                                                            
         ABIN  A,(8),1,OT=(1,3(R9))                                             
         CLI   3(R9),7                                                          
         BL    *+12                                                             
         LA    R8,1                                                             
         B     TBB2                                                             
         ABIN  A,(1,3(R9)),1,OT=(8)                                             
TBB2     ABIN  A,(9),4                                                          
         ABIN  A,(2),1                                                          
         BCT   R6,TBB                                                           
         XIT1                                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TYB       04/01/85'                                      
         END                                                                    
         LTORG                                                                  
