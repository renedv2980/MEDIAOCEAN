*          DATA SET ACREPXM02  AT LEVEL 031 AS OF 05/01/02                      
*PHASE ACXM02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'ADD STATUS ELEMENTS'                                            
ACXM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXM**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACX9D,RC                                                         
*-------------------------------------------------------------------*           
*        FIRST FOR RUN                                                          
*-------------------------------------------------------------------*           
         CLI   MODE,RUNFRST                                                     
         BNE   PRAC00                                                           
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'                                                 
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCACC                                                                
*-------------------------------------------------------------------*           
PRAC00   CLI   MODE,PROCACC                                                     
         BNE   XIT                                                              
         USING ACTRECD,R2                                                       
         L     R2,ADACC                                                         
         USING RSTELD,R4                                                        
         LR    R4,R2                                                            
         MVI   ELCODE,RSTELQ                                                    
         BAS   RE,GETEL                                                         
         BE    XIT                                                              
                                                                                
         BAS   RE,DMPGET                                                        
                                                                                
         XC    ELEMENT,ELEMENT        BUILD NEW STATUS ELEMENT AND              
         USING RSTELD,R4              ADD TO RECORD                             
         LA    R4,ELEMENT                                                       
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         MVI   RSTFILT4,C' '                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,RSTBDATE)                                   
         MVC   RSTTDATE,RSTBDATE                                                
         MVI   RSTFILT3,C' '                                                    
         MVI   RSTFILT1,C' '                                                    
         MVI   RSTFILT2,C' '                                                    
         MVI   RSTCOSTG,C' '                                                    
         MVI   RSTFILT5,C' '                                                    
         GOTO1 ADDEL,DMCB,(R2),(R4)                                             
                                                                                
         BAS   RE,DMPPUT                                                        
                                                                                
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         MVI   MODE,WRITACC                                                     
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ROUTINES TO DUMP OUT RECORDS                                           
*-------------------------------------------------------------------*           
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT'                                                       
*                                                                               
DUMP     CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         SR    R8,R8                                                            
         L     R2,ADACC                                                         
         ICM   R8,3,ACTRLEN                                                     
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R2),C'DUMP',(R8),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              ROUTINE TO GET AN ELEMENT                                        
*                                                                               
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
*-------------------------------------------------------------------*           
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',ACCOUNT),((R4),(R2)),((R5),(R3))                
         B     XIT                                                              
*-------------------------------------------------------------------*           
*              ROUTINE TO ADD AN ELEMENT                                        
*                                                                               
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
*-------------------------------------------------------------------*           
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCOUNT),(R2),(R3)                              
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         B     XIT                                                              
         SPACE 1                                                                
         GETEL R4,DATADISP,ELCODE                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
         SPACE 1                                                                
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         DC    X'FF'                                                            
                                                                                
ACCOUNT  DC    CL8'ACCOUNT'                                                     
                                                                                
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'5000'                                                        
         EJECT                                                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
ACX9D    DSECT                                                                  
TODAY2   DS    CL2                                                              
ELCODE   DS    CL1                                                              
ELEMENT  DS    CL255                                                            
*                                                                               
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACREPXM02 05/01/02'                                      
         END                                                                    
