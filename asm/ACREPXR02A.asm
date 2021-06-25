*          DATA SET ACREPXR02A AT LEVEL 025 AS OF 08/16/00                      
*PHASE ACXR02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'ADD ORIGINATING SERVER TO PRESTO ORDERS'                        
ACXR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXR**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXRD,RC                                                         
         ST    R5,RELO                                                          
*-------------------------------------------------------------------*           
*        FIRST FOR RUN                                                          
*-------------------------------------------------------------------*           
         CLI   MODE,RUNFRST                                                     
         BNE   PROCBUD                                                          
*                                                                               
         L     R7,AMONACC                                                       
         USING ACMD,R7                                                          
         MVC   HELLO,ACMVHELO                                                   
         L     RE,=V(PRNTBL)                                                    
         A     RE,RELO                                                          
         ST    RE,PRNTBL                                                        
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCESS BUDGET RECORDS                                                 
*-------------------------------------------------------------------*           
*                                                                               
PROCBUD  CLI   MODE,REQFRST                                                     
         BNE   RUNL00                                                           
*                                                                               
         USING BUDRECD,R4                                                       
         LA    R4,MYKEY                                                         
         MVC   MYKEY,SPACES                                                     
         MVI   BUDKTYP,BUDKTYPQ                                                 
         MVC   BUDKCPY(1),MYCPNY                                                
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCOUNT,MYKEY,IO1                            
         B     PBUD20                                                           
*                                                                               
PBUD10   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCOUNT,MYKEY,IO1                            
*                                                                               
PBUD20   DS    0H                                                               
         MVI   UPDTREC,C'N'                                                     
         LA    R4,IO1                                                           
         CLI   BUDKTYP,BUDKTYPQ                                                 
         BH    PBUDXIT                                                          
         CLC   BUDKCPY(1),MYCPNY                                                
         BH    PBUDXIT                                                          
         CLC   BUDKBUDN,MYBUDNM                                                 
         BNE   PBUD10                                                           
         LA    R7,BUDREC1C                                                      
         CLC   BUDKUNT(2),=C'1C'                                                
         BE    PBUD30                                                           
         LA    R7,BUDREC2D                                                      
         CLC   BUDKUNT(2),=C'2D'                                                
         BNE   PBUD10                                                           
*                                                                               
PBUD30   DS    0H                                                               
         USING BAMELD,R6                                                        
         MVI   ELCODE,BAMELQ                                                    
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PBUD40   CLI   BAMMNTH,X'00'                                                    
         BNE   PBUD50                                                           
         MVI   0(R6),X'FF'                                                      
         MVI   UPDTREC,C'Y'                                                     
         AP    0(L'BUDREC1C,R7),BAMBUDG                                         
*                                                                               
PBUD50   BAS   RE,NEXTEL                                                        
         BE    PBUD40                                                           
*                                                                               
         CLI   UPDTREC,C'Y'                                                     
         BNE   PBUD10                                                           
*                                                                               
         BAS   RE,DMPGET                                                        
         GOTO1 DELEL,DMCB,(X'FF',(R4)),0                                        
         BAS   RE,DMPPUT                                                        
         CLI   RCWRITE,C'N'        TEST IF OK TO WRITE                          
         BE    PBUD10              NO                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,DMWRT,ACCOUNT,IO1,IO1                               
         CLI   8(R1),0                                                          
         BE    PBUD10                                                           
         DC    H'0'                                                             
*                                                                               
PBUDXIT  B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        RUNLAST                                                                
*-------------------------------------------------------------------*           
RUNL00   CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
*                                                                               
         MVC   P,SPACES                                                         
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
         EDIT  (P8,BUDREC1C),(16,P+25)                                          
         MVC   P+1(20),=CL20'1C DOLLARS DELETED'                                
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         MVC   P,SPACES                                                         
         GOTO1 ACREPORT                                                         
         MVC   P,SPACES                                                         
         EDIT  (P8,BUDREC2D),(16,P+25)                                          
         MVC   P+1(20),=CL20'2D DOLLARS DELETED'                                
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
YES      CR    RB,RB                                                            
         B     XIT                                                              
NO       LTR   RB,RB                                                            
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
         LA    R3,IO1                                                           
         SR    R8,R8                                                            
         ICM   R8,3,ACCORLEN(R3)                                                
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
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
         SPACE 1                                                                
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
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
*                                                                               
BUDREC1C DC    PL8'0'                                                           
BUDREC2D DC    PL8'0'                                                           
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'5'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'50000'                                                       
*                                                                               
UPDTREC  DC    C'N'                                                             
*                                                                               
MYCPNY   DC    XL1'83'                                                          
MYBUDNM  DC    XL2'0014'                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
* LITERAL POOL                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DSECT TO COVER WORKING STORAGE                                                
*                                                                               
ACXRD    DSECT                                                                  
RELO     DS    A                                                                
HELLO    DS    V                                                                
PRNTBL   DS    V                                                                
PARM     DS    6F                                                               
DMPSW    DS    CL1                                                              
ELCODE   DS    CL1                                                              
PRODUL   DS    CL2                                                              
*                                                                               
ELEMENT  DS    XL255                                                            
*                                                                               
MYKEY    DS    CL42                                                             
MYKEYSV  DS    CL42                                                             
*                                                                               
IO1      DS    CL2000                                                           
         EJECT                                                                  
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
**PAN#1  DC    CL21'025ACREPXR02A08/16/00'                                      
         END                                                                    
