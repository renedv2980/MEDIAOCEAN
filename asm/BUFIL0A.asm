*          DATA SET BUFIL0A    AT LEVEL 018 AS OF 05/01/02                      
*PHASE T5020AA                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'T5020A - BUDGET CONTROL LFM - FILE TEST'                        
         SPACE 2                                                                
************************************************************                    
*                                                          *                    
* FIND UNKNOWN RECORDS ON BUDFIL                           *                    
*                                                          *                    
************************************************************                    
         SPACE 2                                                                
T5020A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FITE**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
FT1      L     RF,=V(PRNTBL)      RELOCATE INCLUDED MODULES                     
         AR    RF,R2                                                            
         ST    RF,VPRNTBL                                                       
*                                                                               
         GOTO1 VSETADD                                                          
*                                                                               
FT2      CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    FT4                                                              
         B     FTX                                                              
*                                                                               
FT4      BAS   RE,INITREP          INITIALIZE FOR REPORT                        
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,PARAS,(R8)                                                 
         BAS   RE,READF            READ THE FILE                                
         BAS   RE,TOTALS           PRINT THE TOTALS                             
*                                                                               
FTX      B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE REPORT                                              
*                                                                               
INITREP  LA    R1,HEDSPECS         SET UP FOR PRINTING                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R0,BUCKETS                                                       
         LA    R1,BUCKTAB                                                       
         ZAP   0(4,R1),=P'0'       CLEAR BUCKETS FOR EACH REPORT                
         LA    R1,L'BUCKTAB(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
* READ THE BUDGET FILE AND PERFORM CHECKS ON RECORDS                            
*                                                                               
READF    NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVI   KEY,C'B'                                                         
         GOTO1 HIGH                                                             
         B     READF2                                                           
*                                                                               
READF1   LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
READF2   CLI   BUKSYS,C'B'         TEST STILL IN BUDGET                         
         BNE   READFX                                                           
*                                                                               
         CLI   BUKRTYP,BUKRTYPQ    TEST MAJOR POINTER                           
         BNE   *+14                                                             
         AP    TOTRECS,=P'1'                                                    
         B     READF3                                                           
         CLI   BUKRTYP,BUCRTYPQ    TEST FOR PASSIVE POINTER                     
         BE    READF3                                                           
*                                                                               
         AP    BADRTYP,=P'1'                                                    
         MVC   WORK(20),BADRTYP+4                                               
         LA    R0,BUKLNQ                                                        
         GOTO1 VPRNTBL,DMCB,(20,WORK),KEY,C'DUMP',(R0),=C'1D'                   
         B     READF1                                                           
*                                                                               
READF3   TM    BUKCTYP,X'0F'       TEST FOR STRAY INDICATORS                    
         BZ    READF4              NO                                           
*                                                                               
         AP    BADSTAT,=P'1'                                                    
         MVC   WORK(20),BADSTAT+4                                               
         LA    R0,BUKLNQ                                                        
         GOTO1 VPRNTBL,DMCB,(20,WORK),KEY,C'DUMP',(R0),=C'1D'                   
         B     READF1                                                           
*                                                                               
READF4   CLI   BUKRTYP,BUCRTYPQ                                                 
         BE    READF6              SKIP PASSIVE POINTER                         
*                                                                               
         CLI   BUKSUB,0            TEST IF SUB-RECORD                           
         BE    READF6              NO                                           
         CLI   BUKSUB,BUVSUBQ      TEST IF BAD SUB-RECORD                       
         BNH   READF6              NO                                           
*                                                                               
         AP    BADSUB,=P'1'                                                     
         MVC   WORK(20),BADSUB+4                                                
         LA    R0,BUKLNQ                                                        
         GOTO1 VPRNTBL,DMCB,(20,WORK),KEY,C'DUMP',(R0),=C'1D'                   
*                                                                               
READF6   B     READF1                                                           
*                                                                               
READFX   B     XIT                                                              
         EJECT                                                                  
* SUB-ROUTINE TO PRINT OUT THE BUCKET TOTALS                                    
*                                                                               
TOTALS   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+10(20),=CL20'** FILE TEST TOTALS **'                           
         GOTO1 SPOOL,PARAS,(R8)                                                 
         GOTO1 SPOOL,PARAS,(R8)                                                 
         LA    R3,BUCKTAB          POINT TO BUCKET TABLE                        
         LA    R4,BUCKETS          COUNTER                                      
         SPACE 1                                                                
TOTALS2  DS    0H                                                               
         MVC   P+10(20),4(R3)      DESCRIPTION                                  
         MVI   P+30,C'='                                                        
         EDIT  (P4,0(R3)),(7,P+32)                                              
         GOTO1 SPOOL,PARAS,(R8)                                                 
         LA    R3,L'BUCKTAB(R3)                                                 
         BCT   R4,TOTALS2                                                       
         B     XIT                                                              
         EJECT                                                                  
* HOOK ROUTINE FOR HEADLINE PRINTING                                            
*                                                                               
HOOK     NTR1                                                                   
HOOKX    B     XIT                                                              
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
EFFS     DC    12X'FF'                                                          
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
BUCKTAB  DS    0CL24                                                            
TOTRECS  DC    PL4'0',CL20'TOTAL RECORDS'                                       
BADRTYP  DC    PL4'0',CL20'BAD RECORD TYPE'                                     
BADSTAT  DC    PL4'0',CL20'BAD STATUS BYTE'                                     
BADSUB   DC    PL4'0',CL20'BAD SUB-RECORD TYPE'                                 
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
*                                                                               
         SPROG 0,1                                                              
         SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,48,C'FILE TEST REPORT'                                        
         SSPEC H2,48,C'----------------'                                        
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,93,PAGE                                                       
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER REPORT REQUEST SCREEN                                          
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILFAD                                                       
         EJECT                                                                  
* WORKING STORAGE VALUES                                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
MYBASE   DS    A                                                                
VPRNTBL  DS    V                   V(PRNTBL)                                    
RECDA    DS    XL(L'BUKDA)                                                      
LASTKEY  DS    CL(L'BUKEY)                                                      
*                                                                               
         ORG   TWA1USER            SAVE AREA                                    
*                                                                               
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
COMMA    EQU   C','                                                             
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018BUFIL0A   05/01/02'                                      
         END                                                                    
