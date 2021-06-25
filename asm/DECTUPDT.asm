*          DATA SET DECTUPDT   AT LEVEL 003 AS OF 01/18/12                      
*PROCESS USING(WARN(15))                                                        
*PHASE DECTUPDA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRTREC                                                                 
         TITLE 'UPDATE CONTROL FILE WITH RECORDS FROM INPUT FILE'               
***********************************************************************         
* THIS PROGRAM TAKES ALREADY CONSTRUCTED CONTROL RECORDS FROM AN INPUT*         
* FILE AND WRITES THEM TO CTFILE. IF A RECORD ALREADY EXISTS ON CTFILE*         
* IT WILL BE OVERWRITTEN.                                             *         
*                                                                     *         
* VALID PARAMETER CARDS:                                              *         
*  WRITE=YES/NO                                                       *         
*  TRACE=YES/NO                                                       *         
*  DDSIO=                                                             *         
*  DSPACE=                                                            *         
***********************************************************************         
DECTUPDT CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DECTUPDT,=V(REGSAVE),R9                                        
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE,=CL60'UPDATE CONTROL FILE'                                 
*                                                                               
         BAS   RE,VALPAR           VALIDATE INPUT PARAMETERS                    
*                                                                               
         BAS   RE,PROCREC          READ/UPDATE RECORDS                          
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS RECORDS                                                     *         
***********************************************************************         
*                                                                               
PROCREC  NTR1                                                                   
*                                                                               
         SR    R6,R6               CONTAINS ERROR CODE                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL ',DMFLIST,IO          
*                                                                               
         CLI   WRITE,C'Y'          WRITE=YES?                                   
         BNE   OPENDFRM                                                         
         GOTO1 =V(DATAMGR),DMCB,(0,=C'ENQCTL'),(C'E',=C'CTRL')                  
         TM    8(R1),X'04'         ENQ SUCCESSFUL?                              
         BO    *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
OPENDFRM OPEN  FILIN               INPUT FILE WITH CONTROL RECORDS              
*                                                                               
NEXTREC  DS    0H                                                               
         GET   FILIN,CTREC                                                      
         AP    TOTINPUT,=P'1'                                                   
*                                                                               
         MVI   ELCODE,X'01'        UPDATE ACTIVITY ELEMENT X'01'                
         LA    R5,CTKEY                                                         
         BAS   RE,GETEL                                                         
         BNE   ACT01X                                                           
         USING CTACTD,R5                                                        
         GOTO1 =V(DATCON),DMCB,(5,0),(3,CTACTDT) ACTIVITY DATE:TODAY            
ACT01X   DS    0X                                                               
         DROP  R5                                                               
*                                                                               
         MVI   ELCODE,X'F1'        UPDATE ACTIVITY ELEMENT X'F1'                
         LA    R5,CTKEY              WITH CHANGE DATE                           
         BAS   RE,GETEL                                                         
         BNE   ACTF1X                                                           
         USING ACTVD,R5                                                         
         GOTO1 =V(DATCON),DMCB,(5,0),(3,ACTVCHDT) CHANGE DATE:TODAY             
ACTF1X   DS    0X                                                               
         DROP  R5                                                               
*                                                                               
         MVI   BYTE,0              ASSUME WRITE=NO                              
         CLI   WRITE,C'Y'                                                       
         BNE   *+8                                                              
         MVI   BYTE,X'80'          WRITE=YES: READ FOR UPDATE                   
         GOTO1 =V(DATAMGR),DMCB,(BYTE,=C'DMREAD'),CTFILE,CTKEY,IO               
         CLI   DMCB+8,0                                                         
         BE    WRTREC              CTFILE RECORD WAS FOUND                      
*                                                                               
         TM    DMCB+8,X'FF'-X'10'  RECORD NOT FOUND?                            
         BZ    ADDREC              CORRECT: GO ADD IT                           
         LHI   R6,1                FATAL DATAMGR ERROR ON DMREAD                
         B     ENDCTBK             CLOSE CTFILE                                 
*                                                                               
ADDREC   AP    TOTADD,=P'1'                                                     
*                                                                               
         MVI   ELCODE,X'F1'        UPDATE ACTIVITY ELEMENT X'F1'                
         LA    R5,CTKEY              WITH ADD DATE                              
         BAS   RE,GETEL                                                         
         BNE   ADDACTF1                                                         
         USING ACTVD,R5                                                         
         GOTO1 =V(DATCON),DMCB,(5,0),(3,ACTVADDT) ADD DATE:TODAY                
ADDACTF1 DS    0X                                                               
         DROP  R5                                                               
*                                                                               
         CLI   TRACE,C'Y'          TRACE=YES?                                   
         BNE   ADDREC10                                                         
*                                                                               
         GOTO1 =V(PRTREC),DMCB,CTKEY,(28,25),V(PRINT),V(HEXOUT),       +        
               C'DOME',=C'ADDING NEW RECORD'                                    
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ADDREC10 CLI   WRITE,C'Y'          WRITE=YES?                                   
         BNE   ADDREC20                                                         
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(BYTE,=C'DMADD'),CTFILE,,CTKEY                  
         CLI   DMCB+8,0                                                         
         BE    ADDREC20                                                         
         LHI   R6,2                DMADD FAILED                                 
         B     ENDCTBK             CLOSE CTFILE                                 
*                                                                               
ADDREC20 B     NEXTREC                                                          
*                                                                               
WRTREC   AP    TOTCHG,=P'1'                                                     
         CLI   TRACE,C'Y'          TRACE=YES?                                   
         BNE   WRTREC10                                                         
*                                                                               
         GOTO1 =V(PRTREC),DMCB,CTKEY,(28,25),V(PRINT),V(HEXOUT),       +        
               C'DOME',=C'REPLACING WITH NEW RECORD'                            
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
WRTREC10 CLI   WRITE,C'Y'          WRITE=YES?                                   
         BNE   WRTREC20            NO                                           
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT'),CTFILE,CTKEY,CTKEY                
         CLI   DMCB+8,0                                                         
         BE    WRTREC20                                                         
         LHI   R6,3                DMWRT FAILED                                 
         B     ENDCTBK             CLOSE CTFILE                                 
*                                                                               
WRTREC20 B     NEXTREC                                                          
*                                                                               
ENDCTBK  DS    0H                  EODAD FOR READS                              
         CLI   WRITE,C'Y'          WRITE=YES?                                   
         BNE   CLOSDFRM                                                         
         GOTO1 =V(DATAMGR),DMCB,(0,=C'ENQCTL'),(C'D',=C'CTRL')                  
*                                                                               
CLOSDFRM DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMCLSE'),=C'CONTROL ',DMFLIST,IO          
         CLOSE FILIN                                                            
*                                                                               
         MVC   P(34),=C'TOTAL NUMBER OF ADDED RECORDS =   '                     
         EDIT  TOTADD,(8,P+34),ZERO=NOBLANK                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(34),=C'TOTAL NUMBER OF CHANGED RECORDS = '                     
         EDIT  TOTCHG,(8,P+34),ZERO=NOBLANK                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(34),=C'TOTAL NUMBER OF INPUT RECORDS =   '                     
         EDIT  TOTINPUT,(8,P+34),ZERO=NOBLANK                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CHI   R6,0                                                             
         BE    EXIT                                                             
*                                                                               
         ABEND 342,DUMP            FATAL ERROR ENCOUNTERED: LOOK AT R6          
*                                  *** MUST DEQUEUE CTFILE VIA JCL ***          
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS                                            *         
***********************************************************************         
*                                                                               
VALPAR   NTR1                                                                   
*                                  ASSUME WRITE=NO                              
         MVI   CTFILEW,C'N'        OPEN CTFILE NON-UPDATIVE                     
         MVI   CTRCVRW,C'X'        DON'T OPEN CTRCVR AT ALL                     
         OI    SSOSTAT2,SSOSNRCV   NO RECOVERY                                  
*                                                                               
VALPAR02 DS    0H                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(L'CARD),CARD      PRINT PARAMETER CARD                         
         GOTO1 =V(PRINTER)                                                      
         CLI   CARD,C'*'           SKIP CARDS COMMENTED OUT                     
         BE    VALPAR02                                                         
         LA    R1,PARAMS           R1=A(PARAMETER TABLE)                        
         SR    RE,RE                                                            
VALPAR04 ICM   RE,1,0(R1)          TEST FOR END OF TABLE                        
         BZ    VALPAR08            YES - NOT A VALID CONTROL CARD               
         EX    RE,*+8              MATCH CARD DATA TO TABLE ENTRY               
         BE    VALPAR06                                                         
         CLC   CARD(0),4(R1)                                                    
         AHI   R1,L'PARAMS         BUMP TO NEXT TABLE ENTRY                     
         B     VALPAR04                                                         
*                                                                               
VALPAR06 SR    RF,RF               PROCESS PARAMETER CARD                       
         ICM   RF,7,1(R1)          RF=A(PROCESS/VALIDATION ROUTINE)             
         LA    R1,CARD+1(RE)       R1=A(DATA VALUE)                             
         GOTO1 (RF),(R1)           CALL PROCESS/VALIDATE ROUTINE                
         B     VALPAR02                                                         
*                                                                               
VALPAR08 MVC   P(20),=C'INVALID CONTROL CARD'                                   
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                INVALID PARAMETER CARD                       
         SPACE 3                                                                
PARAMS   DS    0XL24               ** TABLE OF PARAMETER CARDS **               
         DC    AL1(05),AL3(PARDDSIO),CL20'DDSIO='                               
         DC    AL1(06),AL3(PARDSPAC),CL20'DSPACE='                              
         DC    AL1(05),AL3(PARWRITE),CL20'WRITE='                               
         DC    AL1(05),AL3(PARTRACE),CL20'TRACE='                               
         DC    AL1(01),AL3(PAREXIT),CL20'/*'                                    
         DC    AL1(0)              END-OF-TABLE                                 
         EJECT                                                                  
PARDDSIO L     RF,=V(DDSIO)        DDSIO=                                       
         MVC   0(8,RF),0(R1)                                                    
         BR    RE                                                               
*                                                                               
PARDSPAC MVC   SSODSPAC,0(R1)      DSPACE=                                      
         BR    RE                                                               
*                                                                               
PARWRITE MVC   WRITE,0(R1)         WRITE=                                       
         CLI   WRITE,C'Y'          TEST LIVE RUN                                
         BNER  RE                                                               
         MVI   CTFILEW,C'U'                                                     
         MVI   CTRCVRW,C'U'                                                     
         NI    SSOSTAT2,X'FF'-(SSOSNRCV+SSOSROLC)                               
         BR    RE                                                               
*                                                                               
PARTRACE MVC   TRACE,0(R1)         TRACE=                                       
         BR    RE                                                               
*                                                                               
PAREXIT  DS    0H                                                               
         MVI   P,0                                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   WRITE,C' '          WRITE= PARAMETER CARD IS REQUIRED            
         BNE   EXIT                                                             
*                                                                               
         MVC   P(27),=C'MISSING WRITE= CONTROL CARD'                            
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                MISSING WRITE= PARAMETER CARD                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
         GETEL R5,28,ELCODE                                                     
         SPACE 3                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
WRITE    DC    C' '                WRITE=Y/N                                    
TRACE    DC    C'N'                TRACE=Y/N                                    
CARD     DS    CL80                PARAMETER CARD                               
WORK     DS    CL80                                                             
KEY      DS    XL25                CTFILE KEY                                   
ELEM     DS    XL255               BUILD ELEMENT HERE                           
TOTINPUT DC    PL8'0'              TOTAL NUMBER OF INPUT RECORDS                
TOTADD   DC    PL8'0'              TOTAL NUMBER OF ADDED RECORDS                
TOTCHG   DC    PL8'0'              TOTAL NUMBER OF CHANGED RECORDS              
*                                                                               
DMWRT    DC    CL8'DMWRT   '                                                    
*                                                                               
DMFLIST  DS    0C                                                               
CTFILEW  DC    C'?'                                                             
CTFILE   DC    C'CTFILE '                                                       
CTRCVRW  DC    C'?'                                                             
CTRCVR   DC    C'CTRCVR '                                                       
         DC    C'X'                                                             
*                                                                               
         DS    0D                                                               
         DC    C'*SSB*SSB*SSB*SSB*SSB*SSB*SSB*SSB'                              
SSB      DS    0F                                                               
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSB                                                              
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED SSB                             
         ORG                                                                    
         SPACE 3                                                                
FILIN    DCB   DDNAME=FILIN,DSORG=PS,RECFM=VB,LRECL=1000,EODAD=ENDCTBK,+        
               MACRF=GM                                                         
         SPACE 3                                                                
CTREC    DS    XL1004              CONTROL RECORD FROM INPUT FILE               
         ORG   CTREC+4                                                          
CTKEY    DS    XL25                                                             
         ORG                                                                    
IO       DS    XL1000              I/O AREA FOR CTFILE RECORD                   
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DECTUPDT  01/18/12'                                      
         END                                                                    
