*          DATA SET PP131A     AT LEVEL 003 AS OF 05/01/02                      
*PHASE PP131A                                                                   
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT110                                                               
*INCLUDE PRINT                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMSECHK                                                                
         TITLE 'PP131 - PRNT RECOVERY FILE DUMP'                                
********************************************************************            
* THIS MODULE READS THE DISK RECOVERY FILE AND GENERATES TWO TAPES.*            
*                                                                  *            
* ALL PRTDIR RECORDS ARE IGNORED EXCEPT IN TOTAL INPUT COUNT       *            
*                                                                  *            
* TAPES HAVE ALL FILE RECORDS EXCEPT POINTER COPIES AND CHANGES.   *            
*        (THESE RECORDS HAVE X'80' ON IN RRECTYPE)                 *            
********************************************************************            
*                                                                               
*  RCRI  8/24/98 CHANGE TO TALK TO DATA SPACES                                  
*                USE SYSLIST RECORD TO FIND SE NUM                              
*                                                                               
*  BPLA  9/19/96 CHANGE BLKSIZE TO 26364 (FROM 8000)                            
*                                                                               
*  BPLA  3/19/91 PRTDIR LOGIC NO-OPED - PROBLEM WAS IN LOCKER                   
*                                                                               
*  BPLA  11/8/90 CHANGES TO TRY AND FIND PRTFILE BUYREC ADDS                    
*                WITHOUT CORRESPONDING PRTDIR ADDS (20 +21 POINTERS)            
*                DIRECTORY OVERLAYS WILL BE CHECK LIKE ADDS                     
*                                                                               
       ++INCLUDE DMGREQUS                                                       
         SPACE 2                                                                
PP131    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*PP131*,=A(WORK)                                               
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     R1,X'10'            GET CPU ID                                   
         L     R1,X'C4'(R1)                                                     
         MVC   CPUID,X'10'(R1)     CPU IS C'XXXX' FROM SMCASID                  
         MVC   TITLE(3),CPUID                                                   
         MVI   TITLE+4,C'-'                                                     
         MVC   TITLE+6(24),=CL24'PRNT. RECOVERY FILE DUMP'                      
*                                                                               
RDCARD   DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
RC2      CLC   =C'/*',CARD                                                      
         BE    RC30                                                             
*                                                                               
RC4      CLC   =C'PRNT',CARD                                                    
         BNE   RC6                                                              
         MVC   TITLE+6(5),CARD     PRNTX                                        
         MVI   DUMPSYS,X'04'       SET SYSTEM TYPE FOR PRINT                    
         MVC   DUMPFIL,CARD+4      SET FILE NUMBER                              
         MVC   INPSYS,CARD                                                      
         BAS   RE,GETSE                                                         
         B     RDCARD                                                           
*                                                                               
RC6      DS    0H                                                               
         CLC   =C'ERASE=Y',CARD                                                 
         BNE   *+12                                                             
         MVI   ERASESW,C'Y'                                                     
         B     RDCARD                                                           
         CLC   =C'ERASE=N',CARD                                                 
         BNE   *+12                                                             
         MVI   ERASESW,C'N'                                                     
         B     RDCARD                                                           
*                                                                               
RC8      EQU   *                                                                
*                                                                               
RC10     CLC   =C'DDSIO=',CARD                                                  
         BNE   RC12                                                             
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6                                                   
         B     RDCARD                                                           
*                                                                               
RC12     CLC   =C'GLOBAL=',CARD                                                 
         BNE   RC14                                                             
         MVC   GLOBAL,CARD+7       SET GLOBAL/LOCAL FILE FLAG                   
         B     RDCARD                                                           
*                                                                               
RC14     CLC   =C'OVERRIDE',CARD                                                
         BE    *+14                                                             
         CLC   =C'FORCE=Y',CARD                                                 
         BNE   RC16                                                             
         MVI   OVERRIDE,C'Y'       SET OVERRIDE CPUID/DSPID CHECKS              
         B     RDCARD                                                           
*                                                                               
RC16     DS    0H                                                               
         B     CRDERR                                                           
*                                                                               
RC30     DS    0H                                                               
         CLI   DUMPFIL,C' '                                                     
         BNH   SYSERR                                                           
         EJECT                                                                  
*OPEN RECOVERY FILE AND GET FILE INFO                                           
*                                                                               
OPEN     LA    R0,X'44'            SET AND SAVE RECOVERY FILE NUMBER            
         STC   R0,FILENO                                                        
         GOTO1 =V(DMOD000),ERSPARS,A(DMEXT),,,((R0),0)                          
         SR    R4,R4                                                            
         ICM   R4,7,13(R1)         GET AND SAVE A(DTF)                          
         ST    R4,FILEDTFA                                                      
         MVC   FILEDD,22(R4)                                                    
*                                                                               
OPEN12   CLI   GLOBAL,C'N'         GLOBAL=N ENTERED                             
         BNE   OPEN14                                                           
         NI    19(R4),255-X'40'                                                 
         B     OPEN16                                                           
*                                                                               
OPEN14   CLI   GLOBAL,C'Y'         GLOBAL=Y ENTERED                             
         BNE   OPEN16                                                           
         OI    19(R4),X'40'                                                     
*                                                                               
OPEN16   EQU   *                                                                
         GOTO1 =V(DADDS),ERSPARS,A(DAOPEN)                                      
*NOP*    GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'PRINT',FLIST,REC                  
*                                                                               
OPEN18   CLI   GLOBAL,C'N'         CALL V(DMSECHK) TO OK DUMP/ERASE             
         BE    OPEN20                                                           
         LA    R0,0                                                             
         CLI   OVERRIDE,C'Y'       TEST IF OVERRIDE PARM INPUT                  
         BNE   *+8                                                              
         LA    R0,X'20'                                                         
         GOTO1 =V(DMSECHK),DMCB,((R0),=C'DMSLCK'),FILEDD,(R4),(1,0)             
         L     RE,12(R1)                                                        
         MVC   MSG,0(RE)                                                        
         MVC   RESULT,12(R1)                                                    
         CLI   RESULT,2            0=OK,1=OK,2=IGNORE,3=CANCEL                  
         BL    OPEN18X                                                          
         BH    END0                OPERATOR REQUESTED CANCEL                    
         MVC   P(L'MSG),MSG                                                     
         GOTO1 =V(PRINTER)         OPERATOR IGNORED ERROR                       
         B     OPEN20                                                           
OPEN18X  L     RF,=V(DMSECHKS)     SAVE SYTEM LOCK INFO                         
         MVC   FILELOCK,0(RF)                                                   
*                                                                               
OPEN20   OPEN  (OUT1,(OUTPUT),OUT2,(OUTPUT))                                    
         LTR   RF,RF                                                            
         BZ    GET1                TAPES OPENED OK                              
         ST    RF,OPENRF                                                        
         GOTO1 =V(LOGIO),DMCB,(X'FF',1),(30,ERROPN)                             
         MVC   P(L'ERROPN),ERROPN                                               
         GOTO1 =V(PRINTER)                                                      
         OC    FILELOCK,FILELOCK   FREE DSPACE LOCK IF APPLIED                  
         BZ    OPEN20X                                                          
         GOTO1 =V(DMSECHK),DMCB,((R0),=C'DMSUNL')                               
OPEN20X  L     RF,OPENRF                                                        
         DC    H'0'                BAD FILE OPEN                                
*                                                                               
FLIST    DC    CL8' PRECV  ',CL8'X      '                                       
         EJECT                                                                  
* READ RECOVERY FILE SEQUENTIALLY                                               
*                                                                               
GET1     GOTO1 =V(DATAMGR),DMCB,(X'00',=C'DMRSEQ'),=C'PRECOV ',        X        
               DA,REC,A(BUFF)                                                   
         TM    8(R1),X'40'         TEST ERROR                                   
         BNZ   GETX                                                             
         TM    8(R1),X'80'         TEST EOF                                     
         BNZ   END1                                                             
         AP    TRECSIN+14(4),=P'1'                                              
*                                                                               
*                             NO-OP THIS TEST IF YOU WANT TO DUMP               
*                             DIRECTORY CHANGES                                 
         TM    RRECTY,X'80'                                                     
         BNZ   GET1                SKIP POINTER COPIES/CHANGES                  
**NEW 1/30/90          WAS X'10'                                                
         CLI   RFILTY,X'40'        TEST PRTDIR REC                              
         BNE   GET3                                                             
*                                  TEST FOR DIRECTORY OVERLAY                   
         OC    RECKEY+21(3),RECKEY+21   NOT IF PASSIVE                          
         BNZ   GET1                                                             
         CLI   RRECTY,X'01'        SAVE COPY                                    
         BNE   GET2                                                             
         MVC   SAVEREC,RECKEY      SAVE WHOLE RECORD                            
         B     GET1                                                             
*                                                                               
GET2     DS    0H                                                               
         CLI   RRECTY,X'02'        FOR CHANGES                                  
         BNE   GET1                                                             
         CLC   SAVEREC(25),RECKEY  TEST SAME KEY                                
         BNE   GET1                                                             
         CLC   SAVEADDR,RECADDR    TEST SAME DA                                 
         BE    GET1                                                             
         CLI   SAVEREC+25,X'FF'    FF DELETES DO NOT CAUSE                      
         BE    GET1                DIRECTORY OVERLAYS                           
***                                                                             
***      CODE BELOW WILL DETAIL DICTORY OVERLAYS                                
***                                                                             
***      B     GET2C                 BRANCH AROUND FOR NOW                      
***                                                                             
***      MVC   P+1(24),=C'DIRECTORY OVERLAY - COPY'                             
***      GOTO1 =V(HEXOUT),DMCB,SAVEREC,P+30,27                                  
***      GOTO1 (RF),(R1),SAVEADDR,P+92,4                                        
***      GOTO1 =V(PRINTER)                                                      
***      MVC   P+1(24),=C'DIRECTORY OVERLAY - CHA '                             
***      GOTO1 =V(HEXOUT),DMCB,RECKEY,P+30,27                                   
***      GOTO1 (RF),(R1),RECADDR,P+92,4                                         
***      GOTO1 =V(PRINTER)                                                      
***                                                                             
GET2C    DS    0H                                                               
         AP    DIRRECS+14(4),=P'1'                                              
         B     GET12             TREAT DIRECTORY OVERLAYS AS ADDS               
*                                                                               
*                                                                               
*                                                                               
GET3     DS    0H                                                               
**NEW 1/30/90      WAS X'11'                                                    
         CLI   RFILTY,X'41'        TEST PUBDIR REC                              
         BE    GET1                SKIP                                         
*                                                                               
GET4     DS    0H                                                               
GET5     LA    R3,CTRS                                                          
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         CLC   RFILTY(2),0(R3)     MATCH FILE/REC                               
         BE    GET8                                                             
         BXLE  R3,R4,*-10                                                       
         B     GET12                                                            
*                                                                               
GET8     AP    14(4,R3),=P'1'                                                   
*                                                                               
GET12    DS    0H                                                               
         SPACE 1                                                                
* ALL RECORDS GO TO TAPE 1 (OUT1) *                                             
         SPACE 1                                                                
         LH    RE,DM5+2            GET REC LEN                                  
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,RECLEN                                                        
         LA    R0,RECLEN                                                        
         L     R1,=A(OUT1)                                                      
         PUT   (1),(0)                                                          
         AP    TRECS1+14(4),=P'1'                                               
*                                                                               
         LA    R0,RECLEN                                                        
         L     R1,=A(OUT2)                                                      
         PUT   (1),(0)                                                          
         AP    TRECS2+14(4),=P'1'                                               
*                                                                               
         B     GET1                                                             
         SPACE 2                                                                
GETX     AP    ERRORS,=P'1'                                                     
         SPACE 1                                                                
* BUMP DISK ADDRESS TO START OF NEXT TRACK *                                    
         SPACE 1                                                                
         LH    RE,DA                                                            
         LA    RE,1(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,DA                                                            
         B     GET1                                                             
         EJECT                                                                  
END0     GOTO1 =V(LOGIO),DMCB,(X'FF',1),(60,MSG)                                
         MVC   P(L'MSG),MSG                                                     
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
END1     CP    TRECSIN+14(4),=P'1' TEST ANY RECORDS                             
         BL    ERASE                                                            
         CLOSE (OUT1,,OUT2,)                                                    
         SPACE 2                                                                
         CP    ERRORS,=P'0'        CHECK FOR ERRORS                             
         BE    ERASE               NO - GO ON                                   
         OI    ERRORS+3,X'0F'                                                   
         UNPK  ERRMSG(4),ERRORS                                                 
         GOTO1 =V(LOGIO),DMCB,(X'FF',1),(69,ERRMSG)                             
         B     ERASE                                                            
*                                                                               
ERRMSG   DC    CL30'0000 RECOVERY FILE READ ERRORS'                             
         DC    X'15'                                                            
         DC    CL37'********** FILE NOT ERASED **********'                      
         DC    X'15'                                                            
ERRORS   DC    PL4'0'                                                           
*                                                                               
ERROPN   DC    CL30'OUTPUT FILE OPEN FAILURE'                                   
         EJECT                                                                  
* DO NOT ERASE RECOVERY FILE IF READ ERRORS OCCURED                             
*                                                                               
ERASE    MVC   P(24),=C'XXXXXXXX FILE NOT ERASED'                               
         MVC   P(8),FILEDD                                                      
         CP    ERRORS,=P'0'        SKIP IF ERRORS                               
         BNE   ERASE2                                                           
         CLI   ERASESW,C'Y'        SKIP IF ERASE=NO INPUT                       
         BNE   ERASE2                                                           
         MVC   P(24),=C'XXXXXXXX IS BEING ERASED'                               
         MVC   P(8),FILEDD                                                      
*                                                                               
ERASE1   GOTO1 =V(DADDS),ERSPARS,A(WTERASE)                                     
*                                                                               
ERASE2   OC    FILELOCK,FILELOCK   FREE DSPACE LOCK IF APPLIED                  
         BZ    ERASE3                                                           
         GOTO1 =V(DMSECHK),DMCB,((R0),=C'DMSUNL')                               
*                                                                               
ERASE3   GOTO1 =V(DADDS),ERSPARS,A(DACLOSE)                                     
*                                                                               
ERASE4   GOTO1 =V(PRINTER)         PRINT ERASE/NOT ERASED MESSAGE               
         GOTO1 =V(PRINTER)                                                      
         EJECT                                                                  
* PRINT COUNTERS                                                                
*                                                                               
PRTCNT   MVC   P(8),FILEDD                                                      
         MVC   P+9(24),=C'RECOVERY RECORD COUNTERS'                             
         GOTO1 =V(PRINTER)                                                      
         ZAP   DUB,DIRRECS+14(4)                                                
         CVB   R1,DUB                                                           
         SRL   R1,1                                                             
         CVD   R1,DUB                                                           
         ZAP   DIRRECS+14(4),DUB                                                
         SPACE 1                                                                
         LA    R3,CTRS                                                          
         LH    R4,0(R3)                                                         
         LA    R5,TCTRSX-1         SET TO INCLUDE TOTALS                        
         LA    R3,6(R3)                                                         
         SPACE 1                                                                
PRTCNT1  MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
         MVC   P(12),2(R3)                                                      
         OI    17(R3),X'0F'                                                     
         UNPK  P+13(5),14(4,R3)                                                 
         GOTO1 =V(PRINTER)                                                      
         BXLE  R3,R4,PRTCNT1                                                    
         SPACE 1                                                                
EOJ      XBASE                                                                  
*                                                                               
CRDERR   MVC   P(31),=C'PP131 ** INVALID PARAMETER CARD'                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
*                                                                               
SYSERR   MVC   P(30),=C'PP131 ** NO VALID SYSTEM INPUT'                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(30,P)                                
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
         EJECT                                                                  
***********************************************************************         
* GET SE NUMBER FOR PRINT FILE                                        *         
***********************************************************************         
         SPACE 1                                                                
GETSE    NTR1  ,                                                                
         L     RE,=V(UTL)                                                       
         MVI   4(RE),10            SET TO READ IN CONTROL SYSTEM                
         LA    R2,REC                                                           
         USING CTWREC,R2           READ SYSTEM LIST RECORD (FOR FILE)           
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         MVC   CTWKSYSN,DUMPSYS    GET LIST ONLY FOR THIS SYSTEM TYPE           
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',CTFLIST                  
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE',CTWREC,CTWREC             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETSE1   SR    R0,R0                                                            
         LA    R1,CTWDATA                                                       
         USING SYSELD,R1           LOCATE SYSTEM ELEMENT FOR SENO               
*                                                                               
GETSE3   CLI   SYSEL,0             TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   SYSEL,SYSELQ        TEST SYSTEM ELEMENT                          
         BNE   GETSE5                                                           
         CLC   SYSSYS,DUMPSYS      TEST RIGHT SYSTEM                            
         BE    GETSE7                                                           
*                                                                               
GETSE5   IC    R0,SYSLEN           BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     GETSE3                                                           
*                                                                               
GETSE7   LA    RE,SYSNAME+L'SYSNAME-1                                           
         CLI   0(RE),C' '          LOCATE LAST CHARACTER OF NAME                
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         CLC   DUMPFIL,0(RE)       TEST RIGHT FILE SET                          
         BNE   GETSE5              TRY ANOTHER                                  
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),SYSSEN                                                   
         XIT1                                                                   
         DROP  R2                                                               
*                                                                               
CTFLIST  DC    C'NCTFILE X'                                                     
         EJECT                                                                  
         CNOP  2,4                                                              
CTRS     DC    H'18'                                                            
         DC    A(CTRSX-1)                                                       
**NEW 1/30/90         WAS X'1203',X'1201',X'1202',X'1303'                       
**NEW 1/30/90         WAS X'1301',X'1302'                                       
         DC    X'4203',CL12'PRTFILE ADDS',PL4'0'                                
         DC    X'4201',CL12'PRTFILE CPYS',PL4'0'                                
         DC    X'4202',CL12'PRTFILE CHGS',PL4'0'                                
         DC    X'4303',CL12'PUBFILE ADDS',PL4'0'                                
         DC    X'4301',CL12'PUBFILE CPYS',PL4'0'                                
         DC    X'4302',CL12'PUBFILE CHGS',PL4'0'                                
CTRSX    EQU   *                                                                
*                                                                               
TRECSIN  DC    X'0000',CL12'RECOVERY IN ',PL4'0'                                
TRECS1   DC    X'0000',CL12'OUT1        ',PL4'0'                                
TRECS2   DC    X'0000',CL12'OUT2-BACKUP ',PL4'0'                                
DIRRECS  DC    X'0000',CL12'DIR OVERLAYS',PL4'0'                                
TCTRSX   EQU   *                                                                
         EJECT                                                                  
DUB      DS    D                                                                
*                                                                               
DMCB     DS    6F                                                               
         ORG   *-24                                                             
DM1      DS    F                                                                
DM2      DS    F                                                                
DM3      DS    F                                                                
DM4      DS    F                                                                
DM5      DS    F                                                                
DM6      DS    F                                                                
*                                                                               
DA       DC    F'0'                                                             
*                                                                               
ERASESW  DC    C'N'                                                             
DUMPSYS  DC    X'00'                                                            
DUMPFIL  DC    C' '                                                             
INPSYS   DC    CL5' '                                                           
RESULT   DC    X'00'                                                            
GLOBAL   DC    C' '                                                             
OVERRIDE DC    C' '                                                             
CPUID    DC    CL4' '                                                           
FILENO   DC    X'00'                                                            
FILELOCK DC    XL2'00'                                                          
FILEDD   DC    CL8' '                                                           
MSG      DC    CL60' '                                                          
OPENRF   DC    A(0)                                                             
FILEDTFA DC    A(0)                                                             
*                                                                               
CARD     DS    CL80                                                             
SAVEREC  DS    0XL31                                                            
         DS    XL27                                                             
SAVEADDR DS    XL4                                                              
*                                                                               
ERSPARS  DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(ERSPAR6)                                                       
ERSPAR6  DC    A(0)                ** ERASE WHOLE FILE  **                      
*                                                                               
         LTORG                                                                  
*                                                                               
RECLEN   DS    F                                                                
REC      DS    8000C                                                            
         ORG   REC                                                              
       ++INCLUDE DMRCVRHDR                                                      
RECKEY   DS    XL25                                                             
         DS    XL2                                                              
RECADDR  DS    XL4                                                              
         ORG                                                                    
         EJECT                                                                  
OUT1     DCB   DDNAME=OUT1,            DOS SYS005                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04628,                                            X        
               BLKSIZE=26364,                                          X        
               MACRF=PM                                                         
*                                                                               
OUT2     DCB   DDNAME=OUT2,            DOS SYS006                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04628,                                            X        
               BLKSIZE=26364,                                          X        
               MACRF=PM                                                         
*                                                                               
BUFF     DS    64000C              BUFFER FOR FULL TRACK READ                   
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
SSB      CSECT                                                                  
         DC    XL2'00',X'FF',XL5'00',XL248'00'                                  
UTL      CSECT                                                                  
         DC    256X'00'            SE FILLED IN VIA CODE                        
WORK     CSECT                                                                  
         DS    500D                                                             
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PP131A    05/01/02'                                      
         END                                                                    
