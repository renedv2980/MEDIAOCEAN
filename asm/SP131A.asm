*          DATA SET SP131A     AT LEVEL 003 AS OF 05/01/02                      
*PHASE SP131A                                                                   
*INCLUDE SP131                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT110                                                               
*INCLUDE PRINT                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMSECHK                                                                
         TITLE 'SP131 - SPOT/TRF/UNT RECOVERY FILE DUMP'                        
********************************************************************            
* THIS MODULE READS THE DISK RECOVERY FILE AND GENERATES TWO TAPES.*            
*                                                                  *            
* ALL SPTDIR RECORDS ARE IGNORED EXCEPT IN TOTAL INPUT COUNT       *            
*                                                                  *            
* TAPES HAVE ALL SPOTPAK RECORDS EXCEPT POINTER COPIES AND CHANGES.*            
*        (THESE RECORDS HAVE X'80' ON IN RRECTYPE)                 *            
********************************************************************            
       ++INCLUDE DMGREQUS                                                       
         SPACE 2                                                                
SP131    CSECT                                                                  
         SPACE 1                                                                
*=====================================================*                         
* RECOVERY FILE RECORD AND ACTION EQUATES             *                         
*=====================================================*                         
         SPACE 1                                                                
QSPTFIL  EQU   X'21'                                                            
QSTAFIL  EQU   X'22'                                                            
QSPTDIR  EQU   X'23'                                                            
QSPTRCV  EQU   X'24'                                                            
QXSPDIR  EQU   X'36'                                                            
QXSPFIL  EQU   X'37'                                                            
*                                                                               
QREQUEST EQU   X'25'                                                            
QUNTDIR  EQU   X'27'                                                            
QUNTFIL  EQU   X'2A'                                                            
*                                                                               
QTRFFIL  EQU   X'32'                                                            
QTRFDIR  EQU   X'33'                                                            
QTRFRCV  EQU   X'34'                                                            
*                                                                               
QCPY     EQU   1                                                                
QCHG     EQU   2                                                                
QADD     EQU   3                                                                
         PRINT NOGEN                                                            
         NBASE 0,*SP131*,=A(WORK)                                               
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     R1,X'10'            GET CPU ID                                   
         L     R1,X'C4'(R1)                                                     
         MVC   CPUID,X'10'(R1)     CPU IS C'XXXX' FROM SMCASID                  
         MVC   TITLE(3),CPUID                                                   
         MVI   TITLE+4,C'-'                                                     
         MVC   TITLE+11(25),=CL25' RECOVERY FILE DUMP'                          
*                                                                               
RDCARD   DS    0H                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         CLC   =C'/*',CARD                                                      
         BE    RC30                                                             
*                                                                               
         CLC   =C'SPOT',CARD                                                    
         BE    RC2                                                              
*                                                                               
         CLC   =C'NET',CARD                                                     
         BE    RC3                                                              
*                                                                               
         CLC   =C'TRF',CARD                                                     
         BE    RC4                                                              
         B     RC6                                                              
*                                                                               
RC2      MVC   TITLE+6(5),CARD     SPOTX                                        
         MVI   DUMPSYS,X'02'       SET SYSTEM TYPE                              
         MVC   DUMPFIL,CARD+4      SET FILE NUMBER                              
         MVC   INPSYS,CARD                                                      
         BAS   RE,GETSE                                                         
         B     RDCARD                                                           
*                                                                               
RC3      MVC   TITLE+6(4),CARD     NETX                                         
         MVI   DUMPSYS,X'03'       SET SYSTEM TYPE                              
         MVC   DUMPFIL,CARD+3      SET FILE NUMBER                              
         MVC   INPSYS,CARD                                                      
         BAS   RE,GETSE                                                         
         B     RDCARD                                                           
*                                                                               
RC4      MVC   TITLE+6(4),CARD                                                  
         MVI   DUMPSYS,X'0D'       SET SYSTEM TYPE                              
         MVC   DUMPFIL,CARD+3      SET FILE NUMBER                              
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
RC8      DS    0H                                                               
         CLC   =C'FIRST=',CARD                                                  
         BNE   RC10                                                             
         GOTO1 =V(HEXIN),DMCB,CARD+6,DA,8                                       
         B     RDCARD                                                           
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
RC30     L     RE,=V(UTL)          CHECK IF LAST SYSTEM WAS VALID               
         CLI   4(RE),0                                                          
         BNH   SYSERR                                                           
         B     OPEN                                                             
         EJECT                                                                  
***********************************************************************         
* GET SE NUMBER FOR SPOT/NET/TRF FILE                                 *         
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
* OPEN RECOVERY FILE                                                            
*                                                                               
OPEN     LA    R2,=C'SPOT'                                                      
         LA    R3,FLIST                                                         
         LA    R0,QSPTRCV                                                       
         CLI   TITLE+6,C'S'        THIS A SPOT RECV DUMP                        
         BE    OPEN10                                                           
         CLI   TITLE+6,C'N'        OR A NET RECV DUMP                           
         BE    OPEN10                                                           
         LA    R2,=C'STR'                                                       
         LA    R3,TLIST                                                         
         LA    R0,QTRFRCV                                                       
         CLI   TITLE+6,C'T'        THIS A TRF RECV DUMP                         
         BE    OPEN10                                                           
         DC    H'0'                                                             
*                                                                               
OPEN10   STC   R0,FILENO           SAVE RECOVERY FILE NUMBER                    
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
*NOP*    GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',(R2),(R3),REC                        
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
OPEN20   EQU   *                                                                
         OPEN  (OUT1,(OUTPUT),OUT2,(OUTPUT))                                    
         LTR   RF,RF                                                            
         BZ    GET1                                                             
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
FLIST    DC    CL8' RECV   '                                                    
         DC    CL8'X       '                                                    
*                                                                               
TLIST    DC    CL8' TRFRCV '                                                    
         DC    CL8'X       '                                                    
         EJECT                                                                  
* READ RECOVERY FILE SEQUENTIALLY                                               
*                                                                               
GET1     GOTO1 =V(DATAMGR),DMCB,(X'00',=C'DMRSEQ'),=C'RECOVER',        X        
               DA,REC,A(BUFF)                                                   
         TM    8(R1),X'40'         TEST ERROR                                   
         BNZ   GET1X                                                            
         TM    8(R1),X'80'         TEST EOF                                     
         BNZ   END1                                                             
         AP    TRECSIN+14(4),=P'1'                                              
*                                                                               
* THESE TESTS ELIMINATE DIRECTORY RECORDS FROM RECOVERY TAPE                    
*                                                                               
         CLI   RFILTY,QSPTDIR      TEST SPTDIR                                  
         BE    GET1                YES - SKIP                                   
         CLI   RFILTY,QXSPDIR      TEST XSPDIR                                  
         BE    GET1                YES - SKIP                                   
         CLI   RFILTY,QTRFDIR      TEST TRFDIR                                  
         BE    GET1                YES - SKIP                                   
         CLI   RFILTY,QUNTDIR      TEST UNTDIR                                  
         BE    GET1                YES - SKIP                                   
*                                                                               
         CLI   RFILTY,QSTAFIL      TEST STATION                                 
         BNE   GET1A                                                            
         CLC   REC+24+15(2),=X'0000'  TEST LENGTH ZERO                          
         BE    GET1                   YES - SKIP                                
*                                                                               
GET1A    LA    R3,CTRS                                                          
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         CLC   RFILTY(2),0(R3)     MATCH FILE/REC                               
         BE    GET1B                                                            
         BXLE  R3,R4,*-10                                                       
         B     GET1C                                                            
*                                                                               
GET1B    AP    14(4,R3),=P'1'                                                   
*                                                                               
GET1C    TM    RRECTY,X'80'                                                     
         BNZ   GET1                SKIP POINTER COPIES/CHANGES                  
         SPACE 1                                                                
* WRITE RECORD TO TAPE1 AND TAPE2 *                                             
         SPACE 1                                                                
         SR    RE,RE               ** DEIS -- CHANGED LH TO SR/ICM              
         ICM   RE,3,DM5+2          GET REC LEN                                  
**NOP**  C     RE,=F'4000'         ** DEIS -- WAS COMPARE HALFWORD              
         C     RE,=F'3996'         ** MHER -- NICE TRY DAVID !!                 
         BNH   *+14                                                             
         AP    TREC2BIG+14(4),=P'1'                                             
         B     GET1                ORIGINAL RECORD LENGTH > 3976                
*                                                                               
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
*                                                                               
         CLI   RFILTY,QSPTFIL      TEST SPTFILE                                 
         BNE   *+14                                                             
         AP    TRECS2SP+14(4),=P'1'                                             
         B     GET1                                                             
*                                                                               
         CLI   RFILTY,QXSPFIL      TEST XSPFILE                                 
         BNE   *+14                                                             
         AP    TRECS2SX+14(4),=P'1'                                             
         B     GET1                                                             
*                                                                               
         CLI   RFILTY,QSTAFIL      TEST STATION                                 
         BNE   *+14                                                             
         AP    TRECS2ST+14(4),=P'1'                                             
         B     GET1                                                             
*                                                                               
         CLI   RFILTY,QTRFFIL      TEST TRFFILE                                 
         BNE   *+14                                                             
         AP    TRECS2TR+14(4),=P'1'                                             
         B     GET1                                                             
*                                                                               
         CLI   RFILTY,QUNTFIL      TEST UNTFIL                                  
         BNE   *+10                                                             
         AP    TRECS2UN+14(4),=P'1'                                             
         B     GET1                                                             
         SPACE 2                                                                
GET1X    AP    ERRORS,=P'1'                                                     
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
*                                                                               
END2     CP    ERRORS,=P'0'        OUTPUT MESSAGE IF ANY ERRORS                 
         BE    ERASE                                                            
         OI    ERRORS+3,X'0F'                                                   
         UNPK  ERRMSG(4),ERRORS                                                 
         GOTO1 =V(LOGIO),DMCB,(X'FF',1),(69,ERRMSG)                             
         MVC   P(69),ERRMSG                                                     
         GOTO1 =V(PRINTER)                                                      
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
* DO NOT ERASE RECOVERY FILE IF READ ERRORS OCCURED *                           
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
         LA    R3,CTRS             POINT TO COUNTERS TABLE                      
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
*                                                                               
EOJ      XBASE                                                                  
*                                                                               
CRDERR   MVC   P(31),=C'SP131 ** INVALID PARAMETER CARD'                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
*                                                                               
SYSERR   MVC   P(30),=C'SP131 ** NO VALID SYSTEM INPUT'                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(30,P)                                
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
         EJECT                                                                  
         CNOP  2,4                                                              
CTRS     DC    H'18'                                                            
         DC    A(CTRSX-1)                                                       
         DC    AL1(QSPTFIL,QADD),CL12'SPTFILE ADDS',PL4'0'                      
         DC    AL1(QSPTFIL,QCPY),CL12'SPTFILE CPYS',PL4'0'                      
         DC    AL1(QSPTFIL,QCHG),CL12'SPTFILE CHGS',PL4'0'                      
         DC    AL1(QXSPFIL,QADD),CL12'XSPFILE ADDS',PL4'0'                      
         DC    AL1(QXSPFIL,QCPY),CL12'XSPFILE CPYS',PL4'0'                      
         DC    AL1(QXSPFIL,QCHG),CL12'XSPFILE CHGS',PL4'0'                      
         DC    AL1(QSPTFIL,QCPY+128),CL12'SPTF PTR CPY',PL4'0'                  
         DC    AL1(QSTAFIL,QADD),CL12'STATION ADDS',PL4'0'                      
         DC    AL1(QSTAFIL,QCPY),CL12'STATION CPYS',PL4'0'                      
         DC    AL1(QSTAFIL,QCHG),CL12'STATION CHGS',PL4'0'                      
         DC    AL1(QTRFFIL,QADD),CL12'TRFFILE ADDS',PL4'0'                      
         DC    AL1(QTRFFIL,QCPY),CL12'TRFFILE CPYS',PL4'0'                      
         DC    AL1(QTRFFIL,QCHG),CL12'TRFFILE CHGS',PL4'0'                      
         DC    AL1(QUNTFIL,QADD),CL12'UNTFIL ADDS ',PL4'0'                      
         DC    AL1(QUNTFIL,QCPY),CL12'UNTFIL CPYS ',PL4'0'                      
         DC    AL1(QUNTFIL,QCHG),CL12'UNTFIL CHGS ',PL4'0'                      
CTRSX    EQU   *                                                                
*                                                                               
TRECSIN  DC    X'0000',CL12'RECOVERY IN ',PL4'0'                                
TRECS1   DC    X'0000',CL12'OUT1- TOTAL ',PL4'0'                                
TRECS2SP DC    X'0000',CL12'OUT2-SPTFIL ',PL4'0'                                
TRECS2SX DC    X'0000',CL12'OUT2-XSPFIL ',PL4'0'                                
TRECS2ST DC    X'0000',CL12'OUT2-STATION',PL4'0'                                
TRECS2TR DC    X'0000',CL12'OUT2-TRFFIL ',PL4'0'                                
TRECS2UN DC    X'0000',CL12'OUT2-UNTFIL ',PL4'0'                                
TREC2BIG DC    X'0000',CL12'TOO LARGE   ',PL4'0'                                
TCTRSX   EQU   *                                                                
         EJECT                                                                  
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
CARD     DS    CL80                                                             
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
ERASESW  DC    C'N'                                                             
DUMPSYS  DC    X'00'                                                            
DUMPFIL  DC    X'00'                                                            
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
RECLEN   DS    F                                                                
REC      DS    8000C                                                            
         ORG   REC                                                              
       ++INCLUDE DMRCVRHDR                                                      
         ORG                                                                    
         EJECT                                                                  
OUT1     DCB   DDNAME=OUT1,            DOS SYS005                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04000,                                            X        
               BLKSIZE=23476,                                          X        
               MACRF=PM                                                         
*                                                                               
OUT2     DCB   DDNAME=OUT2,            DOS SYS006                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04000,                                            X        
               BLKSIZE=23476,                                          X        
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
**PAN#1  DC    CL21'003SP131A    05/01/02'                                      
         END                                                                    
