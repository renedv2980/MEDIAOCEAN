*          DATA SET RE131A     AT LEVEL 003 AS OF 05/01/02                      
*PHASE RE131A                                                                   
*INCLUDE PRINT110                                                               
*INCLUDE PRINT                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMSECHK                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
         TITLE 'RE131 - REP RECOVERY FILE DUMP'                                 
********************************************************************            
* THIS MODULE READS THE DISK RECOVERY FILE AND GENERATES TWO TAPES.*            
*                                                                  *            
* ALL REPDIR RECORDS ARE IGNORED EXCEPT IN TOTAL INPUT COUNT       *            
*                                                                  *            
* TAPES HAVE ALL RECORDS EXCEPT POINTER COPIES AND CHANGES.        *            
*        (THESE RECORDS HAVE X'80' ON IN RRECTYPE)                 *            
********************************************************************            
* HISTORY OF CHANGES                                               *            
********************************************************************            
* NOV09/94 (BU ) --- 'SOFT' SYSTEM ASSIGNMENT                      *            
*                                                                  *            
* JUN09/95 (BU ) --- DDSIO CARD AND ERASE CARD                     *            
*                                                                  *            
* SEP/97   (MHER) -- DUMP DIRECTORY POINTERS TO OUTPUT TAPES       *            
*                                                                  *            
* MAR11/98 (BU ) --- MODIFY RECORD SIZE FOR 4K BLOCKS              *            
*                                                                  *            
* AUG24/98 (RCRI) -- TALK TO DATASPACE TO SEE IF OK TO ERASE       *            
*                 -- USE SYSLIST REC TO GET SE IF REPX INPUT       *            
*                    *** END TOMBSTONE ***                         *            
********************************************************************            
         SPACE 2                                                                
RE131    CSECT                                                                  
         PRINT NOGEN                                                            
       ++INCLUDE DMGREQUS                                                       
         NBASE 0,*RE131*,=A(WORK)                                               
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     R1,X'10'            GET CPU ID                                   
         L     R1,X'C4'(R1)                                                     
         MVC   CPUID,X'10'(R1)     CPU IS C'XXXX' FROM SMCASID                  
         MVC   TITLE(3),CPUID                                                   
         MVI   TITLE+4,C'-'                                                     
         MVC   TITLE+6(25),=CL25'REP. RECOVERY FILE DUMP'                       
*                                                                               
CARD0010 EQU   *                                                                
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         CLC   =C'/*',CARD         END OF FILE?                                 
         BE    CARDX               NO  - PROCEED                                
*                                                                               
         CLC   =C'ID=',CARD        ID=USER CARD?                                
         BNE   CARD0020            NO  - GET NEXT CARD                          
         MVC   SAVNAME,CARD+3      SAVE USER NAME                               
         CLI   DUMPFIL,C' '        CANT HAVE REPX ALSO                          
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETID            GET REP SE NUMBER FROM USER ID               
         B     CARD0010                                                         
*                                                                               
CARD0020 EQU   *                                                                
         CLC   =C'DDSIO=',CARD     DDSIO CARD                                   
         BNE   CARD0030            NO  - TEST NEXT                              
         ICM   RF,15,VDDSIO        SET ALTERNATE DDSIO                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(8,RF),CARD+6                                                   
         B     CARD0010            GET NEXT CARD                                
*                                                                               
CARD0030 EQU   *                                                                
         CLC   =C'ERASE=Y',CARD    ERASE=Y/N                                    
         BNE   *+12                                                             
         MVI   ERASESW,C'Y'                                                     
         B     CARD0010                                                         
         CLC   =C'ERASE=N',CARD                                                 
         BNE   *+12                                                             
         MVI   ERASESW,C'N'                                                     
         B     CARD0010                                                         
*                                                                               
CARD0040 EQU   *                                                                
         CLC   =C'GLOBAL=',CARD                                                 
         BNE   CARD0050                                                         
         MVC   GLOBAL,CARD+7       SET GLOBAL/LOCAL FILE FLAG                   
         B     CARD0010                                                         
*                                                                               
CARD0050 EQU   *                                                                
         CLC   =C'OVERRIDE',CARD                                                
         BE    *+14                                                             
         CLC   =C'FORCE=Y',CARD                                                 
         BNE   CARD0060                                                         
         MVI   OVERRIDE,C'Y'       SET OVERRIDE CPUID/DSPID CHECKS              
         B     CARD0010                                                         
*                                                                               
CARD0060 CLC   =C'REP',CARD        IDENTIFY REP SYSTEM BY REPX                  
         BNE   CARD0070                                                         
         MVC   TITLE+6(4),CARD     MOVE TO TITLE                                
         CLI   SAVNAME,C' '        CANT HAVE ID=.... ALSO                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DUMPSYS,X'08'       SET SYSTEM TYPE FOR REP                      
         MVC   DUMPFIL,CARD+3      SET FILE NUMBER                              
         MVC   INPSYS,CARD                                                      
         BAS   RE,GETSE            GET REP SE NUMBER FROM SYS LETTER            
         B     CARD0010                                                         
*                                                                               
CARD0070 EQU   *                                                                
         B     CRDERR              INVALID PARAMETER CARD                       
*                                                                               
CARDX    CLI   DUMPSYS,0           TEST IF WE HAVE SET SE NUMBER                
         BE    SYSERR                                                           
         EJECT                                                                  
* OPEN RECOVERY FILE                                                            
*                                                                               
OPEN     LA    R0,X'84'            SET AND SAVE RECOVERY FILE NUMBER            
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
*NOP*    GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'REP',FLIST,REC                    
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
         L     RE,16(R1)           POINT TO RETURNED FILE/SYSTEM INFO           
         MVC   FILERETN,0(RE)                                                   
         CLI   SENAME,C' '         MOVE SE NAME TO TITLE                        
         BNH   *+10                                                             
         MVC   TITLE+6(4),SENAME                                                
         CLI   RESULT,2            0=OK,1=OK,2=IGNORE,3=CANCEL                  
         BL    OPEN18X                                                          
         BH    END0                OPERATOR REQUESTED CANCEL                    
         MVC   P(L'MSG),MSG                                                     
         GOTO1 =V(PRINTER)         OPERATOR IGNORED ERROR                       
         B     OPEN20                                                           
OPEN18X  L     RF,=V(DMSECHKS)     SAVE SYSTEM LOCK INFO                        
         MVC   FILELOCK,0(RF)                                                   
         SPACE 1                                                                
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
         EJECT                                                                  
* READ RECOVERY FILE SEQUENTIALLY                                               
*                                                                               
GET1     EQU   *                                                                
         GOTO1 =V(DATAMGR),DMCB,(X'00',=C'DMRSEQ'),=C'REPRCV',         X        
               DA,REC,A(BUFF)                                                   
         TM    8(R1),X'40'         TEST ERROR                                   
         BNZ   GET1X                                                            
         TM    8(R1),X'80'         TEST EOF                                     
         BNZ   END1                                                             
         AP    TRECSIN+14(4),=P'1'                                              
         SPACE 1                                                                
* TEST FOR DIRECTORY OVERLAY *                                                  
         SPACE 1                                                                
         CLI   RFILTY,X'81'        TEST REPDIR                                  
         BNE   GET10                                                            
         TM    RECKEY,X'80'        TEST PASSIVE POINTER                         
         BO    GET20               YES - DUMP IT                                
         CLI   RRECTY,X'01'        SAVE COPY                                    
         BNE   GET2                                                             
         MVC   SAVEREC,RECKEY      SAVE DIRECTORY RECORD                        
         B     GET20               DUMP DIRECTORY COPY                          
*                                                                               
GET2     DS    0H                                                               
         CLI   RRECTY,X'02'        FOR CHANGES                                  
         BNE   GET20                                                            
         CLC   SAVEKEY,RECKEY      TEST SAME KEY                                
         BNE   GET20                                                            
         CLC   SAVEADDR,RECADDR    TEST SAME DA                                 
         BE    GET20                                                            
         CLI   SAVECNTL,X'FF'      FF DELETES DO NOT CAUSE                      
         BE    GET20               DIRECTORY OVERLAYS                           
         AP    DIRRECS+14(4),=P'1'                                              
         B     GET20                                                            
*                                                                               
GET10    LA    R3,CTRS                                                          
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
         CLC   RFILTY(2),0(R3)     MATCH FILE/REC                               
         BE    GET12                                                            
         BXLE  R3,R4,*-10                                                       
         B     GET14                                                            
*                                                                               
GET12    AP    14(4,R3),=P'1'                                                   
*                                                                               
GET14    TM    RRECTY,X'80'                                                     
         BNZ   GET1                SKIP POINTER COPIES/CHANGES                  
         SPACE 1                                                                
* WRITE RECORD TO TAPE1 AND TAPE2 *                                             
         SPACE 1                                                                
GET20    AP    TRECSOUT+14(4),=P'1'  BUMP OUTPUT COUNTER                        
*                                                                               
         LH    RE,DM5+2            GET REC LEN                                  
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,RECLEN                                                        
         LA    R0,RECLEN                                                        
         L     R1,=A(OUT1)                                                      
         PUT   (1),(0)                                                          
*                                                                               
         LA    R0,RECLEN                                                        
         L     R1,=A(OUT2)                                                      
         PUT   (1),(0)                                                          
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
* DO NOT ERASE RECOVERY FILE IF READ ERRORS OCCURED *                           
*                                                                               
ERASE    MVC   P(24),=C'XXXXXXXX FILE NOT ERASED'                               
         MVC   P(8),FILEDD                                                      
         CP    ERRORS,=P'0'        SKIP IF ERRORS                               
         BNE   ERASE2                                                           
         CLI   ERASESW,C'N'        SKIP IF ERASE=NO INPUT                       
         BE    ERASE2                                                           
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
ERASE4   GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         EJECT                                                                  
* PRINT COUNTERS                                                                
*                                                                               
PRTCNT   MVC   P(8),FILEDD                                                      
         MVC   P+9(24),=C'RECOVERY RECORD COUNTERS'                             
         GOTO1 =V(PRINTER)                                                      
         LA    R3,CTRS                                                          
         LH    R4,0(R3)                                                         
         LA    R5,TCTRSX-1         SET TO INCLUDE TOTALS                        
         LA    R3,6(R3)                                                         
*                                                                               
PRTCNT1  MVC   P,SPACES                                                         
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
         MVC   P(12),2(R3)                                                      
         OI    17(R3),X'0F'                                                     
         UNPK  P+14(6),14(4,R3)                                                 
         GOTO1 =V(PRINTER)                                                      
         BXLE  R3,R4,PRTCNT1                                                    
*                                                                               
EOJ      XBASE                                                                  
*                                                                               
CRDERR   MVC   P(31),=C'RE131 ** INVALID PARAMETER CARD'                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
*                                                                               
SYSERR   MVC   P(30),=C'RE131 ** NO VALID SYSTEM INPUT'                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(30,P)                                
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
         EJECT                                                                  
***********************************************************************         
* GET SE NUMBER FROM USER ID SAVNAME                                  *         
***********************************************************************         
         SPACE 1                                                                
GETID    NTR1  ,                                                                
         L     RE,=V(UTL)                                                       
         MVI   4(RE),X'0A'         SET SYSTEM NUMBER FOR CONTROL                
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',AREC,0                                             
         XC    KEYAREA,KEYAREA                                                  
         MVI   KEYAREA,C'I'        FIND CONTROL FILE ID RECORD                  
         MVC   KEYAREA+15(10),SAVNAME                                           
         OC    KEYAREA+15(10),SPACES                                            
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEYAREA,AREC              
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AREC                                                          
         CLC   KEYAREA(25),0(R1)   CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
GETID10  EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   GETID20             NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    GETID30             YES                                          
GETID20  EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   GETID10             NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
GETID30  EQU   *                                                                
         L     RE,=V(UTL)          OVERRIDE UTL NUMBER                          
         MVC   4(1,RE),3(R1)                                                    
         MVI   DUMPSYS,X'08'       SHOW THAT WE FOUND IT                        
GETIDX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GET SE NUMBER FOR REP FILE FROM SYSTEM LETTER IN DUMPSYS            *         
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
DIRRECS  DC    X'8103',CL12'REPDIR OVLYS',PL4'0'                                
         DC    X'8203',CL12'REPFILE ADDS',PL4'0'                                
         DC    X'8201',CL12'REPFILE CPYS',PL4'0'                                
         DC    X'8202',CL12'REPFILE CHGS',PL4'0'                                
         DC    X'8281',CL12'REPF PTR CPY',PL4'0'                                
         DC    X'8301',CL12'REPREQ  CPYS',PL4'0'                                
         DC    X'8302',CL12'REPREQ  CHGS',PL4'0'                                
         DC    X'8303',CL12'REPREQ  ADDS',PL4'0'                                
CTRSX    EQU   *                                                                
*                                                                               
TRECSIN  DC    X'0000',CL12'RECOVERY IN ',PL4'0'                                
TRECSOUT DC    X'0000',CL12'RECOVERY OUT',PL4'0'                                
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
AREC     DC    A(REC)                                                           
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
FLIST    DC    CL8' REPRCV ',CL8'X'                                             
SAVNAME  DC    CL10' '                                                          
         DC    C'**KEYA**'                                                      
KEYAREA  DS    CL25                                                             
*                                                                               
SAVEREC  DS    0CL32                                                            
SAVEKEY  DS    CL27                                                             
SAVECNTL DS    CL1                                                              
SAVEADDR DS    CL4                                                              
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
FILERETN DS    0XL16               FILE INFO RETURNED FROM V(DMSECHK)           
FILEINFO DS    0XL16                                                            
         DS    X                   RESERVED                                     
SENUM    DS    X                   SE NUMBER                                    
SENAME   DC    CL4' '              SE SHORT NAME                                
SEFLAG1  DS    X                   SE FLAGS                                     
         DS    XL1                 N/D                                          
FILNUM   DS    X                   FILE NUMBER                                  
FILFLAG1 DS    X                   FILE FLAGS                                   
FILFLAG2 DS    X                   FILE FLAGS                                   
         DS    X                   N/D                                          
FILADTF  DS    AL4                 FILE A(DTF)                                  
*                                                                               
ERSPARS  DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(ERSPAR6)                                                       
ERSPAR6  DC    A(0)                ** ERASE WHOLE FILE  **                      
*                                                                               
VDDSIO   DC    V(DDSIO)                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
RECLEN   DS    F                                                                
REC      DS    8000C                                                            
         ORG   REC                                                              
       ++INCLUDE DMRCVRHDR                                                      
RECKEY   DS    XL27                                                             
RECCNTL  DS    XL1                                                              
RECADDR  DS    XL4                                                              
         ORG                                                                    
         EJECT                                                                  
OUT1     DCB   DDNAME=OUT1,                                            X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04048,BLKSIZE=26364,                              X        
               MACRF=PM                                                         
*                                                                               
OUT2     DCB   DDNAME=OUT2,                                            X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04048,BLKSIZE=26364,                              X        
               MACRF=PM                                                         
*                                                                               
BUFF     DS    64000C              BUFFER FOR FULL TRACK READ                   
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
SSB      CSECT                                                                  
         DC    XL2'00',X'FF',XL5'00',XL248'00'                                  
*                                                                               
UTL      CSECT                                                                  
         DC    256X'00'            SE FILLED IN VIA CODE                        
*                                                                               
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
**PAN#1  DC    CL21'003RE131A    05/01/02'                                      
         END                                                                    
