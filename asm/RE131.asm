*          DATA SET RE131      AT LEVEL 012 AS OF 01/06/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044892.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE RE131A                                                                   
*INCLUDE PRINT110                                                               
*INCLUDE PRINT                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
         TITLE 'RE131 - REP recovery file dump'                                 
********************************************************************            
* This module reads the disk recovery file and gererates two tapes.*            
*                                                                  *            
* All REPDIR records are ignored except in total input counts      *            
*                                                                  *            
* Tapes have all records except pointer copies and changes.        *            
*        (These records have X'80' on in RRECTYPE)                 *            
********************************************************************            
*                                                                  *            
* History of changes                                               *            
********************************************************************            
* NOV09/94 (BUHR) -- 'SOFT' SYSTEM ASSIGNMENT                      *            
*                                                                  *            
* JUN09/95 (BUHR) -- DDSIO CARD AND ERASE CARD                     *            
*                                                                  *            
* SEP/97   (MHER) -- DUMP DIRECTORY POINTERS TO OUTPUT TAPES       *            
*                                                                  *            
* MAR11/98 (BUHR) -- MODIFY RECORD SIZE FOR 4K BLOCKS              *            
*                                                                  *            
* MAR16/10 (AHYD) -- Add SYS= or S= (Will remove ID= next time.    *            
*                                                                  *            
*                    *** END TOMBSTONE ***                         *            
********************************************************************            
         USING DPRINT,RA                                                        
RE131    CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
                                                                                
         PRINT NOGEN                                                            
         NBASE 0,*RE131*,=A(CHAIN)                                              
         L     RA,=V(CPRINT)                                                    
         BRAS  RE,SYSIN            Process SYSIN cards                          
         BNE   ERROUT                                                           
         BRAS  RE,OPENFILE         Open the files                               
         BNE   ERROUT                                                           
         BRAS  RE,GETEM                                                         
         BRAS  RE,ENDIT                                                         
         XBASE                                                                  
                                                                                
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* Some error occured print it out                                               
***********************************************************************         
ERROUT   WTO   'Check SYSPRINT for ERRORS'                                      
         LLC   RF,ERR#                                                          
         CLI   ERR#,ERRMAXQ                                                     
         BNH   *+6                                                              
         DC    H'00'                                                            
                                                                                
         BCTR  RF,0                                                             
         MHI   RF,L'ERRTAB                                                      
         LA    RF,ERRTAB(RF)                                                    
         MVC   P(L'ERRTAB),0(RF)                                                
         GOTO1 PRINTER                                                          
                                                                                
         MVC   P(80),CARD                                                       
         GOTO1 PRINTER                                                          
                                                                                
ERRXIT   J     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* Table of errors to print                                                      
***********************************************************************         
ERRTAB   DC    CL35'ERROR *Bad input card*'                                     
         DC    CL35'ERROR *Missing SYS= or S= or ID=*'                          
         DC    CL35'ERROR *ERASE= missing'                                      
         DC    CL35'ERROR *Can not have SYS= and ID='                           
         DC    CL35'ERROR *Failed to open tape files*'                          
         DC    CL35'ERROR *Invalid System*'                                     
ERRMAXQ  EQU   ((*-ERRTAB)/L'ERRTAB)                                            
                                                                                
ERRNONE  EQU   0                                                                
ERRBAD   EQU   1                                                                
ERRNOREP EQU   2                                                                
ERRNOERS EQU   3                                                                
ERRGOTIT EQU   4                                                                
ERROPEN  EQU   5                                                                
ERRIVSYS EQU   6                                                                
         EJECT ,                                                                
********************************************************************            
* Read SYSIN cards and process                                                  
********************************************************************            
SYSIN    NTR1                                                                   
                                                                                
SYSIN10  GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   =C'/*',CARD         End of SYSIN ?                               
         BE    SYSINXIT            Yes, now done                                
         CLI   CARD,C'*'           Comment                                      
         BE    SYSIN88                                                          
                                                                                
         CLC   =C'ID=',CARD        ID=USER CARD?                                
         BNE   SYSIN20             NO  - GET NEXT CARD                          
         CLI   HAVEREP,NO                                                       
         BE    SYSIN12                                                          
         MVI   ERR#,ERRGOTIT                                                    
         B     SYSINXIT                                                         
                                                                                
SYSIN12  MVC   SAVNAME,CARD+3      SAVE USER NAME                               
         LA    RF,REC                                                           
         ST    RF,AREC                                                          
         BRAS  RE,GETID                                                         
         BNE   SYSINXIT                                                         
         MVI   HAVEREP,MAYBE       For now                                      
         B     SYSIN80             GET NEXT CARD                                
*                                                                               
SYSIN20  CLC   =C'DDSIO=',CARD     DDSIO CARD                                   
         BNE   SYSIN25             NO  - TEST NEXT                              
         ICM   RF,15,VDDSIO        SET ALTERNATE DDSIO                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(8,RF),CARD+6                                                   
         B     SYSIN80                                                          
*                                                                               
         USING SSBOFFD,RF                                                       
SYSIN25  CLC   =C'DSPACE=',CARD    DDSIO CARD                                   
         BNE   SYSIN30             NO  - TEST NEXT                              
         ICM   RF,15,=V(SSB)       SET ALTERNATE DDSIO                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SSODSPAC,CARD+7     Move in DSPACE                               
         CLI   SSODSPAC,C'T'                                                    
         BE    SYSIN80                                                          
         CLI   SSODSPAC,C'R'                                                    
         BE    SYSIN80                                                          
         CLI   SSODSPAC,C'A'                                                    
         BE    SYSIN80                                                          
         CLI   SSODSPAC,C'C'                                                    
         BE    SYSIN80                                                          
         CLI   SSODSPAC,C'Q'                                                    
         BNE   SYSIN78                                                          
         B     SYSIN80                                                          
         DROP  RF                                                               
*                                                                               
SYSIN30  CLC   =C'ERASE=',CARD     ERASE CARD                                   
         BNE   SYSIN40             NO  - GET NEXT CARD                          
         MVI   ERASESW,YES                                                      
         MVC   ERASEME,CARD+6      SAVE ERASE NAME                              
         CLC   =C'NO',ERASEME                                                   
         BE    SYSIN80             Okay then                                    
         CLC   =C'YES',ERASEME                                                  
         BNE   SYSIN78             Bad card                                     
         MVI   FLIST,C'U'          Read/Write                                   
         B     SYSIN80                                                          
*                                                                               
SYSIN40  CLC   =C'REP',CARD        See if REPxx                                 
         BNE   SYSIN42                                                          
         MVC   SVSYS(5),CARD       Save of REPxx                                
         MVC   CARD(4),=C'SYS='                                                 
         MVC   CARD+4(5),SVSYS     Build SYS=REPxx                              
         B     SYSIN46                                                          
*                                                                               
SYSIN42  CLC   =C'SYS=',CARD                                                    
         BE    SYSIN46                                                          
         CLC   =C'S=',CARD                                                      
         BNE   SYSIN78                                                          
*                                                                               
SYSIN46  GOTOR DMDDNAME,DMCB,(X'24',=C'DDNAME'),CARD,0                          
         CLI   8(R1),0                                                          
         BE    SYSIN48                                                          
         MVI   ERR#,ERRIVSYS                                                    
         B     SYSINXIT                                                         
                                                                                
SYSIN48  L     RF,8(,R1)           Get A(FILE INFO LIST)                        
         MVC   DDNADATA,0(RF)      Move block in                                
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),DDNASENO    Extract SENUM                                
         MVC   ADTF,DDNAADTF       Save off DTF                                 
         MVC   FILE#,DDNAFINO                                                   
         CLI   FILE#,X'84'         Check is right                               
         BE    *+6                                                              
         DC    H'00'               Ut-oh                                        
                                                                                
         MVI   HAVEREP,YES                                                      
         B     SYSIN80                                                          
                                                                                
SYSIN78  MVI   ERR#,ERRBAD         Unknown card                                 
         B     SYSINXIT                                                         
*                                                                               
SYSIN80  MVC   P(80),CARD          Print good card                              
         MVC   P+36(4),=C'OKAY'                                                 
         GOTO1 PRINTER                                                          
SYSIN88  B     SYSIN10             Get next card                                
*                                                                               
SYSINXIT CLI   ERR#,ERRNONE        Was this set already                         
         J     XIT                 Yes                                          
         MVI   ERR#,ERRNOREP       Set no REP present                           
         CLI   HAVEREP,NO                                                       
         BE    SYSINXIT                                                         
         MVI   ERR#,ERRNOERS       Set no ERASE= present                        
         CLI   ERASESW,YES                                                      
         BNE   SYSINXIT                                                         
         MVI   ERR#,ERRNONE        Set all okay for now                         
                                                                                
         CLI   ERR#,ERRNONE        Set CC                                       
         J     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* OPEN recovery file                                                            
***********************************************************************         
OPENFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',FLIST,REC                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   HAVEREP,MAYBE       GetID call                                   
         BNE   OPEN10                                                           
         WTO   'Change SYSIN cards to have REP# or SYS=REP# or S=R#'            
         B     OPEN20                                                           
*                                                                               
OPEN10   GOTOR =V(DYNALLOC),DMCB,(C'D',DDNADDN),WORK                            
         XR    RF,RF                                                            
         ICM   RF,1,4(R1)                                                       
         BNZ   *+6                                                              
         DC    H'00'                                                            
         MVC   P(18),=C'Recovery file DSN='                                     
         BCTR  RF,0                                                             
         MVC   P+18(0),WORK                                                     
         EX    RF,*-6                                                           
         GOTOR PRINTER                                                          
         GOTOR PRINTER                                                          
*                                                                               
OPEN20   OPEN  (RCVTAPE,(OUTPUT),RCVCOPY,(OUTPUT))                              
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         MVI   ERR#,ERROPEN                                                     
         MVC   TITLE(30),=CL30'REP RECOVERY FILE DUMP'                          
         CLI   ERR#,ERRNONE                                                     
         J     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*   Open control system to access control file identification                   
***********************************************************************         
GETID    NTR1                                                                   
         CLI   HAVEREP,NO                                                       
         BE    GETID02                                                          
         MVI   ERR#,ERRGOTIT                                                    
         B     GETIDXIT                                                         
                                                                                
GETID02  L     RE,=V(UTL)                                                       
         MVI   4(RE),X'0A'         SET SYSTEM NUMBER FOR CONTROL                
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',AREC,0                                             
         XC    KEYAREA,KEYAREA                                                  
         MVI   KEYAREA,C'I'        FIND CONTROL FILE ID RECORD                  
         MVC   KEYAREA+15(10),SAVNAME   LOAD AGENCY NAME                        
         OC    KEYAREA+15(10),SPACES    SET REMAINDER TO SPACES                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEYAREA,AREC                  
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
                                                                                
         L     R1,AREC                                                          
         CLC   KEYAREA(25),0(R1)   CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
                                                                                
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
GETID10  CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   GETID20             NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    GETID30             YES                                          
                                                                                
GETID20  LLC   R0,1(,R1)           BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD                                
         BNE   GETID10             NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
*                                                                               
GETID30  L     RE,=V(UTL)          OVERRIDE UTL NUMBER                          
         MVC   4(1,RE),3(R1)       Set ULT with this system                     
                                                                                
GETIDXIT CLI   ERR#,ERRNONE                                                     
         J     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* Start reading recovery records and process and put to tape(s)                 
***********************************************************************         
GETEM    NTR1                                                                   
GET1     GOTO1 DATAMGR,DMCB,(X'11',=C'DMRSEQ'),=C'REPRCV',             +        
               DA,REC,A(BUFF)                                                   
         TM    8(R1),X'40'         TEST ERROR                                   
         BO    ERRTRACK                                                         
         TM    8(R1),X'80'         TEST EOF                                     
         BO    GETXIT                                                           
         AP    TRECSIN+14(4),=P'1'                                              
*                                  TEST FOR DIRECTORY OVERLAY *                 
         CLI   RFILTY,X'81'        TEST REPDIR                                  
         BNE   GET10                                                            
         TM    RECKEY,X'80'        TEST PASSIVE POINTER                         
         BO    PUTTAPE             YES - DUMP IT                                
         CLI   RRECTY,X'01'        SAVE COPY                                    
         BNE   GET2                                                             
         MVC   SAVEREC,RECKEY      SAVE DIRECTORY RECORD                        
         B     PUTTAPE             DUMP DIRECTORY COPY                          
*                                                                               
GET2     CLI   RRECTY,X'02'        FOR CHANGES                                  
         BNE   PUTTAPE                                                          
         CLC   SAVEKEY,RECKEY      TEST SAME KEY                                
         BNE   PUTTAPE                                                          
         CLC   SAVEADDR,RECADDR    TEST SAME DA                                 
         BE    PUTTAPE                                                          
         CLI   SAVECNTL,X'FF'      FF DELETES DO NOT CAUSE                      
         BE    PUTTAPE             DIRECTORY OVERLAYS                           
         AP    DIRRECS+14(4),=P'1'                                              
         B     PUTTAPE                                                          
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
GET14    TM    RRECTY,X'80'        SKIP POINTER COPIES/CHANGES                  
         BO    GET1                                                             
         CLI   RSIN,X'FF'          SKIP DELETED RECORDS                         
         BE    GET1                                                             
         B     PUTTAPE                                                          
                                                                                
GETXIT   J     XIT                                                              
***********************************************************************         
* Write record out to TAPE1 and TAPE 2                                          
***********************************************************************         
PUTTAPE  AP    TRECSOUT+14(4),=P'1'  BUMP OUTPUT COUNTER                        
*                                                                               
         LH    RE,DM5+2            GET REC LEN                                  
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         ST    RE,RECLEN                                                        
         LA    R0,RECLEN                                                        
         LARL  R1,RCVTAPE                                                       
         PUT   (1),(0)                                                          
*                                                                               
         LA    R0,RECLEN                                                        
         LARL  R1,RCVCOPY                                                       
         PUT   (1),(0)                                                          
         B     GET1                                                             
                                                                                
***********************************************************************         
* Bump disk address to start of next track                                      
***********************************************************************         
ERRTRACK AP    ERRORS,=P'1'                                                     
         L     RE,DA               16 or 20 bit?                                
                                                                                
         USING DTFPHD,RF                                                        
         ICM   RF,15,ADTF                                                       
         BZ    ERRTRK10                                                         
         TM    DTFTYPE,DTFTBIGF    20 bit                                       
         BO    ERRTRK20                                                         
         DROP  RF                                                               
                                                                                
ERRTRK10 SRL   RE,16                                                            
         AHI   RE,1                Assume 16 bit then if no DTF                 
         SLL   RE,16                                                            
         B     ERRTRK90                                                         
                                                                                
ERRTRK20 SRL   RE,12               TTTTT000 --> 000TTTTT                        
         AHI   RE,1                                                             
         SLL   RE,12               000TTTTT --> TTTTT000                        
                                                                                
ERRTRK90 ST    RE,DA                                                            
         OI    DA+2,X'01'          TTTTT100 or  TTTT0100                        
*                                  doesn't seem to mater                        
         B     GET1                                                             
         EJECT ,                                                                
***********************************************************************         
* Finish up and close output files check errors                                 
***********************************************************************         
ENDIT    NTR1                                                                   
         CP    TRECSIN+14(4),=P'1' TEST ANY RECORDS                             
         BL    ENDX                                                             
*                                                                               
         CLOSE (RCVTAPE,,RCVCOPY,)                                              
         CP    ERRORS,=P'0'        CHECK FOR ERRORS                             
         BE    ENDX                NO - GO ON                                   
*                                                                               
         OI    ERRORS+3,X'0F'                                                   
         UNPK  ERRMSG(4),ERRORS                                                 
         GOTO1 LOGIO,DMCB,(X'FF',1),(69,ERRMSG)                                 
                                                                                
         MVC   P(69),ERRMSG                                                     
         GOTO1 PRINTER                                                          
                                                                                
***********************************************************************         
* Finished dumpping recovery records print records counts                       
***********************************************************************         
ENDX     MVC   P(5),CARD                                                        
         MVC   P+6(24),=C'RECOVERY RECORD COUNTERS'                             
         GOTO1 PRINTER                                                          
         GOTO1 PRINTER             SKIP A LINE                                  
                                                                                
         LA    R3,CTRS                                                          
         LH    R4,0(R3)                                                         
         LA    R5,TCTRSX-1         SET TO INCLUDE TOTALS                        
         LA    R3,6(R3)                                                         
*                                                                               
PRTCNT   MVC   P,SPACES                                                         
         GOTO1 PRINTER             SKIP A LINE                                  
         MVC   P(12),2(R3)                                                      
         OI    17(R3),X'0F'                                                     
         UNPK  P+14(6),14(4,R3)                                                 
         GOTO1 PRINTER                                                          
         BXLE  R3,R4,PRTCNT                                                     
         EJECT                                                                  
***********************************************************************         
* Do not erase recovery file if read errors occured                             
***********************************************************************         
         CP    ERRORS,=P'0'                                                     
         BNE   PREEOJ                                                           
         CLC   =C'NO ',ERASEME     ERASE THE FILE?                              
         BE    PREEOJ              NO  - SKIP IT                                
         MVC   P+1(17),=C'FILE TO BE ERASED'                                    
         GOTO1 PRINTER                                                          
                                                                                
***********************************************************************         
* GET RECOVERY FILE DTF ADDRESS *                                               
***********************************************************************         
* Should check for concurrent file update or SECHK for globals                  
***********************************************************************         
         GOTO1 =V(DMOD000),ERSPARS,A(DMEXT),,,(X'84',0)                         
                                                                                
***********************************************************************         
* CALL DADDS TO ERASE FILE *                                                    
***********************************************************************         
         GOTO1 DADDS,ERSPARS,A(WTERASE)                                         
         B     XIT                                                              
*                                                                               
PREEOJ   EQU   *                                                                
         MVC   P+1(17),=C'FILE !NOT! ERASED'                                    
         GOTO1 PRINTER                                                          
         J     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
* Tables of records types                                                       
***********************************************************************         
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
MAYBE    EQU   C'M'                                                             
                                                                                
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
*                                                                               
ERRMSG   DC    CL30'0000 RECOVERY FILE READ ERRORS'                             
         DC    X'15'                                                            
         DC    CL37'********** FILE NOT ERASED **********'                      
         DC    X'15'                                                            
ERRORS   DC    PL4'0'                                                           
         EJECT                                                                  
***********************************************************************         
* Other constants and variables                                                 
***********************************************************************         
DATAMGR  DC    V(DATAMGR)                                                       
DADDS    DC    V(DADDS)                                                         
DMDDNAME DC    V(DMDDNAME)                                                      
CARDS    DC    V(CARDS)                                                         
PRINTER  DC    V(PRINTER)                                                       
LOGIO    DC    V(LOGIO)                                                         
VDDSIO   DC    V(DDSIO)                                                         
                                                                                
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
FLIST    DC    CL8'NREPRCV '                                                    
         DC    C'X'                                                             
*                                                                               
ADTF     DC    A(0)                                                             
AREC     DS    A                                                                
SAVNAME  DS    CL10                                                             
ERASEME  DC    CL3' '                                                           
         DC    C'**KEYA**'                                                      
KEYAREA  DS    CL25                                                             
*                                                                               
SAVEREC  DS    0CL32                                                            
SAVEKEY  DS    CL27                                                             
SAVECNTL DS    CL1                                                              
SAVEADDR DS    CL4                                                              
*                                                                               
ERSPARS  DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(ERSPAR6)                                                       
ERSPAR6  DC    A(0)                ** ERASE WHOLE FILE  **                      
*                                                                               
WORK     DS    XL64                                                             
FILE#    DC    X'00'                                                            
ERR#     DC    AL1(ERRNONE)                                                     
HAVEREP  DC    AL1(NO)                                                          
ERASESW  DC    AL1(NO)                                                          
SESNUM   DC    AL1(0)                                                           
SVSYS    DC    CL5' '                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE DMDDNAMED                                                      
*                                                                               
RECLEN   DS    F                                                                
REC      DS    4100C                                                            
         ORG   REC                                                              
       ++INCLUDE DMRCVRHDR                                                      
RECKEY   DS    XL27                                                             
RECCNTL  DS    XL1                                                              
RECADDR  DS    XL4                                                              
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* Out put files to dump to                                                      
***********************************************************************         
RCVTAPE  DCB   DDNAME=OUT1,                                            +        
               DSORG=PS,                                               +        
               RECFM=VB,                                               +        
               LRECL=08200,BLKSIZE=27648,                              +        
               MACRF=PM                                                         
*                                                                               
RCVCOPY  DCB   DDNAME=OUT2,                                            +        
               DSORG=PS,                                               +        
               RECFM=VB,                                               +        
               LRECL=08200,BLKSIZE=27648,                              +        
               MACRF=PM                                                         
*                                                                               
BUFF     DS    64000C              BUFFER FOR FULL TRACK READ                   
         EJECT                                                                  
***********************************************************************         
* PRINT area                                                                    
***********************************************************************         
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
***********************************************************************         
* SSB extended                                                                  
***********************************************************************         
SSB      CSECT                                                                  
         DC    X'0000FF',X'00',XL252'00'                                        
                                                                                
***********************************************************************         
* UTL                                                                           
***********************************************************************         
UTL      CSECT                                                                  
         DC    256X'00'                                                         
                                                                                
***********************************************************************         
* Working storage                                                               
***********************************************************************         
CHAIN    CSECT                                                                  
         DS    2000D                                                            
***********************************************************************         
* DMGR equates                                                                  
***********************************************************************         
       ++INCLUDE DMGREQUS                                                       
                                                                                
       ++INCLUDE DMDTFPH                                                        
                                                                                
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012RE131     01/06/15'                                      
         END                                                                    
