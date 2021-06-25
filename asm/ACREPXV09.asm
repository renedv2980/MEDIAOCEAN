*          DATA SET ACREPXV09  AT LEVEL 248 AS OF 08/19/03                      
*PHASE ACXV02B                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE '=TRANSFER PROF/MAINT REPORT ACROSS ALL FILES'                   
***********************************************************************         
*  SEARCH FOR ROW AND/OR COLUMN KEYWORDS AND                          *         
*  SEARCH FOR COLUMN FILTERS BY TYPE OF FILTER                        *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACXV02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXV**,R9                                                    
*                                                                               
         USING ACWORKD,RA                                                       
         USING ACXVD,RC                                                         
         L     RA,0(,R1)                                                        
         LA    RC,SPACEND                                                       
         EJECT ,                                                                
ACXV02A  CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         ZAP   TOTRECS,=P'0'       SYSTEM     TOTAL     RECORDS                 
         ZAP   TOTFNDS,=P'0'       SYSTEM     TOTAL     FOUND                   
         ZAP   FTOTRECS,=P'0'      FULL       TOTAL     RECORDS                 
         ZAP   FTOTFNDS,=P'0'      FULL       TOTAL     FOUND                   
         ZAP   CPYFNDS,=P'0'       COMPANY    TOTAL     FOUND                   
         ZAP   CPYRECS,=P'0'       COMPANY    TOTAL     RECORDS                 
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   DUMPECNT,=P'0'                                                   
         ZAP   PDUMP,=P'0'                                                      
         XC    CPYCOUNT,CPYCOUNT                                                
         MVC   REQDATED,SPACES                                                  
         MVC   REQUEST#,SPACES                                                  
         MVI   FORCEHED,YES                                                     
         MVI   FCRESET,YES                                                      
*                                                                               
         USING MASTD,RE            MASTER     DSECT                             
         L     RE,ADMASTC          ->    MASTER    DSECT                        
         L     R2,MCUTL            ->    UTL                                    
         ST    R2,AUTL             SAVE  ADDR OF   UTL                          
         MVC   CTL#SE,4(R2)        SAVE  SE ID FOR CTL FILES                    
         MVC   UPSI,MCUPSI         SAVE  UPSI BYTE                              
         DROP  RE                                                               
*                                                                               
         MVI   FILTSEL,0                                                        
         GOTO1 HEXIN,DMCB,QSELECT,FILTSEL,2                                     
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   *+8                                                              
         MVI   FILTSEL,0                                                        
*                                                                               
         XC    TEXTMSG,TEXTMSG                                                  
         XC    RQSTART,RQSTART                                                  
         MVC   RQEND,=X'FFFFFF'                                                 
*                                                                               
         CLC   QSTART,SPACES                                                    
         BNH   DT010                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,RQSTART)                               
*                                                                               
DT010    CLC   QEND,SPACES                                                      
         BNH   DT020                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(1,RQEND)                                   
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CTWREC,R3                                                        
DT020    XC    CTKEY,CTKEY         CLEAR SYS  LIST REC  KEY                     
         LA    R3,CTKEY            ->    SYSTEM    LIST KEY                     
         MVI   CTWKTYP,CTWKTYPQ    REC   TYPE      C'W' SYSTEM LISTS            
         MVI   CTWKREC,CTWKRSYS    SUB   REC  TYPE C'S' SYSTEM LIST             
*&&US*&& MVI   CTWKSYSN,CTWKACC    SYSTEM NUMBER =  ACCPAK                      
*&&UK*&& MVI   CTWKSYSN,X'06'      SYSTEM NUMBER =  ACCPAK                      
         MVC   TKEY,CTKEY                                                       
         L     R3,AIO3                                                          
         BAS   RE,DMCTREAD         READ                                         
*                                                                               
         USING CTLSTD,R5           FILE INFO ELEMENT                            
         L     R3,AIO3             IO AREA                                      
         LA    R5,CTWDATA          ->   ELEMENTS                                
REQF20   CLI   0(R5),0             EOR ?                                        
         BE    REQF30              YES, GET NEXT RECORD                         
         CLI   CTLSTEL,CTLSTELQ    X'A4' ELEMENT ?                              
         BNE   REQF25              NO, GET NEXT ELEMENT                         
*&&US*&& CLI   CTLSTDTA+7,CTWKACC  ACCPAK LIST DATA ?                           
*&&UK*&& CLI   CTLSTDTA+7,X'06'    ACCPAK LIST DATA ?                           
         BNE   REQF25              NO, GET NEXT ELEMENT                         
*                                  YES, FOUND ACCPAK LIST DATA                  
         CLC   CTLSTDTA(4),=C'ACCT'     DON'T PROCESS ACCT                      
         BE    REQF25                                                           
         CLI   QSELECT+2,C' '      LIMIT THE  ACCPACK SYSTEMS ?                 
         BE    REQF22              NO,  CONTINUE                                
         CLC   QSELECT+2(1),CTLSTDTA+3                                          
         BNE   REQF25              NO, GET NEXT ELEMENT                         
*                                                                               
REQF22   MVC   ACC#SE,CTLSTDTA+8   SAVE SE NUMBER                               
         MVC   ACCSYSID,CTLSTDTA   SAVE ACCPAK SYSTEM ID                        
*                                                                               
*        L     R2,AUTL             ->    UTL                                    
*        MVC   4(1,R2),ACC#SE      SET   UTL  FOR  THIS ACCPAK    FILE          
         L     R2,AIO3                                                          
         BAS   RE,DMOPNACC         OPEN  ACC  FILE                              
         GOTO1 XFILES,1            ACROSS FILES, BUILD LIST OF COMPANY          
         BAS   RE,RS               GET   FIRST     ACCOUNT   RECORD             
*        L     R2,AUTL             ->    UTL                                    
*        MVC   4(1,R2),CTL#SE      RESTORE    SE   ID   FOR  CTL  FILES         
*                                                                               
REQF25   ZIC   R2,1(,R5)           CURR  ELEMENT   LENGTH                       
         AR    R5,R2               NEXT  ELEMENT   ADDR                         
         B     REQF20              TRY   NEXT ELEMENT                           
*                                                                               
REQF30   MVC   TKEY,CTKEY          RESTORE SAVED KEY                            
         GOTO1 XFILES,2            PRINT DATA ON FILES                          
         DROP  R3                                                               
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*        PROCESS THIS ACCFILE OF SCRIBE RECORDS                       *         
***********************************************************************         
         SPACE 1                                                                
         USING MPRRECD,R3                                                       
         USING PLINED,R6                                                        
RS       NTR1                                                                   
         LA    R6,P                                                             
         MVC   P+2(8),=C'ACC SYS='                                              
         MVC   P+10(4),ACCSYSID                                                 
         MVI   PRTFLAG,0                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R3,TKEY                                                          
         XC    MPRKEY,MPRKEY                                                    
         XC    PRVKEY,PRVKEY                                                    
         XC    PRVCPY,PRVCPY                                                    
         MVI   MPRKTYP,MPRKTYPQ    X'2F' MEDIA POSTING RULES                    
         MVI   MPRKSUB,MPRKSUBQ    X'01'                                        
*                                                                               
         L     R3,AIO1             READ FOR FIRST FORMAT ON FILE                
         BAS   RE,DMACHIGH                                                      
*                                                                               
LAST     USING MPRRECD,MYKEY                                                    
*                                                                               
RS10     L     R3,AIO1                                                          
         MVI   REREAD,NO                                                        
         CLC   0(2,R3),=X'2F01'    CHECK RECORD TYPE                            
         BNE   RS90                FINISHED                                     
         MVI   PRTFLAG,0                                                        
         CLC   MPRKCPY,LAST.MPRKCPY                                             
         BE    RS20                                                             
         MVI   PRTFLAG,PRTNCPY                                                  
         MVC   MYKEY,0(R3)         SAVE OFF KEY                                 
*                                                                               
RS20     NI    PRTFLAG,TURNOFF-PRTELM                                           
         GOTO1 HEXOUT,DMCB,MPRKCPY,PCPY,1                                       
*        CLC   MPRKCPY,LAST.MPRKCPY                                             
*        BE    RS20A                                                            
*        MVC   TEMPCPY,MPRKCPY                                                  
*        BAS   RE,GETCPY                                                        
*        MVC   TKEY,MYKEY          RESTORE KEY                                  
RS20A    MVC   PLOGO2,CLOGO                                                     
         MVC   PALPHA,MPRKALPH                                                  
         MVC   PSYS,MPRKSYS                                                     
         MVC   PMED,MPRKMED                                                     
         MVC   POFC,MPRKOFC                                                     
         MVC   PCLI,MPRKCLI                                                     
         MVC   PPRD,MPRKPRD                                                     
         LA    RF,POSTSC           TABLE OF POST MAINT SCREENS                  
RS20B    CLI   0(RF),X'FF'                                                      
         BE    RS20D                                                            
         CLC   MPRKPRO,0(RF)       MATCH ON NUMBER                              
         BE    RS20C                                                            
         LA    RF,8(RF)                                                         
         B     RS20B                                                            
RS20C    MVC   PPROF,1(RF)         PRINT OUT NAME                               
*                                                                               
RS20D    L     R3,AIO1                                                          
         LR    R4,R3                                                            
         AH    R4,DATADISP                                                      
RS30     CLI   0(R4),0                                                          
         BE    RS70                                                             
         MVI   DICTFORM,NO         ASSUME NOT  TRANSLATED                       
* DCUR - IF YOU WANT TO SEE IF A SPECIFIC PROFILE IS SET                        
*        CLI   0(R4),MBTELQ        X'2E'  MEDIA BILLING TRANSFER ELEM           
*        BE    MBT010                                                           
* DCUR - IF YOU WANT TO SEE IF A SPECIFIC PROFILE IS SET                        
         CLI   0(R4),MBTELQ        X'2E'  MEDIA BILLING TRANSFER ELEM           
         BE    MBT010                                                           
         CLI   0(R4),MTPELQ        X'2F'  MEDIA TRANSFER PROFILE ELEM           
         BE    MTP010                                                           
*                                                                               
NEXTELM  SR    R1,R1                                                            
         IC    R1,1(,R4)                                                        
         AR    R4,R1                                                            
         B     RS30                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RS70     L     R3,AIO1                                                          
         TM    PRTFLAG,PRTELM                                                   
         BZ    RS72                                                             
         GOTO1 ACREPORT                                                         
*                                                                               
RS72     DS    0H                                                               
*        BAS   RE,DMACHIGH         RE-ESTABLISH KEY SEQUENCE READ               
         BAS   RE,DMACSEQ                                                       
         B     RS10                                                             
         EJECT ,                                                                
***********************************************************************         
*  END OF ACC FILE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RS90     DS    0H                                                               
         TM    PRTFLAG,PRTELM                                                   
         BO    RS99                                                             
         GOTO1 ACREPORT                                                         
*                                                                               
RS99     DS    0H                                                               
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING MBTELD,R4                                                        
         USING PLINED,R6                                                        
MBT010   LA    R6,P                                                             
*        TM    PRTFLAG,PRTNCPY                                                  
*        BZ    MBT011                                                           
*        GOTO1 ACREPORT                                                         
*        NI    PRTFLAG,TURNOFF-PRTNCPY                                          
*                                                                               
MBT011   MVC   PTYPE,=C'RCV'                                                    
         CLI   MBTTYP,MBTTRCV                                                   
         BE    MBT011A                                                          
*        MVC   PTYPE,=C'INC'                                                    
*        CLI   MBTTYP,MBTTINC                                                   
*        BE    MBT011A                                                          
         MVC   PTYPE,SPACES                                                     
         MVC   PCPY,SPACES                                                      
         MVC   PALPHA,SPACES                                                    
         MVC   PSYS,SPACES                                                      
         MVC   PMED,SPACES                                                      
         MVC   POFC,SPACES                                                      
         MVC   PCLI,SPACES                                                      
         MVC   PPRD,SPACES                                                      
         B     NEXTELM                                                          
*                                                                               
MBT011A  DS    0H                                                               
*        CLI   MBTMEMOX,C'C'       ONLY WANT CERTAIN MEMO KEYWORDS              
*        BE    MBT011B                                                          
*        CLI   MBTMEMOX,C'Z'                                                    
*        BE    MBT011B                                                          
         OC    MBTMEMOX,MBTMEMOX   PRINT ANY MEMO KEYWORDS                      
         BNZ   MBT011B                                                          
         MVC   PCPY,SPACES                                                      
         MVC   PLOGO2,SPACES                                                    
         MVC   PALPHA,SPACES                                                    
         MVC   PTYPE,SPACES                                                     
         MVC   PSYS,SPACES                                                      
         MVC   PMED,SPACES                                                      
         MVC   POFC,SPACES                                                      
         MVC   PCLI,SPACES                                                      
         MVC   PPRD,SPACES                                                      
         B     NEXTELM                                                          
*                                                                               
MBT011B  CLC   MBTULA,SPACES                                                    
         BNH   *+10                                                             
         MVC   PULA,MBTULA                                                      
*        TM    MBTSTAT,MBTSAOR                                                  
*        BZ    *+10                                                             
*        MVC   PAOR,=C'AOR'                                                     
*                                                                               
         CLI   MBTLLN,MBTPDLNQ                                                  
         BE    MBT012                                                           
         CLI   MBTLLN,MBTPRLNQ                                                  
         BE    MBT020                                                           
         DC    H'00'                                                            
*                                                                               
MBT012   CLC   MBTOFFC,SPACES                                                   
         BNH   *+10                                                             
         MVC   POFF,MBTOFFC                                                     
         CLC   MBTCNTRA,SPACES                                                  
         BNH   *+10                                                             
         MVC   PCULA,MBTCNTRA                                                   
         OC    MBTPOST,MBTPOST                                                  
         BZ    MBT014                                                           
         EDIT  MBTPOST,PPOST$                                                   
*                                                                               
MBT014   OC    MBTMEMO,MBTMEMO                                                  
         BZ    MBT100                                                           
         EDIT  MBTMEMO,PMEMO$                                                   
         B     MBT100                                                           
*                                                                               
MBT020   DS    0H                                                               
*        MVC   PPERID,MBTPERID                                                  
         OC    MBTAMTX,MBTAMTX                                                  
         BZ    MBT022                                                           
         MVC   PAMTX,=C'AMT='                                                   
         MVC   PAMTEXP,MBTAMTX                                                  
*                                                                               
MBT022   OC    MBTMEMOX,MBTMEMOX                                                
         BZ    MBT100                                                           
         MVC   PMEMOX,=C'MEMO='                                                 
         MVC   PMEMOEXP,MBTMEMOX                                                
*                                                                               
MBT100   GOTO1 ACREPORT                                                         
         OI    PRTFLAG,PRTELM                                                   
         B     NEXTELM                                                          
         DROP  R4,R6                                                            
         EJECT ,                                                                
         USING MTPELD,R4                                                        
         USING CPYDATAD,R7                                                      
         USING PLINED,R6                                                        
MTP010   LA    R6,P                                                             
* DCUR - IF YOU WANT TO SEE IF A PARTICULAR PROFILE IS SET                      
*        CLI   MTPFNUM,MTPFDACC                                                 
*        BNE   NEXTELM                                                          
* DCUR - IF YOU WANT TO SEE IF A PARTICULAR PROFILE IS SET                      
         CLI   MTPFNUM,MTPFDACC                                                 
         BNE   NEXTELM                                                          
*                                                                               
         ICM   R5,15,CPYCOUNT                                                   
         L     R7,=A(CPYLIST)                                                   
MTP020   CLC   CPYACC,ACCSYSID                                                  
         BNE   MTP030                                                           
         CLC   CPYHEX,MPRKCPY                                                   
         BNE   MTP030                                                           
         LA    R0,MAX2FQ                                                        
         LA    RE,CPY2F#1                                                       
MTP021   CLC   0(L'CPY2F#1,RE),SPACES                                           
         BH    MTP022                                                           
         MVC   0(L'CPY2F#1,RE),MTPFDATA                                         
         B     NEXTELM                                                          
*                                                                               
MTP022   CLC   0(L'CPY2F#1,RE),MTPFDATA                                         
         BE    NEXTELM                                                          
         LA    RE,L'CPY2F#1+1(,RE)                                              
         BCT   R0,MTP021                                                        
         DC    H'00'                                                            
*                                                                               
MTP030   LA    R7,CPYDLNQ(,R7)                                                  
         BCT   R5,MTP020                                                        
         B     NEXTELM                                                          
         DROP  R4,R7                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
DMCTREAD LR    R0,RE                                                            
         L     RE,AUTL                                                          
         MVC   SAV#SE,4(RE)                                                     
         MVC   4(1,RE),CTL#SE                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,TKEY,(R3)                             
         CLI   DMCB+8,0                                                         
         BE    DMERROK                                                          
         DC    H'00'                                                            
*                                                                               
DMCTHIGH LR    R0,RE                                                            
         L     RE,AUTL                                                          
         MVC   SAV#SE,4(RE)                                                     
         MVC   4(1,RE),CTL#SE                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,TKEY,(R3)                             
         B     DMERR                                                            
*                                                                               
DMCTSEQ  LR    R0,RE                                                            
         L     RE,AUTL                                                          
         MVC   SAV#SE,4(RE)                                                     
         MVC   4(1,RE),CTL#SE                                                   
         GOTO1 DATAMGR,DMCB,DMRSEQ,CTFILE,TKEY,(R3)                             
         B     DMERR                                                            
*                                                                               
DMOPNACC LR    R0,RE                                                            
         L     RE,AUTL                                                          
         MVC   SAV#SE,4(RE)                                                     
         MVC   4(1,RE),ACC#SE                                                   
         GOTO1 DATAMGR,DMCB,DMOPEN,ACCOUNT,ACFILEL                              
         B     DMERROK                                                          
*                                                                               
DMACREAD LR    R0,RE                                                            
         L     RE,AUTL                                                          
         MVC   SAV#SE,4(RE)                                                     
         MVC   4(1,RE),ACC#SE                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,ACCOUNT,TKEY,(R3)                            
         CLI   DMCB+8,0                                                         
         BE    DMERROK                                                          
         DC    H'00'                                                            
*                                                                               
DMACSEQ  LR    R0,RE                                                            
         L     RE,AUTL                                                          
         MVC   SAV#SE,4(RE)                                                     
         MVC   4(1,RE),ACC#SE                                                   
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCOUNT,TKEY,(R3)                            
         B     DMERR                                                            
*                                                                               
DMACHIGH LR    R0,RE                                                            
         L     RE,AUTL                                                          
         MVC   SAV#SE,4(RE)                                                     
         MVC   4(1,RE),ACC#SE                                                   
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCOUNT,TKEY,(R3)                            
         B     DMERR                                                            
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    DMERROK                                                          
         DC    H'0'                                                             
*                                                                               
DMERROK  L     RE,AUTL                                                          
         MVC   4(1,RE),SAV#SE                                                   
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  GET COMPANY RECORDS                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R3                                                       
XFILES   NTR1                                                                   
         STC   R1,XFMODE                                                        
         CLI   XFMODE,1                                                         
         BNE   XFILE100                                                         
         LA    R3,TKEY                                                          
         L     R7,=A(CPYLIST)                                                   
         ICM   R5,15,CPYCOUNT                                                   
         BZ    *+6                                                              
         BCTR  R5,0                                                             
         MHI   R5,CPYDLNQ                                                       
         AR    R7,R5                                                            
         L     R5,CPYCOUNT                                                      
         MVC   CPYKEY,SPACES                                                    
         XC    MYKEY,MYKEY                                                      
*                                                                               
XFILE005 L     R3,AIO2                                                          
         GOTO1 DMACHIGH                                                         
         TM    8(R1),X'80'         END OF FILE                                  
         BO    XFILE090                                                         
         CLC   TKEY,MYKEY                                                       
         BE    XFILE090                                                         
         MVC   MYKEY,TKEY                                                       
         LR    R4,R3                                                            
         AH    R4,DATADISP                                                      
*                                                                               
         USING CPYELD,R4                                                        
         USING CPYDATAD,R7                                                      
XFILE010 CLI   0(R4),0             EOR ?                                        
         BE    XFILE030                                                         
*        BNE   *+6                 YES                                          
*        DC    H'00'                                                            
*                                                                               
         CLI   0(R4),CPYELQ        COMPANY   ELEMENT ?                          
         BNE   XFILE020            YES, PROCESS                                 
         MVC   CPYACC,ACCSYSID                                                  
         MVC   CPYHEX,CPYKCPY                                                   
         MVC   CPYLOGON,CPYLOGO   GET COMPANY LOGO                              
         MVC   CPYALFA,CPYALPHA   GET COMPANY ALPHA CODE                        
         LA    R7,CPYDLNQ(,R7)                                                  
         LA    R5,1(,R5)           NUMBER OF ENTRIES                            
         B     XFILE030                                                         
*                                                                               
XFILE020 ZIC   RF,1(,R4)           GET ELEMENT LENGTH                           
         AR    R4,RF               BUMP TO NEXT ELEMENT                         
         B     XFILE010            TRY NEXT ELEMENT                             
*                                                                               
XFILE030 MVC   TKEY,CPYKEY                                                      
         LA    R3,TKEY                                                          
         MVI   CPYKCPY+1,X'FF'                                                  
         B     XFILE005                                                         
*                                                                               
XFILE090 ST    R5,CPYCOUNT                                                      
         B     XFILE900                                                         
         EJECT                                                                  
XFILE100 CLI   XFMODE,2                                                         
         BNE   XFILE900                                                         
         L     R7,=A(CPYLIST)                                                   
         ICM   R5,15,CPYCOUNT                                                   
         BZ    XFILE900                                                         
         L     R2,AUTL             ->    UTL                                    
         MVC   4(1,R2),CTL#SE      RESTORE    SE   ID   FOR  CTL  FILES         
*                                                                               
XFILE110 XC    MPROFILE,MPROFILE                                                
         XC    WORK,WORK                                                        
         XC    CPYSHEX,CPYSHEX     ALTERNATE HEX CODE                           
         MVC   CPYSALFA,SPACES     ALTERNATE ALPHA                              
         MVC   WORK(4),=C'S0MX'                                                 
         MVC   WORK+4(2),CPYALFA                                                
         GOTO1 GETPROF,DMCB,WORK,MPROFILE,DATAMGR                               
         OC    MPROFILE(2),MPROFILE                                             
         BZ    XFILE130            NONE FOUND                                   
         CLC   MPROFILE(2),=C'00'                                               
         BE    XFILE130            NONE FOUND                                   
         MVC   CPYSALFA,MPROFILE                                                
*                                                                               
         USING CT5REC,R3                                                        
         LA    R3,TKEY                                                          
         XC    TKEY,TKEY                                                        
         MVI   CT5KTYP,CT5KTYPQ   C'5'                                          
         MVC   CT5KALPH,CPYSALFA  ALPHA USER ID                                 
         L     R3,AIO2                                                          
         GOTO1 DMCTHIGH                                                         
         CLC   CT5KEY,TKEY                                                      
         BE    XFILE125                                                         
         OI    CPYDIND,CPYSNF                                                   
         B     XFILE130                                                         
*                                                                               
XFILE125 OI    CPYDIND,CPYSFD                                                   
         GOTO1 SETCPYX,CPYSHEX                                                  
*                                                                               
XFILE130 XC    MPROFILE,MPROFILE                                                
         XC    WORK,WORK                                                        
         XC    CPYPHEX,CPYPHEX     ALTERNATE HEX CODE                           
         MVC   CPYPALFA,SPACES     ALTERNATE ALPHA                              
         MVC   WORK(4),=C'P0MX'                                                 
         MVC   WORK+4(2),CPYALFA                                                
         GOTO1 GETPROF,DMCB,WORK,MPROFILE,DATAMGR                               
         OC    MPROFILE(2),MPROFILE                                             
         BZ    XFILE140            NONE FOUND                                   
         CLC   MPROFILE(2),=C'00'                                               
         BE    XFILE140            NONE FOUND                                   
         MVC   CPYPALFA,MPROFILE                                                
*                                                                               
         USING CT5REC,R3                                                        
         LA    R3,TKEY                                                          
         XC    TKEY,TKEY                                                        
         MVI   CT5KTYP,CT5KTYPQ   C'5'                                          
         MVC   CT5KALPH,CPYPALFA  ALPHA USER ID                                 
         L     R3,AIO2                                                          
         GOTO1 DMCTHIGH                                                         
         CLC   CT5KEY,TKEY                                                      
         BE    XFILE135                                                         
         OI    CPYDIND,CPYPNF                                                   
         B     XFILE140                                                         
*                                                                               
XFILE135 OI    CPYDIND,CPYPFD                                                   
         GOTO1 SETCPYX,CPYPHEX                                                  
*                                                                               
XFILE140 LA    R7,CPYDLNQ(,R7)                                                  
         BCT   R5,XFILE110                                                      
*                                                                               
         USING PCPYD,R2                                                         
         LA    R2,P                                                             
         L     R5,CPYCOUNT                                                      
         L     R7,=A(CPYLIST)                                                   
*                                                                               
XFILE200 MVC   PACCFILE,CPYACC                                                  
         MVC   PALFA,CPYALFA                                                    
         MVC   PLOGO,CPYLOGON                                                   
         MVC   PSHEX,=C'SPOT='                                                  
         GOTO1 HEXOUT,DMCB,CPYSHEX,PSHEXCD,1                                    
         MVC   PPHEX,=C'PRINT='                                                 
         GOTO1 HEXOUT,DMCB,CPYPHEX,PPHEXCD,1                                    
         MVC   PHEX,=C'MAIN='                                                   
         GOTO1 HEXOUT,DMCB,CPYHEX,PCPYHEX,1                                     
         CLC   CPYPALFA,SPACES                                                  
         BNH   XFILE210                                                         
         MVC   PPRNTA,=C'P='                                                    
         MVC   PPRNTAPH,CPYPALFA                                                
*                                                                               
XFILE210 CLC   CPYSALFA,SPACES                                                  
         BNH   XFILE212                                                         
         MVC   PSPOTA,=C'S='                                                    
         MVC   PSPOTAPH,CPYSALFA                                                
*                                                                               
XFILE212 TM    CPYDIND,CPYSNF+CPYPNF                                            
         BZ    *+8                                                              
         MVI   PERRIND,C'*'                                                     
*                                                                               
         LA    RE,P2FDATA                                                       
         CLC   CPY2F#1(3*14),SPACES                                             
         BNH   XFILE220                                                         
         MVC   P2F,=C'2F='                                                      
         MVC   0(3*14,RE),CPY2F#1                                               
*                                                                               
XFILE220 GOTO1 ACREPORT                                                         
         LA    R7,CPYDLNQ(,R7)                                                  
         BCT   R5,XFILE200                                                      
*                                                                               
XFILE900 B     XIT                                                              
         DROP  R2,R3,R4,R7                                                      
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
         USING CT5REC,R3                                                        
SETCPYX  NTR1                                                                   
         L     R3,AIO2                                                          
         CLC   CT5KALPH,=C'CI'     THIS IS A BAD ALPHA SO SKIP                  
         B     XIT                                                              
         LA    R4,CT5DATA          POINT TO ELEMENT                             
*                                                                               
         USING CTSYSD,R4                                                        
SETCPY10 CLI   0(R4),0             EOR                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   0(R4),CTSYSELQ      X'21' - SYSTEM ELEMENT                       
         BNE   SETCPY15                                                         
         CLI   CTSYSNUM,6          ACC ?                                        
         BE    SETCPY20                                                         
*                                                                               
SETCPY15 SR    RF,RF                                                            
         IC    RF,1(,R4)                                                        
         AR    R4,RF                                                            
         B     SETCPY10                                                         
*                                                                               
SETCPY20 MVC   0(1,R1),CTSYSAGB                                                 
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*  GET COMPANY RECORD                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R3                                                       
GETCPY   NTR1                                                                   
         LA    R3,TKEY                                                          
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,TEMPCPY     MOVE IN COMPANY HEX CODE                     
         L     R3,AIO2                                                          
         BAS   RE,DMACHIGH                                                      
         LR    R4,R3                                                            
         AH    R4,DATADISP                                                      
*                                                                               
         USING CPYELD,R4                                                        
GETCPY10 CLI   0(R4),0             EOR ?                                        
         BNE   *+6                 YES                                          
         DC    H'00'                                                            
*                                                                               
         CLI   0(R4),CPYELQ        COMPANY   ELEMENT ?                          
         BNE   GETCPY20            YES, PROCESS                                 
         MVC   CLOGO,CPYLOGO       GET  COMPANY   LOGO                          
         MVC   CALPHA,CPYALPHA     GET  COMPANY   ALPHA     CODE                
         B     GETCPY90                                                         
*                                                                               
GETCPY20 ZIC   RF,1(,R4)           GET  ELEMENT   LENGTH                        
         AR    R4,RF               BUMP TO   NEXT ELEMENT                       
         B     GETCPY10            TRY  NEXT ELEMENT                            
*                                                                               
GETCPY90 XIT1                                                                   
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  PRINT TOTALS FOR COMPANY                                           *         
***********************************************************************         
         SPACE 1                                                                
*&&DO                                                                           
         USING PSD,R4                                                           
PRTCPYTO NTR1                                                                   
         CLI   PRVKEY+2,X'00'           ANY  COMPANY ?                          
         BE    PRTCPYTX                 NO,  EXIT                               
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R4,P                     ->   OUTPUT TEXT                        
         MVC   PSDSYSIH,=C'ACC SYS='                                            
         MVC   PSDSYSID,ACCSYSID        GET  ACCPAK SYSTEM ID                   
         MVC   PSDCPYCH,=C'COMP='                                               
         MVC   CPYCDE,PRVKEY+2          GET  COMPANY CODE                       
         GOTO1 HEXOUT,DMCB,CPYCDE,PSDCPYCD,1                                    
         MVC   PSDCPYAH,=C'ALPHA='      GET  COMPANY ALPHA CODE                 
         MVC   PSDCPYAL,CALPHA                                                  
         MVC   PSDCPYLH,=C'LOGO='       GET  COMPANY LOGO                       
         MVC   PSDCPYLO,CLOGO                                                   
         ZAP   WORK(8),CPYFNDS          GET  NUMBER OF MATCHES                  
         OI    WORK+7,X'0F'                                                     
         MVC   PSDRMATH,=C'RECORDS MATCHED='                                    
         UNPK  PSDRMAT,WORK(8)                                                  
         ZAP   WORK(8),CPYRECS          GET  NUMBER OF RECORDS                  
         OI    WORK+7,X'0F'                                                     
         MVC   PSDTRECH,=C'TOTAL RECORDS='                                      
         UNPK  PSDTREC,WORK(8)                                                  
         GOTO1 ACREPORT                                                         
         CP    CPYFNDS,=P'0'            SKIP A LINE IF THERE WAS DATA           
         BE    PRTCPYTX                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
PRTCPYTX DS    0H                                                               
         ZAP   CPYRECS,=P'0'       COMPANY   TOTAL     RECORDS                  
         ZAP   CPYFNDS,=P'0'       COMPANY   TOTAL     FOUND                    
         B     XIT                 RETURN                                       
         DROP  R4                                                               
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
*  EQUATES                                                            *         
***********************************************************************         
         SPACE 1                                                                
MAX2FQ   EQU   24                                                               
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
CHARALL  EQU   C'A'                CHARACTER  FORMAT KEYWORDS ** ALL **         
CHARONLY EQU   C'C'                CHARACTER  FORMAT KEYWORDS ONLY              
TRANONLY EQU   C'T'                TRANSLATED FORMAT KEYWORDS ONLY              
CHARRPTS EQU   C'C'                CHARACTER  FORMAT REPORT   TYPES             
CHARRPTM EQU   C'M'                CHARACTER  FORMAT REPORT   TYPES             
*                                             NOT IN TABLE    AND               
*                                             FORMAT NAMES OF NULLS             
TRANRPTS EQU   C'T'                TRANSLATED FORMAT REPORT   TYPES             
EOT      EQU   X'FF'               END OF TABLE                                 
ESCJLFT  EQU   34                  X'22' ABOVE ESCAPE SEQUENCE                  
ESCHIGHQ EQU   48                  X'30' ABOVE ESCAPE SEQUENCE                  
K        EQU   1024                MAX   LENGTH    OF   RECORD                  
         SPACE 3                                                                
*        SUBDIVISION OF BATCH TYPES                                             
         SPACE 1                                                                
TY30DI   EQU   229                 TYPE 30 DIFFERENCE (FOREIGN CURR)            
TY06MN   EQU   230                 TYPE 06 MANUAL BILLING                       
TY30CH   EQU   231                 TYPE 30 CHECK                                
TY30OF   EQU   232                 TYPE 30 OFFSET                               
TY30WO   EQU   233                 TYPE 30 WRITE OFF                            
TY30TT   EQU   234                 TYPE 30 TRANSFER TO                          
TY30TF   EQU   235                 TYPE 30 TRANSFER FROM                        
         EJECT ,                                                                
***********************************************************************         
*  CONSTANTS                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
HEXIN    DC    V(HEXIN)                                                         
*                                                                               
AIO1     DC    A(IO1)              4K FOR FORMAT RECORD                         
AIO2     DC    A(IO2)              2K FOR ANYTHING YOU WANT                     
AIO3     DC    A(IO3)              2K SYSTEM LIST                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
EVERY    DC    PL4'1'                                                           
MAXDUMP  DC    PL4'500'                                                         
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
DMOPEN   DC    CL8'OPEN    '                                                    
ACFILEL  DC    C'NACCDIR NACCMST X'                                             
FNAMEC   DC    C'FORMAT NAME'                                                   
RTYPEC   DC    C'REPORT TYPE CODE'                                              
RTYPEN   DC    C'REPORT TYPE NAME'                                              
*                                                                               
* POSTING SCREENS                                                               
*                                                                               
POSTSC   DC    X'01',CL7'POST   '                                               
         DC    X'02',CL7'APOST  '                                               
         DC    X'03',CL7'RPOST  '                                               
         DC    X'04',CL7'SPOST  '                                               
         DC    X'05',CL7'UCPOST '                                               
         DC    X'06',CL7'UNPOST '                                               
         DC    X'07',CL7'UACPOST'                                               
         DC    X'08',CL7'UANPOST'                                               
         DC    X'09',CL7'TPOST  '                                               
         DC    X'0A',CL7'DPOST  '                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  BUFFERS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         DC    F'0'                IOAREA     #1 - ACCOUNT   RECORD             
         DC    C'**AIO1**'                                                      
IO1      DC    (K*4)X'00'                                                       
*                                                                               
         DC    F'0'                IOAREA     #2 - SYSTEM    LIST REC           
         DC    C'**AIO2**'                                                      
IO2      DC    (K*2)X'00'                                                       
*                                                                               
         DC    F'0'                IOAREA     #3 - SYSTEM    LIST REC           
         DC    C'**AIO3**'                                                      
IO3      DC    (K*2)X'00'                                                       
*                                                                               
CPYLIST  DS    500CL(CPYDLNQ)                                                   
         EJECT ,                                                                
***********************************************************************         
*  VARIABLES                                                          *         
***********************************************************************         
         SPACE 1                                                                
ACXVD    DSECT                                                                  
AUTL     DS    A                   UTL                                          
*                                                                               
CPYCOUNT DS    A                   COUNT NUMBER OF COMPANIES                    
*                                                                               
RQSTART  DS    PL3                                                              
RQEND    DS    PL3                                                              
FILTSEL  DS    XL1                                                              
*                                                                               
LASTACTD DS    CL10                LAST  ACTIVITY  DATE (MMMDD/YYYY)            
REQDATE  DS    PL3                 LAST  REQUEST   DATE (PACKED)                
REQDATED DS    CL10                LAST  REQUEST   DATE (MMMDD/YYYY)            
REQUEST# DS    CL3                 NUM   OF   OVERNIGHT REQUESTS                
*                                                                               
PRTFLAG  DS    XL1                                                              
PRTELM   EQU   X'80'                                                            
PRTNCPY  EQU   X'40'                                                            
*                                                                               
NEWTYPE  DS    CL1                                                              
NEWCODE  DS    CL6                                                              
SAVELDG  DS    CL2                                                              
CPYCDE   DS    XL1                                                              
PRVCPY   DS    XL1                 PREVIOUS COMPANY PROCESSED                   
*                                                                               
CPYRECS  DS    PL8                                                              
CPYFNDS  DS    PL8                                                              
TOTRECS  DS    PL8                                                              
TOTFNDS  DS    PL8                                                              
FTOTRECS DS    PL8                 FULL  TOTAL     RECORDS                      
FTOTFNDS DS    PL8                 FULL  TOTAL     FOUND                        
*                                                                               
CLOGO    DS    CL(L'CPYLOGO)       COMPANY    LOGO                              
CALPHA   DS    CL(L'CPYALPHA)      COMPANY    ALPHA     CODE                    
*                                                                               
CKEY     DS    CL(L'ACCKEY)        COMPANY KEY                                  
SVKEY    DS    CL(L'ACCKEY)        SAVED KEY                                    
CTKEY    DS    CL(L'ACCKEY)        SAVED CONTROL FILE KEY                       
MYKEY    DS    CL(L'ACCKEY)                                                     
PRVKEY   DS    CL(L'ACCKEY)                                                     
SKEY     DS    CL(L'ACCKEY)        DIRECTORY  KEY  FOR  SYS  LIST RCD           
TKEY     DS    CL(L'ACCKEY)        DM KEY                                       
*                                                                               
TEXTMSG  DS    CL12                                                             
*                                                                               
MPROFILE DS    XL16                                                             
*                                                                               
LIMRTYPS DS    CL1                 LIMIT REPORT    TYPES                        
FORMNAME DS    CL(L'RESKFORM)      FORMAT     NAME                              
SVFORMNM DS    CL(L'RESKFORM)      FORMAT     NAME SAVE AREA                    
REPTCODE DS    CL(L'STYCODE)       REPORT     TYPE CODE                         
REPTNAME DS    CL(L'STYNAME)       REPORT     TYPE NAME                         
FNDRPTYP DS    CL(L'STYNAME)       REPORT     TYPE NAME THAT WAS  FOUND         
NPARMS   DS    XL1                 #     OF   PARAMETERS                        
*                                                                               
XFMODE   DS    XL1                                                              
*                                                                               
FOUNDSW  DS    XL1                 FOUND SWITCHES TO   REPORT                   
FNDKYW   EQU   X'80'               .     KEYWORD                                
FNDFILT  EQU   X'40'               .     FILTER                                 
FNDRPTTN EQU   X'20'               .     REPORT    TYPE NAME                    
FNDMULEL EQU   X'10'               .     MISSING   UNIT/LEDGER    EL            
FNDMFFEL EQU   X'08'               .     MISSING   FREE FORM (STYELD)EL         
FNDC0EL  EQU   X'04'               .     OLD  SCRIBE    ELEMENT                 
*                                                                               
FOUNDSW2 DS    XL1                 FOUND SWITCHES FOR  EXCLUDE                  
FNDSKIPR EQU   X'80'               .     SKIP THIS RECORD                       
FNDEXC   EQU   X'40'               .     EXCLUDE   KYW  AND  FILTER             
FNDULEL  EQU   X'20'               .     UNIT/LEDGER    ELEMENT                 
FNDFFEL  EQU   X'10'               .     FREE FORM (STYELD)  ELEMENT            
*                                                                               
DICTFORM DS    CL1                 DICTIONARY FORM (Y/N)                        
FNDKEYWD DS    CL6                 LAST  KEYWORD   THAT WAS  FOUND              
FNDFILTR DS    CL19                LAST  FILTER    THAT WAS  FOUND              
*                                                                               
REREAD   DS    C                   Y OR N                                       
*                                                                               
CTL#SE   DS    X                   SE# CTL-FILE                                 
ACC#SE   DS    X                   SE# ACCPAK                                   
SAV#SE   DS    X                   SE# SAVED                                    
ACCSYSID DS    CL4                 ACCPAK     SYSTEM    ID                      
UPSI     DS    X                   JOB   UPSI BYTE                              
UPSIGET  EQU   X'80'               .     DUMP GET  REQUESTS                     
UPSIPUT  EQU   X'40'               .     DUMP PUT  REQUESTS                     
UPSIELMT EQU   X'20'               .     DUMP UPDATED   ELEMENTS                
*                                                                               
WRTSW    DS    CL1                                                              
DMPSW    DS    CL1                                                              
TEMPCPY  DS    XL1                                                              
*                                                                               
ELM      DS    CL255                                                            
BLOCK    DS    6CL32               DATA  BLOCK     FOR  SCANNER                 
*                                                                               
DUMPCNT  DS    PL4                 RECORDS      DUMPED                          
DUMPECNT DS    PL4                 ELEMENTS     DUMPED                          
PDUMP    DS    PL4                 TOTAL        DUMPED                          
         EJECT ,                                                                
***********************************************************************         
*  KEYWORDS DSECT                                                     *         
***********************************************************************         
         SPACE 1                                                                
CPYDATAD DSECT                                                                  
CPYACC   DS    CL4                                                              
CPYALFA  DS    CL2                                                              
CPYHEX   DS    XL1                                                              
CPYLOGON DS    CL(L'CPYLOGO)                                                    
CPY2F#1  DS    CL2                                                              
         DS    CL1                                                              
         DS    (MAX2FQ-1)CL3                                                    
CPYSHEX  DS    XL1                                                              
CPYSALFA DS    CL2                                                              
CPYPHEX  DS    XL1                                                              
CPYPALFA DS    CL2                                                              
CPYDIND  DS    XL1                                                              
CPYSNF   EQU   X'80'                                                            
CPYPNF   EQU   X'40'                                                            
CPYSFD   EQU   X'20'                                                            
CPYPFD   EQU   X'10'                                                            
CPYDLNQ  EQU   *-CPYDATAD                                                       
         EJECT ,                                                                
***********************************************************************         
*  DETAIL OUTPUT LINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
         EJECT ,                                                                
PCPYD    DSECT                                                                  
         DS    CL1                                                              
PERRIND  DS    CL1                                                              
         DS    CL1                                                              
PACCFILE DS    CL4                                                              
         DS    CL2                                                              
PALFA    DS    CL(L'CPYALPHA)                                                   
         DS    CL1                                                              
PLOGO    DS    CL(L'CPYLOGO)                                                    
         DS    CL1                                                              
PSHEX    DS    CL5                 SPOT=                                        
PSHEXCD  DS    CL2                                                              
         DS    CL1                                                              
PPHEX    DS    CL6                 PRINT=                                       
PPHEXCD  DS    CL2                                                              
         DS    CL1                                                              
PHEX     DS    CL5                 MAIN=                                        
PCPYHEX  DS    CL2                                                              
         DS    CL1                                                              
PSPOTA   DS    CL2                 S=                                           
PSPOTAPH DS    CL2                                                              
         DS    CL1                                                              
PPRNTA   DS    CL2                 P=                                           
PPRNTAPH DS    CL2                                                              
         DS    CL1                                                              
P2F      DS    CL3                                                              
P2FDATA  DS    (MAX2FQ)CL3                                                      
         DS    CL1                                                              
         EJECT ,                                                                
***********************************************************************         
* PRINT LINE                                                                    
***********************************************************************         
PLINED   DSECT                                                                  
         DS    CL2                                                              
PCPY     DS    CL2                 COMPANY CODE                                 
         DS    CL2                                                              
PALPHA   DS    CL2                 ALPHA                                        
         DS    CL1                                                              
PLOGO2   DS    CL(L'CPYLOGO)                                                    
         DS    CL1                                                              
PSYS     DS    CL1                 SYSTEM                                       
         DS    CL1                                                              
PMED     DS    CL1                 MEDIA                                        
         DS    CL1                                                              
POFC     DS    CL2                 OFFICE CODE                                  
         DS    CL1                                                              
PCLI     DS    CL3                 CLIENT                                       
         DS    CL1                                                              
PPRD     DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
PPROF    DS    CL7                 PROFILE                                      
         DS    CL1                                                              
PTYPE    DS    CL3                                                              
         DS    CL1                                                              
PAOR     DS    CL3                 AOR OR NOT                                   
         DS    CL1                                                              
PULA     DS    CL14                ACCOUNT                                      
         DS    CL1                                                              
POFF     DS    CL2                 OFFICE                                       
         DS    CL1                                                              
PCULA    DS    CL14                CONTRA                                       
         DS    CL1                                                              
PPOST$   DS    CL10                POST AMOUNT                                  
         DS    CL1                                                              
PMEMO$   DS    CL10                MEMO AMOUNT                                  
         ORG   POFF                                                             
PPERID   DS    CL3                 PERSON ID                                    
         DS    CL1                                                              
PAMTX    DS    CL4                                                              
PAMTEXP  DS    CL8                 EXPRESSION                                   
         DS    CL1                                                              
PMEMOX   DS    CL5                                                              
PMEMOEXP DS    CL8                 EXPRESSION                                   
         DS    CL1                                                              
PGRP     DS    CL1                 PRODUCT GROUP OVERRIDE                       
PLINEQ   EQU   *-PLINED            LENGTH OF A DETAIL OUTPUT LINE               
         EJECT ,                                                                
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACSCRDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACSCRDSECT                                                     
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
*DDREPMASTD                                                                     
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*DMDTFIS                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
*ACDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'248ACREPXV09 08/19/03'                                      
         END                                                                    
