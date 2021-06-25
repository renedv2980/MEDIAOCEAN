*          DATA SET ACCRES     AT LEVEL 013 AS OF 01/19/14                      
*PHASE ACCRESA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE UNSCAN                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE STXITER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE DMPRTQB                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'ACCRES - ACCPAK COMPANY RESIDENCE DIRECTORY'                    
ACCRES   CSECT                                                                  
         PRINT NOGEN                                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,*ACCRES*,RA,R9,WORK=V(REGSAVE)                                 
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         ST    RB,DMPLIST                                                       
         MVI   DMPLIST,0                                                        
         L     R2,=V(DUMMY)                                                     
         ST    R2,DMPLIST+4                                                     
         MVI   DMPLIST+4,X'80'         END OF LIST                              
         GOTO1 STXITER,DMCB,DMPLIST                                             
         XC    NUMIDS,NUMIDS                                                    
         EJECT                                                                  
***********************************************************************         
* READ CARDS - SET UP TABLE OF REQUESTED REPORTS                      *         
*  FORMAT OF CARD IS SEQ=XX(,SYSTEM=ACCX)                             *         
***********************************************************************         
         LA    R6,CRDTAB                                                        
         USING CRDD,R6                                                          
ACCR01   GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   CARD(2),=C'/*'                                                   
         BE    ACCR15                                                           
         CLI   CARD,C'*'                                                        
         BE    ACCR01                                                           
         CLC   =C'DDSIO=',CARD                                                  
         BNE   ACCR01A                                                          
         L     RE,=V(DDSIO)                                                     
         MVC   0(8,RE),CARD+6                                                   
         B     ACCR01                                                           
*                                                                               
         USING SSBD,RE                                                          
ACCR01A  CLC   =C'DSPACE=',CARD                                                 
         BNE   ACCR02                                                           
         L     RE,=V(SSB)                                                       
         MVC   SSODSPAC,CARD+7                                                  
         B     ACCR01                                                           
         DROP  RE                                                               
*                                                                               
ACCR02   XC    WORK,WORK                                                        
         GOTO1 SCANNER,DMCB,(C'C',CARD),(3,WORK)                                
         CLI   4(R1),0                                                          
         BE    BADCARD                                                          
         LA    R3,WORK                                                          
*                                                                               
         CLC   12(4,R3),=C'NOOP'                                                
         BNE   ACCR02B                                                          
         LLC   RF,NOOP#            NUMBER OF SYSTEMS TO NOOP                    
         LR    R0,RF                                                            
         MHI   RF,L'SYLNAME                                                     
         LARL  RE,NOOPLIST                                                      
         AR    RE,RF                                                            
         MVC   0(L'SYLNAME,RE),22(R3)                                           
         AHI   R0,1                                                             
         STC   R0,NOOP#                                                         
         B     ACCR01                                                           
*                                                                               
ACCR02B  CLC   12(3,R3),=C'SYS'                                                 
         BNE   ACCR02C                                                          
         LLC   RF,ISYS#            NUMBER OF SYSTEMS TO INCLUDE                 
         LR    R0,RF                                                            
         MHI   RF,L'SYLNAME                                                     
         LARL  RE,ISYSLIST                                                      
         AR    RE,RF                                                            
         MVC   0(L'SYLNAME,RE),22(R3)                                           
         AHI   R0,1                                                             
         STC   R0,ISYS#                                                         
         B     ACCR01                                                           
*                                                                               
ACCR02C  CLC   12(4,R3),=C'TEST'                                                
         BNE   ACCR03                                                           
         MVC   TESTFILE,22(R3)                                                  
         B     ACCR01                                                           
*                                                                               
ACCR03   XC    0(CRDLEQU,R6),0(R6)                                              
         CLC   12(3,R3),=C'SEQ'                                                 
         BNE   BADCARD                                                          
         LA    R5,RPTAB            VALIDATE REPORT CODE                         
         USING RPTD,R5                                                          
ACCR04   CLC   RPTCD,22(R3)                                                     
         BE    ACCR05                                                           
         LA    R5,RPTLNQ(R5)                                                    
         CLI   0(R5),X'FF'                                                      
         BNE   ACCR04                                                           
         B     BADCARD             INVALID SEQUENCE OPTION                      
*                                                                               
ACCR05   MVC   CRDSEQ,22(R3)       SAVE VALID REPORT SEQUENCE                   
         CLI   4(R1),1                                                          
         BE    ACCR07                                                           
         LA    R3,32(R3)                                                        
         CLC   12(3,R3),=C'SYS'                                                 
         BNE   BADCARD                                                          
         MVC   CRDFLT,22(R3)       SAVE SYSTEM FILTER NAME                      
*                                                                               
ACCR07   LA    R6,CRDLEQU(R6)                                                   
         MVI   0(R6),X'FF'                                                      
         B     ACCR01              GET NEXT CARD                                
*                                                                               
BADCARD  MVC   P+1(80),CARD                                                     
         GOTO1 PRINTER                                                          
         MVC   P+1(30),=CL30'INVALID REQUEST CARD'                              
         GOTO1 PRINTER                                                          
         B     EOJ                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF SYSTEM FILES AND SE NUMBERS                           *         
***********************************************************************         
ACCR15   L     R2,=A(UTL)                                                       
         MVI   4(R2),10            SET UTL TO 10 TO OPEN CTFILE                 
         GOTO1 DATAMGR,DMCB,DMOPEN,=C'CONTROL',CTFILEL                          
*                                                                               
         L     R6,ASYSTAB                                                       
         USING SYLD,R6                                                          
         LA    R7,SYSLIST                                                       
*                                                                               
ACCR17   LA    R5,KEY              GET SYSTEM LIST RECORD                       
         USING CTWREC,R5                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
         MVC   CTWKSYSN,0(R7)                                                   
         BAS   RE,CTHIGH                                                        
         CLC   CTWREC,KEYSAVE                                                   
         BNE   ACCR25                                                           
*                                                                               
         L     R5,AIO                                                           
         LA    R5,CTWDATA                                                       
         USING SYSELD,R5                                                        
ACCR19   CLI   SYSEL,SYSELQ        SYSTEM LIST ELEMENT                          
         BNE   ACCR23                                                           
*        CLI   TESTFILE,C'Y'       INCLUDE TEST FILES ?                         
*        BE    ACCR20                                                           
*        CLC   =C'ACCTT',SYSNAME   NO, SKIP T                                   
*        BE    ACCR23                                                           
*        CLC   =C'ACC0',SYSNAME    AND 0                                        
*        BE    ACCR23                                                           
*                                                                               
ACCR20   MVC   SYLNAME,SYSNAME     SYSTEM NAME TO SYSTAB                        
         MVC   SYLSE,SYSSEN        SE NUMBER TO SYSTAB                          
         MVC   SYLNUM,SYSNUM       SYSTEM NUMBER ACC1, SPOTB ...ETC.            
         MVC   SYLDSP,1(R7)        DISPLACEMENT INTO IDTAB                      
         MVI   SYLFILT,NO                                                       
         CLC   =C'ACC',SYSNAME                                                  
         BNE   ACCR22                                                           
         GOTO1 FILTSYS,(R6)                                                     
         MVC   SYLCHR,SYSNAME+3                                                 
ACCR22   CLC   =C'NET',SYSNAME                                                  
         BNE   *+10                                                             
         MVC   SYLCHR,SYSNAME+3                                                 
         CLC   =C'STR',SYSNAME                                                  
         BNE   *+10                                                             
         MVC   SYLCHR,SYSNAME+3                                                 
*&&UK*&& CLC   =C'MED',SYSNAME                                                  
*&&UK*&& BNE   *+10                                                             
*&&UK*&& MVC   SYLCHR,SYSNAME+3                                                 
         CLC   =C'TAL',SYSNAME                                                  
         BNE   *+10                                                             
         MVC   SYLCHR,SYSNAME+3                                                 
         CLC   =C'PRNT',SYSNAME                                                 
         BNE   *+10                                                             
         MVC   SYLCHR,SYSNAME+4                                                 
         CLC   =C'SPOT',SYSNAME                                                 
         BNE   *+10                                                             
         MVC   SYLCHR,SYSNAME+4                                                 
         LA    R6,SYLLNQ(R6)                                                    
         MVI   SYLNAME,X'FF'         NEW END OF TABLE                           
*                                                                               
                                                                                
ACCR23   LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0             END OF RECORD                                
         BNE   ACCR19                                                           
*                                                                               
ACCR25   LA    R7,L'SYSLIST(R7)                                                 
         CLI   0(R7),X'FF'         TEST END OF SYSLIST                          
         BNE   ACCR17                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* READ ACCOUNT FILES / ADD TO ID TABLE                                *         
***********************************************************************         
         L     R6,ASYSTAB                                                       
         USING SYLD,R6                                                          
*                                                                               
ACCR30   CLC   SYLNAME(3),=C'ACC'                                               
         BNE   ACCR39              FOUND AN ACCOUNT SYSTEM                      
         CLI   SYLFILT,YES         FILTER IT OUT                                
         BE    ACCR39              YES                                          
ACCR30E  L     R2,=A(UTL)                                                       
         MVC   4(1,R2),SYLSE       SET UTL FOR THIS ACC FILE                    
         L     R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,DMOPEN,ACCOUNT,ACFILEL                              
         XC    DKEY,DKEY                                                        
         MVI   DKEY,X'41'           READ FOR FIRST/NEXT COMPANY                 
*                                                                               
ACCR31   BAS   RE,ACHIGH                                                        
         CLI   DMCB+8,0                                                         
         BNE   ACCR39              EOF OR SOME READ ERROR                       
         L     R4,AIO                                                           
         CLI   0(R4),X'FF'                                                      
         BE    ACCR39                                                           
         USING CPYRECD,R4                                                       
         LA    R4,CPYRFST                                                       
*                                                                               
         USING CPYELD,R4                                                        
ACCR32   CLI   CPYEL,CPYELQ        GET COMPANY ELEMENT                          
         BE    ACCR33                                                           
         CLI   0(R4),0                                                          
         BE    ACCR38                                                           
         XR    R0,R0                                                            
         IC    R0,CPYLN                                                         
         AR    R4,R0                                                            
         B     ACCR32                                                           
*                                                                               
         USING IDD,R7                                                           
ACCR33   LA    R7,IDWRK            BUILD ID RECORD IN WORK                      
         XC    IDWRK,IDWRK                                                      
         MVC   IDAGY,CPYALPHA      AGENCY ALPHA                                 
         OC    IDAGY,SPACES                                                     
         MVC   IDID,CPYUID         PRINCIPAL ID                                 
         MVC   IDABBR(L'CPYLOGO),CPYLOGO  ABBREVIATION                          
         L     RF,AIO                                                           
         MVC   IDACD,0(RF)         ACCPAK CODE                                  
         MVC   IDANUM,SYLNUM       SYSTEM NUMBER                                
         MVC   IDACHR,SYLCHR       AND CHARACTER                                
         OI    IDSTAT,IDSACC       ON-ACCOUNT FILE                              
         TM    CPYSTAT4,CPYSOFF2   NEW OFFICES                                  
         BNO   *+8                                                              
         OI    IDSTAT,IDSTWO       2 CHARACTER OFFICE                           
         BAS   RE,PROFS            BUILD PROFILES                               
         SR    R1,R1                                                            
*                                                                               
ACCR34   IC    R1,1(R4)            GET COMPANY NAME                             
         AR    R4,R1                                                            
         USING NAMELD,R4                                                        
         CLI   NAMEL,0                                                          
         BE    ACCR35                                                           
         CLI   NAMEL,NAMELQ                                                     
         BNE   ACCR34                                                           
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IDNAME(0),NAMEREC                                                
         OC    IDNAME,SPACES                                                    
*                                                                               
ACCR35   BAS   RE,ADDID            ADD IT TO TABLE                              
ACCR38   LA    RF,DIR                                                           
         SR    R1,R1                                                            
         IC    R1,0(RF)            GET COMPANY CODE OF PRESENT RECORD           
         AHI   R1,1                BUMP TO NEXT                                 
         XC    DKEY,DKEY                                                        
         STC   R1,DKEY             READ FOR NEXT KEY                            
         B     ACCR31                                                           
*                                                                               
ACCR39   LA    R6,SYLLNQ(R6)       SKIP IF NOT ACCOUNT                          
         CLI   SYLNAME,X'FF'                                                    
         BNE   ACCR30                                                           
         DROP  R4,R6,R7                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF ALPAHA ID'S AND SE NUMBERS                           *         
***********************************************************************         
ACCR40   L     R2,=A(UTL)                                                       
         MVI   4(R2),10                                                         
         LA    R5,KEY              BUILD A KEY                                  
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         BAS   RE,CTHIGH                                                        
         L     R5,AIO                                                           
         CLI   CTIKEY,CTIKTYPQ                                                  
         BE    *+6                                                              
         DC    H'0'                CAN'T READ THE FIRST RECORD                  
*                                                                               
ACCR41   LA    R7,IDWRK            BUILD IDD ENTRY IN WORK                      
         XC    IDWRK,IDWRK                                                      
         USING IDD,R7                                                           
         MVC   IDID,CTIKNUM        USER ID                                      
         LA    R5,CTIDATA          R5 TO FIRST ELEMENT                          
*                                                                               
ACCR42   CLI   0(R5),CTDSCELQ                                                   
         BNE   ACCR44              ID NAME                                      
         USING CTDSCD,R5                                                        
         MVC   WORK,SPACES         ID NAME ELEMENT                              
         SR    R1,R1                                                            
         IC    R1,CTDSCLEN                                                      
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),CTDSC                                                    
         MVC   IDABBR,WORK                                                      
         B     ACCR55                                                           
*                                                                               
ACCR44   CLI   0(R5),CTAGYELQ                                                   
         BNE   ACCR46              AGENCY ALPHA ELEMENT                         
         USING CTAGYD,R5                                                        
         MVC   IDAGY,CTAGYID       ALPHA CODE TO TABLE                          
         B     ACCR55                                                           
*                                                                               
ACCR46   CLI   0(R5),CTSYSELQ                                                   
         BNE   ACCR55              SYSTEM ELEMENT                               
         USING CTSYSD,R5                                                        
         L     R6,ASYSTAB          SEARCH SYSTEM TABLE                          
*                                                                               
         USING SYLD,R6                                                          
ACCR50   CLI   0(R6),X'FF'                                                      
         BE    ACCR55              NOT IN SYSTEM TABLE - SKIP IT                
         CLI   SYLFILT,YES                                                      
         BE    ACCR51                                                           
         CLC   CTSYSSE,SYLSE                                                    
         BE    ACCR52              SYSTEM SE MATCHES TABLE                      
ACCR51   LA    R6,SYLLNQ(R6)       NEXT SELIST ENTRY                            
         B     ACCR50                                                           
*                                                                               
ACCR52   SR    R1,R1               DISPLACEMENT TO SYS BYTES                    
         IC    R1,SYLDSP                                                        
         LA    R1,IDD(R1)          R1 TO SYSTEM BYTES IN TABLE ENTRY            
         CLI   CTSYSAGB,0                                                       
         BE    ACCR55              NO AGENCY CODE,  SKIP IT                     
         MVC   0(1,R1),SYLNUM      SYSTEM NUMBER                                
         MVC   1(1,R1),CTSYSAGB    BINARY AGENCY CODE                           
         MVC   2(2,R1),SYLCHR      SYSTEM CHARACTER                             
*                                                                               
ACCR55   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BNE   ACCR42              NEXT ELEMENT                                 
*                                                                               
ACCR56   BAS   RE,ADDID            ADD ID TO TABLE                              
         BAS   RE,CTSEQ            GET NEXT ID RECORD                           
         L     R5,AIO                                                           
         USING CTIREC,R5                                                        
         CLI   CTIKEY,CTIKTYPQ                                                  
         BNE   ACCR60                                                           
         OC    CTIKSPAR(CTIKNUM-CTIKSPAR),CTIKSPAR                              
         BZ    ACCR41                                                           
         DROP  R5,R6,R7                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS THE REPORTS                                                 *         
***********************************************************************         
ACCR60   LA    R6,CRDTAB                                                        
         USING CRDD,R6                                                          
*                                                                               
ACCR62   LA    R5,RPTAB                                                         
         USING RPTCD,R5           FIND REQUESTED REPORT IN TABLE                
         CLC   RPTCD,CRDSEQ                                                     
         BE    *+12                                                             
         LA    R5,RPTLNQ(R5)                                                    
         B     *-14                                                             
         SR    R4,R4                                                            
         IC    R4,RPTFLD          GET FIRST FIELD NUMBER OF THIS REPORT         
         BCTR  R4,0                                                             
         MHI   R4,FLDLNQ                                                        
         LA    R4,FLDTAB(R4)      R4 TO FIELD DEFINITION                        
         USING FLDD,R4                                                          
*                                 SORT TABLE INTO REQUESTED SEQUENCE            
         SR    R0,R0                                                            
         IC    R0,FLDDSP          DISPLACMENT TO FIELD                          
         STC   R0,SORTDSP                                                       
         SR    R2,R2                                                            
         IC    R2,FLDSLN          LENGTH FOR SORT                               
         LA    RE,IDLNQ           LENGTH OF ENTRY                               
         L     RF,NUMIDS          NUMBER IN TABLE                               
         GOTO1 XSORT,DMCB,(0,AIDTAB),(RF),(RE),(R2),(R0)                        
         BAS   RE,HDLNS           SET UP HEADLINES                              
*                                                                               
         L     R7,AIDTAB          PROCESS TABLE ENTRIES                         
         USING IDD,R7                                                           
ACCR72   SR    R1,R1                                                            
         IC    R1,SORTDSP                                                       
         LA    R1,0(R1,R7)        R1 TO SORT FIELD                              
         CLI   1(R1),0            IF NOTHING IN CODE FIELD                      
         BE    ACCR78             SKIP IT                                       
         CLI   CRDFLT,C' '        TEST ANY FILTERS                              
         BNH   *+14                                                             
*****AH3                                                                        
         CLC   IDACHR(2),CRDFLT+3    ACC FILTER                                 
         BNE   ACCR78             SKIP UNWANTED FILES                           
         LA    R3,P+1             R3 TO PRINT COLUMN                            
         LA    R2,L'RPTFLD        NUMBER OF COLUMNS TO PROCESS                  
         LA    R1,RPTFLD          CURRENT FIELD                                 
*                                                                               
ACCR76   SR    R4,R4                                                            
         CLI   0(R1),0            BLANK COLUMN                                  
         BE    ACCR77                                                           
         IC    R4,0(R1)                                                         
         BCTR  R4,0                                                             
         MHI   R4,FLDLNQ                                                        
         LA    R4,FLDTAB(R4)      R4 TO FIELD DEFINITION ENTRY                  
         USING FLDD,R4                                                          
         SR    RF,RF                                                            
         ICM   RF,7,FLDRTN        A(ROUTINE FOR THIS FIELD)                     
         STM   R1,R2,SVREG                                                      
         BASR  RE,RF              SPECIAL COLUMN ROUTINE                        
         LM    R1,R2,SVREG                                                      
*                                                                               
ACCR77   LA    R1,1(R1)           NEXT FIELD NUMBER                             
         BCT   R2,ACCR76                                                        
*                                                                               
         OC    P+1(L'P-1),SPACES                                                
         L     RF,PRINTER                                                       
         BASR  RE,RF              ID INFO TO PRINT                              
*                                                                               
         TM    IDSTAT,IDSCON      HANDLE ERROR MESSAGES                         
         BZ    ACCR78                                                           
         MVC   P+4(40),=CL40'*** ERROR *** CONFLICT ON  CONTROL FILE'           
         EDIT  (B2,IDERR),(4,P+46)                                              
         BASR  RE,RF                                                            
*                                                                               
ACCR78   LA    R7,IDLNQ(R7)        NEXT ID                                      
         CLI   0(R7),X'FF'                                                      
         BNE   ACCR72                                                           
         MVC   P+5(21),=C'COMPANIES ON ACCPAK  '                                
         EDIT  ACCNT,(3,P+1)                                                    
         CLC   CRDSEQ,=C'AP'                                                    
         BE    ACCR80                                                           
         MVC   P+30(21),=C'COMPANIES ON SPOTPAK '                               
         EDIT  SPCNT,(3,P+26)                                                   
         MVC   P+56(22),=C'COMPANIES ON PRINTPAK '                              
         EDIT  PRCNT,(3,P+52)                                                   
         MVC   P+83(21),=C'COMPANIES ON NETPAK  '                               
         EDIT  NECNT,(3,P+79)                                                   
*                                                                               
ACCR80   GOTO1 PRINTER                                                          
         LA    R6,CRDLEQU(R6)                                                   
         CLI   0(R6),X'FF'                                                      
         BNE   ACCR62                                                           
*                                                                               
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
EOJ      XBASE                                                                  
         DROP  R4,R5,R6,R7                                                      
         EJECT                                                                  
***********************************************************************         
* PROFILE ROUTINE                                                     *         
***********************************************************************         
         USING SYLD,R6                                                          
FILTSYS  NTR1                                                                   
         LR    R6,R1                                                            
         XR    R0,R0                                                            
         ICM   R0,1,NOOP#                                                       
         BZ    FILTSYS5                                                         
         LARL  RE,NOOPLIST                                                      
FILTSYS1 CLC   SYLNAME,0(RE)                                                    
         BE    FILTSYSN            SKIP SYSTEM                                  
         LA    RE,L'SYLNAME(,RE)                                                
         BCT   R0,FILTSYS1                                                      
         B     FILTSYSX                                                         
*                                                                               
FILTSYS5 XR    R0,R0                                                            
         ICM   R0,1,ISYS#                                                       
         BZ    FILTSYSX                                                         
         LARL  RE,ISYSLIST                                                      
FILTSYS6 CLC   SYLNAME,0(RE)                                                    
         BE    FILTSYSX            INCLUDE SYSTEM                               
         LA    RE,L'SYLNAME(,RE)                                                
         BCT   R0,FILTSYS6                                                      
         B     FILTSYSN                                                         
*                                                                               
FILTSYSN MVI   SYLFILT,YES         FILTER OUT                                   
FILTSYSX B     XIT                                                              
         DROP  R6                                                               
***********************************************************************         
* PROFILE ROUTINE                                                     *         
***********************************************************************         
         USING CPYELD,R4                                                        
         USING IDD,R7                                                           
PROFS    NTR1  ,                                                                
         MVC   IDPROF1,SPACES                                                   
         MVC   IDPROF2,SPACES                                                   
         MVC   IDPROF3,SPACES                                                   
         MVC   IDPROF4,SPACES                                                   
         MVC   UNSCN1H,SPACES                                                   
         MVC   UNSCN2H,SPACES                                                   
         MVC   UNSCN3H,SPACES                                                   
         MVC   UNSCN4H,SPACES                                                   
         SR    R5,R5                                                            
*                                                                               
         LA    R3,LWBLK                                                         
         GOTO1 OUTBLK,COMSTAT      BUILD OUTPUT BLOCK FOR UNSCAN                
         CLI   CPYLN,CPYLN1Q       TEST FOR SMALLER ELEMENT LENGTH              
         BL    PROF23                                                           
         GOTO1 OUTBLK,COMSTA2      CONTINUE OUTPUT BLOCK                        
         GOTO1 OUTBLK,COMSTA3                                                   
         GOTO1 OUTBLK,COMSTA4                                                   
         GOTO1 OUTBLK,COMSTA5                                                   
         GOTO1 OUTBLK,COMSTA6                                                   
         GOTO1 OUTBLK,COMSTA7                                                   
         GOTO1 OUTBLK,COMSTA8                                                   
         GOTO1 OUTBLK,COMCD                                                     
                                                                                
         OC    CPYUID,CPYUID                                                    
         BZ    PROF7                                                            
         LA    R5,1(R5)            ADD 1 TO OUTPUT COUNT                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(2,R3),=C'ID'                                                   
         EDIT  (2,CPYUID),(5,10(R3)),ALIGN=LEFT                                 
*                                                                               
PROF7    CLI   CPYLN,CPYLN3Q                                                    
         BL    PROF8                                                            
         CLI   CPYTCMP,0                                                        
         BE    PROF8                                                            
         LA    R5,1(R5)            ADD 1 TO OUTPUT COUNT                        
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(3,R3),=C'TID'                                                  
         GOTO1 HEXOUT,DMCB,CPYTCMP,WORK,1,0,0                                   
         MVC   10(2,R3),WORK                                                    
*                                                                               
PROF8    TM    CPYSTAT7,CPYSTMSY   COMPANY ON TMS?                              
         BNO   PROF9                                                            
         LA    R5,1(R5)            ADD 1 TO OUTPUT COUNT                        
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(3,R3),=C'TMS'                                                  
         CLI   CPYLN,CPYLN3Q       LONGER EL HAS TMS START DATE                 
         BL    PROF9                                                            
         OC    CPYTMSSD,CPYTMSSD   TMS START DATE?                              
         BZ    PROF9                                                            
         GOTO1 DATCON,DMCB,(2,CPYTMSSD),(17,10(R3))                             
*                                                                               
PROF9    CLI   CPYLN,CPYLN2Q       TEST FOR SMALLER ELEMENT LENGTH              
         BL    PROF15                                                           
         OC    CPYTSD,CPYTSD       DISPLAY TIME SHEET DAY                       
         BZ    PROF13                                                           
         LA    R5,1(R5)            INCREMENT OUTPUT COUNT                       
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(3,R3),=C'TSD'                                                  
*                                                                               
         LA    RF,DAYS                                                          
         LA    R0,DAYSLNQ          # TIMES TO LOOP                              
         USING DAYSD,RF                                                         
*                                                                               
PROF11   CLC   DAYNUM,CPYTSD                                                    
         BNE   *+14                                                             
         MVC   10(L'DAYNAME,R3),DAYNAME                                         
         B     PROF15                                                           
         LA    RF,DAYQ(RF)                                                      
         BCT   R0,PROF11                                                        
         B     PROF15                                                           
         DROP  RF                                                               
*                                                                               
PROF13   OC    CPYCTFIL,CPYCTFIL   ANY AGENCY ID?                               
         BZ    PROF15              NO                                           
         LA    R5,1(R5)            YES, INCREMENT OUTPUT COUNT                  
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(6,R3),=C'CTFILE'                                               
         MVC   10(2,R3),CPYCTFIL                                                
*                                                                               
PROF15   CLI   CPYTENO,C'0'                                                     
         BL    PROF17                                                           
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(2,R3),=C'TE'                                                   
         MVC   10(1,R3),CPYTENO                                                 
         LA    R5,1(R5)                                                         
*                                                                               
PROF17   CLI   CPYDEPTL,0                                                       
         BE    PROF19                                                           
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(4,R3),=C'DPTL'                                                 
         MVC   10(1,R3),CPYDEPTL                                                
         OI    10(R3),X'F0'                                                     
         LA    R5,1(R5)                                                         
*                                                                               
PROF19   CLI   CPYLN,CPYLN2Q                                                    
         BL    PROF21                                                           
         CLI   CPYOFFC,X'40'                                                    
         BNH   PROF21                                                           
         LA    R3,20(R3)                                                        
         MVC   0(20,R3),SPACES                                                  
         MVC   0(5,R3),=C'GLOFF'                                                
         MVC   10(2,R3),CPYOFFC                                                 
         LA    R5,1(R5)                                                         
*                                                                               
PROF21   DS    0H                                                               
         LTR   R5,R5                    WERE ANY ENTRIES ADDED TO               
         BZ    PROF23                   PROFILE TABLE                           
         MVI   UNSCN1H,L'UNSCN1H                                                
         GOTO1 UNSCAN,DMCB,((R5),LWBLK),UNSCN1H,0                               
         CLI   DMCB,0                                                           
         BE    PROF23                                                           
*                                                                               
         MVI   UNSCN2H,L'UNSCN2H                                                
         GOTO1 (RF),(R1),,UNSCN2H                                               
         CLI   DMCB,0                                                           
         BE    PROF23                                                           
*                                                                               
         MVI   UNSCN3H,L'UNSCN3H                                                
         GOTO1 (RF),(R1),,UNSCN3H                                               
         CLI   DMCB,0                                                           
         BE    PROF23                                                           
                                                                                
         MVI   UNSCN4H,L'UNSCN4H                                                
         GOTO1 (RF),(R1),,UNSCN4H                                               
         CLI   DMCB,0                                                           
         BE    PROF23                                                           
*                                                                               
PROF23   MVC   IDPROF1,UNSCN1                                                   
         MVC   IDPROF2,UNSCN2                                                   
         MVC   IDPROF3,UNSCN3                                                   
         MVC   IDPROF4,UNSCN4                                                   
         XIT1                                                                   
         DROP  R4,R7                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD OUTPUT BLOCK FOR UNSCAN                                       *         
***********************************************************************         
         USING CPYELD,R4                                                        
OUTBLK   NTR1                                                                   
         SR    R6,R6                                                            
         IC    R6,0(R1)            GET DISP TO STATUS BYTE                      
         CLM   R6,1,CPYLN          TEST IF DATA IS WITHIN ELEMENT               
         BH    OUTBLKX             NO                                           
         LA    R6,CPYELD(R6)       POINT TO STATUS DATA                         
         LA    R7,1(R1)            R7=A(STATUS TABLE DATA)                      
*                                                                               
OUTNXT   CLI   0(R7),X'FF'         TEST FOR EOT                                 
         BE    OUTBLKX             YES                                          
         MVC   WORK(1),1(R7)       BIT FROM TABLE                               
         NC    WORK(1),0(R6)       STATUS BIT                                   
         BZ    OUTXT               NOT ON                                       
*                                                                               
         LA    R5,1(R5)            INCREMENT COUNT                              
         MVC   0(20,R3),SPACES     CLEAR ENTRY                                  
         MVC   0(10,R3),2(R7)      LEFT SIDE TO BLOCK                           
         CLI   0(R7),1             ONE SIDED                                    
         BE    *+10                YES                                          
         MVC   10(10,R3),12(R7)    RIGHT SIDE TO BLOCK                          
         LA    R3,20(R3)                                                        
*                                                                               
OUTXT    CLI   0(R7),1                                                          
         BE    *+8                                                              
         LA    R7,10(R7)                                                        
         LA    R7,12(R7)                                                        
         B     OUTNXT                                                           
*                                                                               
OUTBLKX  XIT1  REGS=(R3,R5)                                                     
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ID ENTRY TO TABLE                                               *         
***********************************************************************         
                                                                                
ADDID    NTR1  ,                                                                
         LA    R3,IDWRK            ADD ITEM(IDWRK) TO TABLE                     
W        USING IDD,R3                                                           
         OC    W.IDAGY,W.IDAGY                                                  
         BZ    XIT                 SKIP IF NO AGENCY CODE                       
         OC    W.IDS(IDMLNQ*IDSLNQ),W.IDS  OR IF NO SYSTEM ENTRIES              
         BZ    XIT                                                              
*                                                                               
         L     R7,AIDTAB                                                        
T        USING IDD,R7                                                           
                                                                                
ADDID02  CLI   T.IDS,X'FF'         TEST EOT                                     
         BE    ADDID10             YES, ADD NEW ENTRY                           
         CLC   T.IDAGY,W.IDAGY                                                  
         BE    *+12                SAME ALPHA CODE                              
ADDID03  LA    R7,IDLNQ(R7)                                                     
         B     ADDID02                                                          
*                                                                               
         LA    R0,IDSLNQ           NUMBER OF SYSTEM ELEMENTS                    
         LA    R4,T.IDS                                                         
         LA    R5,W.IDS                                                         
*                                                                               
ADDID04  OC    0(IDMLNQ,R4),0(R4)  TEST ALREADY HAVE SYSYTEM INFO               
         BZ    ADDID06             NO, ADD FROM NEW ENTRY                       
         OC    0(IDMLNQ,R5),0(R5)  TEST NEW ENTRY BLANK                         
         BZ    ADDID08             YES, SKIP IT                                 
         CLC   0(IDMLNQ,R4),0(R5)  SAME SYSTEM INFO                             
         BE    ADDID08                                                          
         OI    T.IDSTAT,IDSCON     TURN ON CONFLICT SWITCH                      
         MVC   T.IDERR(L'IDERR),W.IDID                                          
         OI    W.IDSTAT,IDSCON                                                  
         MVC   W.IDERR(L'IDERR),T.IDID                                          
*                                                                               
ADDID05  LA    R7,IDLNQ(R7)           FIND E-O-T                                
         CLI   0(R7),X'FF'                                                      
         BNE   ADDID05                                                          
         B     ADDID10                ADD TO END OF TABLE                       
*                                                                               
ADDID06  MVC   0(IDMLNQ,R4),0(R5)  MOVE NEW ITEM TO TABLE                       
ADDID08  LA    R4,IDMLNQ(R4)       TO NEXT SYSTEM ELEMENT                       
         LA    R5,IDMLNQ(R5)                                                    
         BCT   R0,ADDID04                                                       
         B     XIT                                                              
*                                                                               
ADDID10  MVC   0(IDLNQ,R7),0(R3)                                                
         LA    R7,IDLNQ(R7)                                                     
         MVI   0(R7),X'FF'                                                      
         L     R1,NUMIDS                                                        
         AH    R1,=H'1'                                                         
         ST    R1,NUMIDS                                                        
XIT      XIT1                                                                   
         DROP  T,W                                                              
         EJECT                                                                  
***********************************************************************         
* SET UP HEAD/MID LINES                                               *         
***********************************************************************         
                                                                                
         USING RPTCD,R5                                                         
HDLNS    MVC   TITLE(39),=C'ACCRES - COMPANY RESIDENCE DIRECTORY  -'            
         MVC   TITLE+40(L'RPTNME),RPTNME                                        
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'75'                                                      
         MVC   SPACING+2(2),=C'02'                                              
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         MVC   MID4,SPACES                                                      
         LA    R0,L'RPTFLD         NUMBER OF COLUMNS                            
         LA    R1,RPTFLD                                                        
         LA    R3,MID1+1                                                        
*                                                                               
HDLN3    SR    R4,R4                                                            
         CLI   0(R1),0                                                          
         BE    HDLN5                                                            
         IC    R4,0(R1)            FIELD NUMBER                                 
         BCTR  R4,0                                                             
         MHI   R4,FLDLNQ                                                        
         LA    R4,FLDTAB(R4)                                                    
         USING FLDD,R4                                                          
         MVC   0(10,R3),FLDHD1                                                  
         MVC   132(10,R3),FLDHD2                                                
         MVC   264(10,R3),FLDHD3                                                
         SR    RF,RF                                                            
         IC    RF,FLDRLEN                                                       
         LA    R3,0(RF,R3)                                                      
HDLN5    LA    R1,1(R1)                                                         
         BCT   R0,HDLN3                                                         
*                                                                               
         ZAP   ACCNT,=P'0'                                                      
         ZAP   SPCNT,=P'0'                                                      
         ZAP   PRCNT,=P'0'                                                      
         ZAP   NECNT,=P'0'                                                      
         BR    RE                                                               
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* COLUMN ROUTINES                                                     *         
***********************************************************************         
         USING IDD,R7                                                           
RTNNAME  MVC   0(36,R3),IDNAME     NAME TO REPORT                               
         LA    R3,38(R3)                                                        
         BR    RE                                                               
*                                                                               
RTNSP    ST    RE,SAVRE                                                         
         CLI   IDSCD,0                                                          
         BE    RTNSPX               SPOT                                        
         TM    IDSTAT,IDSCON                                                    
         BO    *+10                DON'T COUNT ERRORS                           
         AP    SPCNT,=P'1'                                                      
         MVC   0(5,R3),=C'SPXX/'    SPXX/F0                                     
         MVC   2(2,R3),IDSCHR                                                   
         GOTO1 HEXOUT,DMCB,IDSCD,5(R3),1                                        
RTNSPX   LA    R3,10(R3)                                                        
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
RTNPR    ST    RE,SAVRE                                                         
         CLI   IDPCD,0                                                          
         BE    RTNPRX               PRINT                                       
         TM    IDSTAT,IDSCON                                                    
         BO    *+10                                                             
         AP    PRCNT,=P'1'                                                      
         MVC   0(5,R3),=C'PRXX/'    PRXX/F0                                     
         MVC   2(2,R3),IDPCHR                                                   
         GOTO1 HEXOUT,DMCB,IDPCD,5(R3),1                                        
RTNPRX   LA    R3,10(R3)                                                        
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
RTNNE    ST    RE,SAVRE                                                         
         CLI   IDNCD,0                                                          
         BE    RTNNEX                 NETWORK                                   
         TM    IDSTAT,IDSCON                                                    
         BO    *+10                                                             
         AP    NECNT,=P'1'                                                      
         MVC   0(5,R3),=C'NEXX/'    NEXX/F0                                     
         MVC   2(2,R3),IDNCHR                                                   
         GOTO1 HEXOUT,DMCB,IDNCD,5(R3),1                                        
RTNNEX   LA    R3,10(R3)                                                        
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
RTNAC    ST    RE,SAVRE                                                         
         CLI   IDACD,0                                                          
         BE    RTNACX                 ACCPAK                                    
         TM    IDSTAT,IDSACC          ONLY COUNT ONES ON ACC                    
         BZ    *+10                                                             
         AP    ACCNT,=P'1'                                                      
         MVC   0(5,R3),=C'ACXX/'    ACXX/F0                                     
         MVC   2(2,R3),IDACHR                                                   
         GOTO1 HEXOUT,DMCB,IDACD,5(R3),1                                        
RTNACX   LA    R3,10(R3)                                                        
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                  HX AL                                        
RTNHX    ST    RE,SAVRE                                                         
         MVC   3(2,R3),IDAGY       AGENCY ALPHA                                 
         CLI   IDACD,0                                                          
         BE    RTNHXX              NO ACCPAK                                    
         GOTO1 HEXOUT,DMCB,IDACD,0(R3),1                                        
RTNHXX   LA    R3,8(R3)                                                         
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                  AL HX                                        
RTNAA    ST    RE,SAVRE                                                         
         MVC   0(2,R3),IDAGY       AGENCY ALPHA                                 
         CLI   IDACD,0                                                          
         BE    RTNAAX              NO ACCPAK                                    
         GOTO1 HEXOUT,DMCB,IDACD,3(R3),1                                        
RTNAAX   LA    R3,8(R3)                                                         
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                  ABBREV.                                      
RTNABBR  MVC   0(10,R3),IDABBR      AGENCY ALPHA                                
         LA    R3,11(R3)                                                        
         BR    RE                                                               
*                                  PRINCIPAL ID                                 
RTNID    EDIT  (B2,IDID),(5,0(R3))                                              
         TM    IDSTAT,IDSTWO                                                    
         BNO   *+8                                                              
         MVI   5(R3),C'*'          2 CHARACTER OFFICE                           
         LA    R3,10(R3)                                                        
         BR    RE                                                               
*                                   PROFILE                                     
RTNPROF  DS    0H                                                               
         ST    RE,SAVRE                                                         
         MVC   SPACING+2(2),=C'01'                                              
         MVC   0(L'IDPROF1,R3),IDPROF1                                          
         CLC   IDPROF2,SPACES                                                   
         BZ    RTNPROF5                                                         
         L     RF,PRINTER                                                       
         BASR  RE,RF                                                            
*                                                                               
         MVC   0(L'IDPROF2,R3),IDPROF2                                          
         CLC   IDPROF3,SPACES                                                   
         BZ    RTNPROF5                                                         
         L     RF,PRINTER                                                       
         BASR  RE,RF                                                            
*                                                                               
         MVC   0(L'IDPROF3,R3),IDPROF3                                          
         CLC   IDPROF4,SPACES                                                   
         BZ    RTNPROF5                                                         
         L     RF,PRINTER                                                       
         BASR  RE,RF                                                            
*                                                                               
         MVC   0(L'IDPROF3,R3),IDPROF4                                          
*                                                                               
RTNPROF5 L     RE,SAVRE                                                         
         MVC   SPACING+2(2),=C'02'                                              
         BR    RE                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* READ CONTROL FILE                                                   *         
***********************************************************************         
CTHIGH   ST    RE,SAVRE                                                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),CTFILE,KEY,AIO,0                     
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
CTSEQ    ST    RE,SAVRE                                                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),CTFILE,KEY,AIO,0                     
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* READ THE ACCOUNT FILE                                               *         
***********************************************************************         
ACHIGH   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),ACCDIR,DKEY,DIR                      
         CLI   DMCB+8,0                                                         
         BNE   ACHIGHX                                                          
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO,DMWORK                         
ACHIGHX  L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
         DS    0D                                                               
         DC    CL8'**D/C***'                                                    
AIO      DC    A(IO)                                                            
ASYSTAB  DC    A(SYSTAB)                                                        
AIDTAB   DC    A(IDTAB)                                                         
*                                                                               
PRINT    DC    V(PRINT)                                                         
PRINTER  DC    V(PRINTER)                                                       
CARDS    DC    V(CARDS)                                                         
SCANNER  DC    V(SCANNER)                                                       
UNSCAN   DC    V(UNSCAN)                                                        
DATCON   DC    V(DATCON)                                                        
XSORT    DC    V(XSORT)                                                         
HEXOUT   DC    V(HEXOUT)                                                        
HEXIN    DC    V(HEXIN)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
PDUMPER  DC    V(PDUMPER)                                                       
STXITER  DC    V(STXITER)                                                       
DATAMGR  DC    V(DATAMGR)                                                       
*                                                                               
ACFILEL  DC    C'NACCDIR NACCMST X'                                             
CTFILEL  DC    C'NCTFILE NGENDIR NGENFIL X'                                     
                                                                                
DMOPEN   DC    C'OPEN    '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
GETREC   DC    C'GETREC  '                                                      
CTFILE   DC    C'CTFILE  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
ACCOUNT  DC    C'ACCOUNT '                                                      
TESTFILE DC    C'N'                DEFAULT IS TO SKIP TEST FILES                
*                                                                               
DA       DS    F                                                                
DKEY     DS    CL42                                                             
DIR      DS    CL60                                                             
DMWORK   DC    24F'0'                                                           
*                                                                               
NOOP#    DC    AL1(0)                                                           
ISYS#    DC    AL1(0)                                                           
*                                                                               
DAYS     DC    AL1(1),C'MON'                                                    
         DC    AL1(2),C'TUE'                                                    
         DC    AL1(3),C'WED'                                                    
         DC    AL1(4),C'THU'                                                    
         DC    AL1(5),C'FRI'                                                    
         DC    AL1(6),C'SAT'                                                    
         DC    AL1(7),C'SUN'                                                    
DAYSLNQ  EQU   (*-DAYS)/4                                                       
         EJECT                                                                  
***********************************************************************         
* ACCOUNT PROFILE STATUS BYTES                                        *         
***********************************************************************         
COMSTAT  DC    AL1(CPYSTAT1-CPYELD)                                             
         DC    X'01',AL1(CPYSIOMR),CL10'MTCHORD   '                             
         DC    X'01',AL1(CPYSCIVE),CL10'CKINV     '                             
         DC    X'01',AL1(CPYSOROE),CL10'OFFICE    '                             
         DC    X'01',AL1(CPYSCOST),CL10'COST      '                             
         DC    X'01',AL1(CPYSDISC),CL10'CD        '                             
         DC    X'01',AL1(CPYSGENA),CL10'GA        '                             
         DC    X'02',AL1(CPYSNOJL),CL10'LBLS      ',CL10'N'                     
         DC    X'02',AL1(CPYSNOET),CL10'ESTA      ',CL10'N'                     
         DC    X'FF'                                                            
*                                                                               
COMSTA2  DC    AL1(CPYSTAT2-CPYELD)                                             
         DC    X'02',AL1(CPYSETPP),CL10'ESTA      ',CL10'PLN'                   
         DC    X'02',AL1(CPYSETAC),CL10'ESTA      ',CL10'ACT'                   
         DC    X'01',AL1(CPYSEBIF),CL10'BILLEST   '                             
         DC    X'02',AL1(CPYSETDO),CL10'ESTA      ',CL10'DIF'                   
         DC    X'01',AL1(CPYSCKDP),CL10'CHKDUP    '                             
         DC    X'01',AL1(CPYSVENR),CL10'VEND      '                             
         DC    X'02',AL1(CPYSCACA),CL10'SJCNTRA   ',CL10'SC'                    
         DC    X'01',AL1(CPYSERTP),CL10'PAYEST    '                             
         DC    X'FF'                                                            
*                                                                               
COMSTA3  DC    AL1(CPYSTAT3-CPYELD)                                             
         DC    X'02',AL1(CPYSSXCC),CL10'SXCNTRA   ',CL10'CL'                    
         DC    X'01',AL1(CPYSWO14),CL10'WO        '                             
         DC    X'02',AL1(CPYSPC1C),CL10'PC        ',CL10'1C'                    
         DC    X'02',AL1(CPYSOPBM),CL10'BA        ',CL10'Y'                     
         DC    X'02',AL1(CPYSCA22),CL10'SECNTRA   ',CL10'SC'                    
         DC    X'02',AL1(CPYSPCSJ),CL10'PC        ',CL10'SJ'                    
         DC    X'01',AL1(CPYSDPST),CL10'DPS       '                             
         DC    X'02',AL1(CPYSBSEC),CL10'BS        ',CL10'Y'                     
         DC    X'FF'                                                            
*                                                                               
COMSTA4  DC    AL1(CPYSTAT4-CPYELD)                                             
         DC    X'02',AL1(CPYSPESK),CL10'%EST      ',CL10'SK'                    
         DC    X'02',AL1(CPYSOV12),CL10'BBD       ',CL10'Y'                     
         DC    X'01',AL1(CPYSNPRD),CL10'NEWPROD   '                             
         DC    X'01',AL1(CPYSMINT),CL10'MI        '                             
         DC    X'01',AL1(CPYSOFF2),CL10'NEWOFF    '                             
         DC    X'FF'                                                            
*                                                                               
COMSTA5  DC    AL1(CPYSTAT5-CPYELD)                                             
         DC    X'02',AL1(CPYSOFPL),CL10'OFF       ',CL10'P&&L'                  
         DC    X'02',AL1(CPYSBAPR),CL10'APP       ',CL10'REG'                   
         DC    X'02',AL1(CPYSBAPE),CL10'APP       ',CL10'EFF'                   
         DC    X'01',AL1(CPYSNCST),CL10'NEWCOST   '                             
         DC    X'01',AL1(CPYSEXPP),CL10'EXPROD    '                             
         DC    X'01',AL1(CPYSVEND),CL10'VENCOP    '                             
         DC    X'01',AL1(CPYAPGS),CL10'APG$'                                    
         DC    X'FF'                                                            
*                                                                               
COMSTA6  DC    AL1(CPYSTAT6-CPYELD)                                             
         DC    X'01',AL1(CPYSADVP),CL10'ADVPTR    '                             
         DC    X'01',AL1(CPYSRAPP),CL10'RAPTR     '                             
         DC    X'FF'                                                            
*                                                                               
COMSTA7  DC    AL1(CPYSTAT7-CPYELD)                                             
         DC    X'01',AL1(CPYSAGRP),CL10'ACTGRPS   '                             
         DC    X'01',AL1(CPYSJTIM),CL10'JTIME     '                             
         DC    X'01',AL1(CPYSL1NA),CL10'PERAN     '                             
         DC    X'01',AL1(CPYSNEWB),CL10'NEWBILL   '                             
         DC    X'FF'                                                            
*                                                                               
COMSTA8  DC    AL1(CPYSTAT8-CPYELD)                                             
         DC    X'01',AL1(CPYSRLOG),CL10'RLOGO     '                             
         DC    X'FF'                                                            
*                                                                               
COMCD    DC    AL1(CPYCDC-CPYELD)                                               
         DC    X'02',C'N',CL10'DISC       ',CL10'N'                             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* TABLE OF SYSTEM NUMBERS                                             *         
***********************************************************************         
                                                                                
SYSLIST  DS    0XL2  SYSTEM NUMBER/DISPLACEMENT IN IDTAB                        
         DC    AL1(CTWKACC,IDA-IDD)          ACCPAK                             
         DC    AL1(CTWKSPOT,IDS-IDD)         SPOTPAK                            
         DC    AL1(CTWKNET,IDN-IDD)          NETPAK                             
         DC    AL1(CTWKPRNT,IDP-IDD)         PRINTPAK                           
*&&UK*&& DC    X'04',AL1(IDM-IDD)            MEDLINE                            
         DC    X'FF'                                                            
***********************************************************************         
* FIELD EQUATES                                                       *         
***********************************************************************         
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
HX       EQU   1                   HEXCOMP/ALPHA                                
AA       EQU   2                   ALPHA/HEXCOMP                                
ABBR     EQU   3                   ABBREVIATION                                 
NAME     EQU   4                   COMPANY NAME                                 
ID       EQU   5                   PRINCIPAL ID                                 
ACC      EQU   6                   ACCFILE/ACCPAK CODE                          
SPOT     EQU   7                   SPOTFILE/SPOTPAK CODE                        
PRNT     EQU   8                   PRINTFILE/PRINTPAK CODE                      
NET      EQU   9                   NETFILE/NETPAK CODE                          
PROF     EQU   10                  ACCOUNT PROFILE                              
         EJECT                                                                  
***********************************************************************         
* TABLE OF VALID REPORT SEQUENCES(RPTD)                               *         
***********************************************************************         
RPTAB    DC    C'HC'                                                            
         DC    CL25'HEXCOMP SEQUENCE'                                           
         DC    AL1(HX,ABBR,NAME,ID,ACC,SPOT,PRNT,NET)                           
*                                                                               
         DC    C'AB'                                                            
         DC    CL25'ABBR SEQUENCE'                                              
         DC    AL1(ABBR,NAME,AA,ID,ACC,SPOT,PRNT,NET)                           
*                                                                               
         DC    C'AA'                                                            
         DC    CL25'ALPHA SEQUENCE'                                             
         DC    AL1(AA,ABBR,NAME,ID,ACC,SPOT,PRNT,NET)                           
*                                                                               
         DC    C'AC'                                                            
         DC    CL25'ACCPAK SEQUENCE'                                            
         DC    AL1(ACC,NAME,ABBR,HX,ID,SPOT,PRNT,NET)                           
*                                                                               
         DC    C'AL'                                                            
         DC    CL25'NAME SEQUENCE'                                              
         DC    AL1(NAME,ABBR,HX,ID,ACC,SPOT,PRNT,NET)                           
*                                                                               
         DC    C'AP'                                                            
         DC    CL25'ACCOUNT PROFILE'                                            
         DC    AL1(NAME,ABBR,HX,ID,ACC,PROF,0,0)                                
*                                                                               
         DC    C'SP'                                                            
         DC    CL25'SPOTPAK SEQUENCE'                                           
         DC    AL1(SPOT,NAME,ABBR,HX,ID,ACC,PRNT,NET)                           
*                                                                               
         DC    C'PR'                                                            
         DC    CL25'PRINT SEQUENCE'                                             
         DC    AL1(PRNT,NAME,ABBR,HX,ID,ACC,SPOT,NET)                           
*                                                                               
         DC    C'NE'                                                            
         DC    CL25'NETPAK SEQUENCE'                                            
         DC    AL1(NET,NAME,ABBR,HX,ID,ACC,SPOT,PRNT)                           
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* TABLE OF FIELD DEFINITIONS(FLDD)                                    *         
***********************************************************************         
FLDTAB   DC    AL1(HX)                                                          
         DC    CL10'CODES'                                                      
         DC    CL10'HX AL'                                                      
         DC    CL10'-----'                                                      
         DC    AL1(IDACD-IDD)                                                   
         DC    AL1(L'IDACD)                                                     
         DC    AL1(8)                                                           
         DC    AL3(RTNHX)                                                       
*                                                                               
         DC    AL1(AA)                                                          
         DC    CL10'CODES'                                                      
         DC    CL10'AL HX'                                                      
         DC    CL10'-----'                                                      
         DC    AL1(IDAGY-IDD)                                                   
         DC    AL1(L'IDAGY)                                                     
         DC    AL1(8)                                                           
         DC    AL3(RTNAA)                                                       
*                                                                               
         DC    AL1(ABBR)                                                        
         DC    CL10' '                                                          
         DC    CL10'ABBREV'                                                     
         DC    CL10'------'                                                     
         DC    AL1(IDABBR-IDD)                                                  
         DC    AL1(L'IDABBR)                                                    
         DC    AL1(11)                                                          
         DC    AL3(RTNABBR)                                                     
*                                                                               
         DC    AL1(NAME)                                                        
         DC    CL10' '                                                          
         DC    CL10'NAME'                                                       
         DC    CL10'----'                                                       
         DC    AL1(IDNAME-IDD)                                                  
         DC    AL1(L'IDNAME)                                                    
         DC    AL1(38)                                                          
         DC    AL3(RTNNAME)                                                     
*                                                                               
         DC    AL1(ID)                                                          
         DC    CL10' '                                                          
         DC    CL10'ID NO.'                                                     
         DC    CL10'------'                                                     
         DC    AL1(IDID-IDD)                                                    
         DC    AL1(L'IDID)                                                      
         DC    AL1(10)                                                          
         DC    AL3(RTNID)                                                       
*                                                                               
         DC    AL1(ACC)                                                         
         DC    CL10'ACCPAK'                                                     
         DC    CL10'SYS/CD'                                                     
         DC    CL10'------'                                                     
         DC    AL1(IDA-IDD)                                                     
         DC    AL1(IDMLNQ)                                                      
         DC    AL1(10)                                                          
         DC    AL3(RTNAC)                                                       
*                                                                               
         DC    AL1(SPOT)                                                        
         DC    CL10'SPOTPAK'                                                    
         DC    CL10'SYS/CD'                                                     
         DC    CL10'------'                                                     
         DC    AL1(IDS-IDD)                                                     
         DC    AL1(IDMLNQ)                                                      
         DC    AL1(10)                                                          
         DC    AL3(RTNSP)                                                       
*                                                                               
         DC    AL1(PRNT)                                                        
         DC    CL10'PRINTPAK'                                                   
         DC    CL10'SYS/CD'                                                     
         DC    CL10'------'                                                     
         DC    AL1(IDP-IDD)                                                     
         DC    AL1(IDMLNQ)                                                      
         DC    AL1(10)                                                          
         DC    AL3(RTNPR)                                                       
*                                                                               
         DC    AL1(NET)                                                         
         DC    CL10'NETPAK'                                                     
         DC    CL10'SYS/CD'                                                     
         DC    CL10'------'                                                     
         DC    AL1(IDN-IDD)                                                     
         DC    AL1(IDMLNQ)                                                      
         DC    AL1(10)                                                          
         DC    AL3(RTNNE)                                                       
*                                                                               
         DC    AL1(PROF)                                                        
         DC    CL10'      '                                                     
         DC    CL10'PROFILE'                                                    
         DC    CL10'------'                                                     
         DC    AL1(IDPROF1-IDD)                                                 
         DC    AL1(0)                                                           
         DC    AL1(45)                                                          
         DC    AL3(RTNPROF)                                                     
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         DC    CL8'**W/S***'                                                    
DMPLIST  DS    0F                                                               
         DS    6F                                                               
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    CL1                                                              
NUMIDS   DS    F                                                                
SORTDSP  DS    CL1                                                              
SVREG    DS    5F                                                               
SAVRE    DS    F                                                                
ACCNT    DS    PL3                                                              
SPCNT    DS    PL3                                                              
PRCNT    DS    PL3                                                              
NECNT    DS    PL3                                                              
CARD     DS    CL80                                                             
*                                                                               
UNSCN1H  DS    0CL(L'IDPROF1+8)                                                 
         DS    CL8                                                              
UNSCN1   DS    CL(L'IDPROF1)                                                    
*                                                                               
UNSCN2H  DS    0CL(L'IDPROF2+8)                                                 
         DS    CL8                                                              
UNSCN2   DS    CL(L'IDPROF2)                                                    
*                                                                               
UNSCN3H  DS    0CL(L'IDPROF3+8)                                                 
         DS    CL8                                                              
UNSCN3   DS    CL(L'IDPROF3)                                                    
*                                                                               
UNSCN4H  DS    0CL(L'IDPROF4+8)                                                 
         DS    CL8                                                              
UNSCN4   DS    CL(L'IDPROF4)                                                    
*                                                                               
WORK     DS    CL100                                                            
IDWRK    DS    CL(IDLNQ)                                                        
CRDTAB   DS    (25*CRDLEQU)C                                                    
         DS    0D                                                               
KEY      DS    CL42                                                             
         DS    0D                                                               
KEYSAVE  DS    CL42                                                             
*                                                                               
LWBLK    DS    25CL32                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MEDIAOCEAN CONTROL BLOCKS                                           *         
***********************************************************************         
         DS    0D                                                               
         DC    C'*SSB*SSB*SSB*SSB'                                              
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSB+(SSOSTAT2-SSBD)                                              
         DC    AL1(SSOSNRCV)       SET RECOVERY OFF                             
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    X'0A'                                                            
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* IO AREAS AND TABLES                                                 *         
***********************************************************************         
         DS    0D                                                               
         DC    CL8'***IO***'                                                    
IO       DS    CL2000                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'*SYSTAB*'                                                    
SYSTAB   DC    X'FF'                                                            
         DS    (SYLLNQ*250)C                                                    
*                                                                               
         DS    0D                                                               
         DC    CL8'*IDTAB**'                                                    
IDTAB    DC    X'FF'                                                            
         DS    (IDLNQ*MAXTAB)C                                                  
MAXTAB   EQU   1000                                                             
         DS    0D                                                               
NOOPLIST DS    30CL(L'SYLNAME)                                                  
         DS    0D                                                               
ISYSLIST DS    30CL(L'SYLNAME)                                                  
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER ID RECORD DETAILS(IDTAB)                             *         
***********************************************************************         
IDD      DSECT                                                                  
IDS      DS    0C                  SPOT                                         
IDSNUM   DS    CL1                 SYSTEM NUMBER                                
IDSCD    DS    CL1                 AGENCY CODE                                  
IDSCHR   DS    CL2                 SYSTEM CHARACTER                             
IDMLNQ   EQU   *-IDS               LENGTH OF SYSTEM ENTRY                       
*                                                                               
IDN      DS    0C                  NETPAK                                       
IDNNUM   DS    CL1                 SYSTEM NUMBER                                
IDNCD    DS    CL1                 AGENCY CODE                                  
IDNCHR   DS    CL2                 SYSTEM CHARACTER                             
*                                                                               
IDP      DS    0C                  PRINT                                        
IDPNUM   DS    CL1                 SYSTEM NUMBER                                
IDPCD    DS    CL1                 AGENCY CODE                                  
IDPCHR   DS    CL2                 SYSTEM CHARACTER                             
         ORG   IDP                                                              
IDM      DS    0C                  MEDLINE                                      
IDMNUM   DS    CL1                 SYSTEM NUMBER                                
IDMCD    DS    CL1                 AGENCY CODE                                  
IDMCHR   DS    CL2                 SYSTEM CHARACTER                             
*                                                                               
IDA      DS    0C                  ACCOUNT                                      
IDANUM   DS    CL1                 SYSTEM NUMBER                                
IDACD    DS    CL1                 AGENCY CODE                                  
IDACHR   DS    CL2                 SYSTEM CHAR : NEW FILES 2 CHAR               
IDSLNQ   EQU   (*-IDD)/IDMLNQ      NUMBER OF SYSTEM ENTRIES                     
*                                                                               
IDAGY    DS    CL2                 AGENCY ALPHA                                 
IDID     DS    CL2                 ID NUMBER                                    
IDABBR   DS    CL10                ABBREVIATION                                 
IDNAME   DS    CL36                AGENCY NAME (FROM ACCPAK)                    
IDSTAT   DS    XL1                 STATUS OF ENTRY                              
IDSCON   EQU   X'80'                CONFLICT ON CONTROL FILE                    
IDSACC   EQU   X'40'                ON ACCOUNT FILE                             
IDSTWO   EQU   X'20'                2 CHARACTER OFFICE                          
IDERR    DS    XL2                 CONFLICTING ID NUMBER                        
*                                                                               
IDPROF1  DS    CL45                PROFILE INFO                                 
IDPROF2  DS    CL45                ADDITIONAL PROFILE INFO                      
IDPROF3  DS    CL45                ADDITIONAL PROFILE INFO                      
IDPROF4  DS    CL45                ADDITIONAL PROFILE INFO                      
IDLNQ    EQU   *-IDD               LENGTH OF TABLE ENTRY                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER LIST OF SYSTEM FILES(SYSTAB)                         *         
***********************************************************************         
SYLD     DSECT                                                                  
SYLNAME  DS    CL7                 NAME (ACC1,SPOTB...ETC)                      
SYLSE    DS    CL1                 SE NUMBER                                    
SYLNUM   DS    CL1                 SYSTEM NUMBER(1,0B ETC.)                     
SYLDSP   DS    CL1                 DISPLACEMENT INTO IDTAB                      
SYLCHR   DS    CL2                 NEW FILES ARE TWO CHAR                       
SYLFILT  DS    CL1                 FILTER                                       
SYLLNQ   EQU   *-SYLD                                                           
                                                                                
***********************************************************************         
* DSECT TO COVER TABLE OF INPUT CARDS(CRDTAB)                         *         
***********************************************************************         
CRDD     DSECT                                                                  
CRDSEQ   DS    CL2                 SEQUENCE AL,HC, ETC...                       
CRDFLT   DS    CL5                 ACCPAK SYSTEM FILTER ACCX                    
CRDLEQU  EQU   *-CRDD                                                           
                                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER DAY TABLE                                            *         
***********************************************************************         
DAYSD    DSECT                                                                  
DAYNUM   DS    XL1                 CORRESPONDING DAY NUMBER FROM GETDAY         
DAYNAME  DS    CL3                 ABBREVIATED DAY                              
DAYQ     EQU   *-DAYSD                                                          
                                                                                
***********************************************************************         
* DSECT TO COVER REPORT CODE TABLE(RPTAB)                             *         
***********************************************************************         
RPTD     DSECT                                                                  
RPTCD    DS    CL2                 REPORT CODE                                  
RPTNME   DS    CL25                REPORT NAME                                  
RPTFLD   DS    CL8                 FIELD EQUATES(COLUMNS)                       
RPTLNQ   EQU   *-RPTD                                                           
                                                                                
***********************************************************************         
* DSECT TO COVER FIELD DEFINITION RECORDS(FLDTAB)                     *         
***********************************************************************         
FLDD     DSECT                                                                  
FLDNUM   DS    CL1                 FIELD NUMBER                                 
FLDHD1   DS    CL10                HEADLINE 1                                   
FLDHD2   DS    CL10                         2                                   
FLDHD3   DS    CL10                         3                                   
FLDDSP   DS    AL1                 DISPLACEMENT TO FIELD                        
FLDSLN   DS    AL1                 LENGTH FOR SORT                              
FLDRLEN  DS    AL1                 LENGTH FOR REPORT                            
FLDRTN   DS    AL3                 SPECIAL PROCESSING ROUTINE                   
FLDLNQ   EQU   *-FLDD                                                           
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         ORG   SSBD                                                             
       ++INCLUDE FASSBOFF                                                       
         ORG                                                                    
         PRINT ON                                                               
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACCRES    01/19/14'                                      
         END                                                                    
