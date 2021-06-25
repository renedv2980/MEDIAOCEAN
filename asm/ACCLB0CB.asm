*          DATA SET ACCLB0CB   AT LEVEL 022 AS OF 05/01/02                      
*PHASE T6210CB                                                                  
CLB0C    TITLE '- BILL PROGRAM - CONTROL (FORMAT) - LIST VERSION'               
CLB0C    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CLBC**,R8,R7,RR=RE                                           
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING CWORKD,RC                                                        
         L     R6,ALSVALS                                                       
         USING LSVALSD,R6                                                       
         USING TLSTD,LSTLST                                                     
         USING FBLKD,CFMTBLK                                                    
         USING BOFELD,FBBOFEL                                                   
         ST    RE,BORELO                                                        
                                                                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     SCRLAST             LAST FOR THIS SCREEN                         
         B     LINFRST             FIRST FOR VALIDATE THIS LINE                 
         B     VALCLM              VALIDATE COLUMN                              
         B     LINLAST             LAST FOR VALIDATE THIS LINE                  
         B     DISCLMS             DISPLAY COLUMNS                              
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     EXITY               SET UP MY OWN HEADING                        
         B     EXITY               VALIDATE LINE SELECTION                      
         B     EXITY               DISPLAY (SUB-) TOTAL ROUTINE                 
         B     EXITY               DISPLAY SCREEN TOTAL ROUTINE                 
         B     PFKRTN              PF KEY ROUTINES                              
         EJECT                                                                  
***********************************************************************         
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
                                                                                
SCRFRST  TM    BCINDS1,BCINACT     TEST 1ST TIME                                
         BO    *+12                                                             
         TM    BCINDS2,BCINTRS                                                  
         BZ    *+8                                                              
         OI    CINDS,CIFST                                                      
         TM    CINDS,CIFST                                                      
         BZ    SCRF14                                                           
                                                                                
         XC    SVWORK,SVWORK                                                    
         CLI   CUCTRY,CTRYGER      TEST NOT GERMANY                             
         BE    SCRF10                                                           
         XC    CONLNGW,CONLNGW     REMOVE LANGUAGE FIELD                        
         XC    CONLNGX,CONLNGX                                                  
         OI    CONLNGH+FHATD,FHATPR                                             
                                                                                
SCRF10   DS    0H                                                               
         MVC   THISFORM,CSFORMAT                                                
         MVC   THISLANG,CSFMLANG                                                
         CLI   TLACT,ACTLFT        TEST COME FROM FORMAT LIST SCREEN            
         BE    *+16                                                             
         CLI   CSFORMAT,0          TEST HAVE BILLING FORMAT CODE                
         BE    SCRF14                                                           
         B     SCRF12                                                           
*                                                                               
         LA    R2,TLCDIR                                                        
         MVC   THISFKEY,PBCKFMT-PBCKEY(R2)                                      
SCRF12   EDIT  THISFORM,(3,CONFMT),ALIGN=LEFT                                   
         GOTO1 DISLNG,THISLANG                                                  
         B     SCRF20                                                           
                                                                                
SCRF14   GOTO1 AFVAL,CONFMTH                                                    
         BE    SCRF16                                                           
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$ENTKY)                                           
         LA    RE,CONFMTH                                                       
         ST    RE,FVADDR                                                        
         B     EXITN                                                            
                                                                                
SCRF16   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         TM    FVIIND,FVINUM                                                    
         BZ    EXITN                                                            
         OC    BCFULL(3),BCFULL                                                 
         BNZ   EXITN                                                            
         CLI   BCFULL+3,0                                                       
         BE    EXITN                                                            
         MVC   THISFORM,BCFULL+3                                                
                                                                                
         USING PBCRECD,IOKEY                                                    
SCRF20   XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,THISFORM                                                 
         GOTO1 VALLNG                                                           
         BNE   EXITN                                                            
         MVC   THISLANG,PBCKLANG                                                
         GOTO1 AIO,IO1+IOACCMST+IORD                                            
         BNE   ERRRECNF                                                         
         GOTO1 AFMTBLK,BOPARM,('FBGET',CFMTBLK),AIO1                            
                                                                                
         GOTO1 AFVAL,CONNAMH                                                    
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(X'20',AIO1),0                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,12(R1)                                                        
         XR    RE,RE                                                            
         IC    RE,1(RF)                                                         
         SH    RE,=H'3'                                                         
         CLC   THISFKEY,SVFKEY     TEST SAME FMT/LANG                           
         BNE   SCRF24                                                           
         CLI   FVILEN,0                                                         
         BE    SCRF24                                                           
         CLC   FVILEN,1(RF)        IS LENGTH DIFFERENT ?                        
         BE    SCRF22                                                           
         EX    RE,*+8                                                           
         BE    SCRF26                                                           
         CLC   FVIFLD(0),2(RF)                                                  
SCRF22   OI    SVINDS,SVIPWRT                                                   
         B     SCRF26                                                           
SCRF24   XC    CONNAM,CONNAM                                                    
         EX    RE,*+4                                                           
         MVC   CONNAM(0),2(RF)                                                  
         OI    CONNAMH+6,X'80'     TRANSMIT                                     
                                                                                
SCRF26   GOTO1 GETSECT                                                          
         BE    SCRF30                                                           
         MVC   FVMSGNO,=AL2(AE$NOSEC)                                           
         GOTO1 SUBNUM,THISFORM                                                  
         LA    R1,CONFMTH                                                       
         ST    R1,FVADDR                                                        
         LA    R2,FVPARMS                                                       
         XR    RF,RF                                                            
         ICM   RF,1,0(R2)                                                       
         BZ    *+8                                                              
         BXH   R2,RF,*-8                                                        
         CLI   THISLANG,0                                                       
         BNE   *+14                                                             
         MVC   0(2,R2),=X'0240'                                                 
         B     EXITN                                                            
         IC    RE,CONLNGH+5                                                     
         LA    RE,1(RE)                                                         
         STC   RE,0(R2)                                                         
         MVC   1(L'CONLNG,R2),CONLNG                                            
         B     EXITN                                                            
                                                                                
SCRF30   MVC   SVOPS,LSOPS                                                      
         XC    LSOPTION,LSOPTION                                                
         L     RE,=A(VALOPT)                                                    
         A     RE,BORELO                                                        
         ST    RE,AOVERVAL                                                      
         GOTO1 AFVAL,BASOPTH                                                    
         MVC   AOVEROUT,ALSVALS                                                 
         XC    BOWORK2,BOWORK2                                                  
         MVC   BOWORK2(DEFCLML),DEFCLM                                          
         GOTO1 AVALOPT,BOPARM,OPTTAB,BOWORK2,0                                  
         BNE   EXITL                                                            
         CLC   SVOPS,LSOPS         TEST CHANGE IN OPTIONS                       
         BNE   EXITH                                                            
         CLC   THISFKEY,SVFKEY     TEST CHANGE IN FORMAT KEY                    
         BE    EXITY                                                            
         MVC   SVFKEY,THISFKEY                                                  
         MVC   CSFORMAT,THISFORM                                                
         MVC   CSFMLANG,THISLANG                                                
         B     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LIST OF SECT NO.S AND NAMES VALID FOR THIS FORMAT  *         
***********************************************************************         
                                                                                
GETSECT  NTR1  ,                                                                
         CLC   THISFKEY,SVFKEY                                                  
         BE    EXITY                                                            
         LA    R3,SECTAB           R3=A(SECTAB)                                 
         USING SECTABD,R3                                                       
         XC    SECTNUM(SECTABLN),SECTNUM                                        
         LA    R2,IOKEY            BUILD SECDEF RECORD KEY AND READ FOR         
                                                                                
         USING PBSRECD,R2          FIRST SECTION ON THIS FORMAT                 
         XC    PBSKEY,PBSKEY                                                    
         MVI   PBSKTYP,PBSKTYPQ                                                 
         MVC   PBSKCPY,CUABIN                                                   
         MVI   PBSKSUB,PBSKDEFQ                                                 
         MVC   PBSKFMT,THISFORM                                                 
         GOTO1 AIO,IO3+IOACCDIR+IOHIGH                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
GETSEC02 CLC   THISFORM,PBSKFMT                                                 
         BNE   GETSEC10                                                         
         CLC   THISLANG,PBSKLANG                                                
         BE    GETSEC06                                                         
GETSEC04 GOTO1 AIO,IO3+IOACCDIR+IOSEQ                                           
         B     GETSEC02                                                         
                                                                                
GETSEC06 MVC   SECTNUM,PBSKSEC     PUT SECTION NUMBER IN TABLE                  
         GOTO1 AIO,IO3+IOGET+IOACCMST                                           
         XC    BOHALF1,BOHALF1     BUILD HELLO SEARCH ARGUMENT                  
         MVI   BOHALF1,FFTTBSNM                                                 
         MVC   BOHALF1+1(1),SECTNUM                                             
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),(X'DB',AIO3),(2,BOHALF1)             
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   SECTNAME,=C'*ADD* '  TEMPORARY MEASURE, DIE WHEN LIVE            
         B     GETSEC08                                                         
                                                                                
         L     RF,12(R1)                                                        
         USING FFTEL,RF                                                         
         MVC   SECTNAME,BCSPACES                                                
         ZIC   RE,FFTDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   SECTNAME(0),FFTDATA SAVE SECTION NAME IN TABLE                   
         DROP  RF                                                               
GETSEC08 LA    R3,SECTABLN(R3)     BUMP TO AND CLEAR NEXT ENTRY                 
         XC    SECTNUM(SECTABLN),SECTNUM                                        
         B     GETSEC04                                                         
                                                                                
GETSEC10 LA    R3,SECTAB           TEST ANY SECTIONS FOUND                      
         CLI   SECTNUM,0                                                        
         BE    EXITN                                                            
         B     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
                                                                                
LSTFRST  DS    0H                                                               
         USING PBCRECD,IOKEY                                                    
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* GET FIRST/NEXT RECORD FOR TSAR LIST                                 *         
***********************************************************************         
                                                                                
GETFRST  MVC   PBCKFMT,THISFORM                                                 
         MVC   PBCKLANG,CSFMLANG                                                
         GOTO1 AIO,IO1+IOACCMST+IORD                                            
         BNE   EXITN                                                            
         MVI   PBCKSEQ,1                                                        
         GOTO1 AIO,IO2+IOACCMST+IORD                                            
         BE    GFRST02                                                          
         L     RF,AIO2                                                          
C        USING PBCRECD,RF          SET CONTINUATION RECORD TO BE EMPTY          
         MVC   C.PBCRLEN,=AL2(PBCRFST+1-PBCRECD)                                
         MVI   C.PBCRFST,0                                                      
         DROP  C                                                                
GFRST02  OI    CINDS,CIFTYP                                                     
         B     GNXT02                                                           
                                                                                
GETNEXT  L     R2,SVR2                                                          
         L     R3,SVR3                                                          
         L     R4,SVR4                                                          
         B     GNXT04                                                           
                                                                                
GNXT02   L     R2,AIO1                                                          
         L     R3,AFMTTAB          R3=A(FMTTAB)                                 
         USING FMTTABD,R3                                                       
         LA    R4,SECTAB           R4=A(SECTAB)                                 
         USING SECTABD,R4                                                       
         USING BLFELD,TLBLFEL                                                   
                                                                                
GNXT04   CLI   LSSECT,0                                                         
         BE    GNXT06                                                           
         OI    CINDS,CIFTYP        IF ONE SECTION THEN EACH IS FIRST            
         CLC   SECTNUM,LSSECT                                                   
         BE    GNXT06                                                           
         LA    R4,SECTABLN(R4)                                                  
         CLI   SECTNUM,0                                                        
         BNE   GNXT04                                                           
         LA    R4,SECTAB                                                        
         OI    CINDS,CIBLNK        INSERT A BLANK LINE                          
         LA    R3,FMTTABL(R3)                                                   
         B     GNXT04                                                           
                                                                                
GNXT06   CLI   FMTFLD,EOT                                                       
         BE    EXITN                                                            
         GOTO1 TESTSC,FMTSYS       TEST SYSTEM COUNTRY FILTER                   
         BE    *+12                                                             
         LA    R3,FMTTABL(R3)                                                   
         B     GNXT04                                                           
                                                                                
         XC    TLNUM,TLNUM                                                      
         XC    TLREC(TLFT1LNQ),TLREC                                            
         MVC   TLRLEN,=AL2(TLFT1LNQ)                                            
         XC    TLKSRT,TLKSRT                                                    
         USING SORTD,TLKSRT                                                     
                                                                                
         MVC   SORTFMT,FMTFLD      PUT FMTFLD IN SORTFMT & SORTFMT2             
         MVC   SORTFMT2,FMTFLD                                                  
         MVC   SORTSEC,SECTNUM     PUT IN SECTION NUMBER                        
         CLI   SORTFMT,BLFFNET2    IF NET2 USE NETQ AS SRTFMT                   
         BNE   *+8                                                              
         MVI   SORTFMT,BLFFNETQ                                                 
         CLI   SORTFMT,BLFFGRS2    IF GRS2 USE GRSQ AS SRTFMT                   
         BNE   *+8                                                              
         MVI   SORTFMT,BLFFGRSQ                                                 
                                                                                
         TM    CINDS,CIBLNK        TEST IF THIS IS A BLANK LINE                 
         BZ    *+12                                                             
         OI    TLINDS1,TLIPRO                                                   
         B     GNXTX2                                                           
         TM    CINDS,CIFTYP        TEST FIRST RECORD FOR THIS DATA TYPE         
         BZ    *+8                                                              
         OI    TLFT1IND,CIFTYP                                                  
                                                                                
         MVI   BLFEL,BLFELQ        SET FIRST PART OF BLFEL                      
         MVI   BLFLN,BLFLNQ                                                     
         MVC   BLFFLD,SORTFMT2                                                  
         MVC   BLFSECT,SORTSEC                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'FMTFLD),FMTFLD                                            
         MVC   WORK+L'FMTFLD(L'SECTNUM),SECTNUM                                 
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('BLFELQ',AIO1),(2,WORK)             
         CLI   12(R1),0                                                         
         BE    GNXT10                                                           
         GOTO1 VHELLO,BOPARM,(C'G',ACCMST),('BLFELQ',AIO2),(2,WORK)             
         CLI   12(R1),0                                                         
         BNE   *+14                                                             
GNXT10   L     RF,12(R1)                                                        
         MVC   BLFEL(BLFLNQ),0(RF)                                              
         MVC   TLSECNAM,SECTNAME                                                
         MVC   TLFMTTAB(FMTTABL),0(R3)                                          
         LA    R4,SECTABLN(R4)                                                  
         CLI   SECTNUM,EOT                                                      
         BNE   GNXTX1                                                           
         LA    R4,SECTAB                                                        
         OI    CINDS,CIFTYP        NEXT REC WILL BE 1ST FOR DATA TYPE           
         OI    CINDS,CIBLNK        INSERT A BLANK LINE                          
         LA    R3,FMTTABL(R3)                                                   
         B     GNXTX3                                                           
                                                                                
GNXTX1   NI    CINDS,FF-CIFTYP                                                  
GNXTX2   NI    CINDS,FF-CIBLNK                                                  
GNXTX3   ST    R2,SVR2                                                          
         ST    R3,SVR3                                                          
         ST    R4,SVR4                                                          
         B     EXITY                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN                                                      *         
***********************************************************************         
                                                                                
         USING FMTTABD,TLFMTTAB                                                 
DISCLMS  TM    TLINDS1,TLIPRO      IF PROTECTED THEN ITS A BLANK LINE           
         BO    EXIT                                                             
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISDATA             DATA TYPE                                    
         B     DISSECT             SECTION                                      
         B     DISLIN              LINE                                         
         B     DISCOL              COLUMN                                       
         B     DISHD1              HEADING 1                                    
         B     DISHD2              HEADING 2                                    
         B     DISALGN             ALIGN                                        
         B     DISHDLN             HEADING LINE                                 
         B     DISHDPR             HEADING PER                                  
         B     DISPRT              PRINT                                        
         B     DISKEY              KEY                                          
                                                                                
***********************************************************************         
* DISPLAY DATA TYPE                                                   *         
***********************************************************************         
                                                                                
DISDATA  TM    TLFT1IND,CIFTYP     TEST FIRST RECORD FOR DATA TYPE              
         BZ    EXITY                                                            
         MVI   FVIFLD,AC#ESCL                                                   
         MVC   FVIFLD+1(2),FMTDIC                                               
         ICM   RE,3,FMTDICD        OVERRIDE DESCRIPTION                         
         BZ    *+8                                                              
         STCM  RE,3,FVIFLD+1                                                    
         MVI   FVIFLD+3,12                                                      
         GOTO1 VDICTAT,BOPARM,C'SL  ',FVIFLD                                    
                                                                                
         CLI   SORTFMT2,BLFFNET2                                                
         BE    *+12                                                             
         CLI   SORTFMT2,BLFFGRS2                                                
         BNE   EXITY                                                            
         ICM   RE,3,FMTDICD                                                     
         BNZ   EXITY                                                            
         LA    R1,FVIFLD                                                        
         CLI   0(R1),C' '                                                       
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         LA    R1,1(R1)                                                         
         MVI   0(R1),C'('                                                       
         LH    RF,=Y(LC@TOTAL-TWAD)                                             
         LA    RF,TWAD(RF)                                                      
         MVC   1(L'LC@TOTAL,R1),0(RF)                                           
         LA    RF,L'LC@TOTAL(R1)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
         B     EXITY                                                            
                                                                                
***********************************************************************         
* DISPLAY SECTION NAME                                                *         
***********************************************************************         
                                                                                
DISSECT  MVC   FVIFLD(L'TLSECNAM),TLSECNAM                                      
         B     EXIT                                                             
                                                                                
***********************************************************************         
* DISPLAY LINE                                                        *         
***********************************************************************         
                                                                                
DISLIN   CLI   BLFLINF,0                                                        
         BNE   *+12                                                             
         MVI   FVIFLD,C'*'                                                      
         B     EXITY                                                            
         XR    RF,RF                                                            
         CLI   FMTLINMN,1                                                       
         BNE   DISLIN02                                                         
         CLI   FMTLINMX,1                                                       
         BNE   DISLIN02                                                         
         LA    RF,X'80'                                                         
DISLIN02 GOTO1 DISNOS,BOPARM,((RF),BLFLINF),BLFLINN                             
         TM    BLFOPT2,BLFOBOT                                                  
         BZ    EXITY                                                            
         LA    RF,FVIFLD+3                                                      
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         B     EXITY                                                            
                                                                                
***********************************************************************         
* DISPLAY COLUMN                                                      *         
***********************************************************************         
                                                                                
DISCOL   CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         GOTO1 DISNOS,BOPARM,BLFCOLF,BLFCOLN                                    
         B     EXITY                                                            
                                                                                
***********************************************************************         
* DISPLAY HEADING 1                                                   *         
***********************************************************************         
                                                                                
DISHD1   CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         MVC   FVIFLD(L'BLFHED1),BLFHED1                                        
         B     EXITY                                                            
                                                                                
***********************************************************************         
* DISPLAY HEADING 2                                                   *         
***********************************************************************         
                                                                                
DISHD2   CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         MVC   FVIFLD(L'BLFHED2),BLFHED2                                        
         B     EXITY                                                            
                                                                                
***********************************************************************         
* DISPLAY HEADING ALIGNMENT                                           *         
***********************************************************************         
                                                                                
DISALGN  LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         MVC   FVIFLD(L'UC@LEFT),UC@LEFT                                        
         TM    BLFOPT1,BLFOLFTQ                                                 
         BO    EXITY                                                            
         MVC   FVIFLD(L'UC@CENTR),UC@CENTR                                      
         TM    BLFOPT1,BLFOCTRQ                                                 
         BO    EXITY                                                            
         MVC   FVIFLD(L'UC@RIGHT),UC@RIGHT                                      
         TM    BLFOPT1,BLFORGTQ                                                 
         BO    EXITY                                                            
         MVC   FVIFLD(L'UC@MMIZE),UC@MMIZE                                      
         TM    BLFOPT1,BLFOMAXQ                                                 
         BO    EXITY                                                            
         MVC   FVIFLD,BCSPACES                                                  
         B     EXITN                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
* DISPLAY HEADING LINE                                                *         
***********************************************************************         
                                                                                
DISHDLN  CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         IC    R1,BLFHEDL          0-3 OR 0-8                                   
         LA    R1,1(R1)                                                         
         STC   R1,FVIFLD                                                        
         OI    FVIFLD,X'F0'                                                     
         B     EXITY                                                            
                                                                                
***********************************************************************         
* DISPLAY HEADING PER                                                 *         
***********************************************************************         
                                                                                
DISHDPR  CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         TM    BLFOPT2,BLFOHPG+BLFOHPR                                          
         BZ    EXITY                                                            
         TM    BLFOPT2,BLFOHPR                                                  
         BZ    DISHDPR2                                                         
         MVC   FVIFLD(L'UC@PRGRP),UC@PRGRP                                      
         B     EXITY                                                            
DISHDPR2 TM    BLFOPT2,BLFOHPG                                                  
         BZ    EXITY                                                            
         MVC   FVIFLD(L'UC@PAGE),UC@PAGE                                        
         B     EXITY                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
* DISPLAY PRINT                                                       *         
***********************************************************************         
                                                                                
DISPRT   LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         MVC   FVIFLD(L'UC@YES),UC@YES                                          
         TM    BLFOPT1,BLFONPRQ                                                 
         BNO   EXITY                                                            
         MVC   FVIFLD(L'UC@YES),BCSPACES                                        
         MVC   FVIFLD(L'UC@NO),UC@NO                                            
         B     EXITY                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
* DISPLAY KEY                                                         *         
***********************************************************************         
                                                                                
DISKEY   LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         MVC   FVIFLD(L'UC@NO),UC@NO                                            
         TM    BLFOPT1,BLFONKYQ                                                 
         BO    EXITY                                                            
         LA    RE,BLFKEYS                                                       
DISK02   CLI   0(RE),0             TETS E-O-LIST                                
         BE    EXITY                                                            
         CLC   BLFFLD,0(RE)        TEST THIS IS A KEY CANDIDATE                 
         BE    DISK04              YES - THEN IT'S A KEY                        
         LA    RE,1(RE)                                                         
         B     DISK02                                                           
DISK04   MVC   FVIFLD(L'UC@YES),UC@YES                                          
         B     EXITY                                                            
                                                                                
BLFKEYS  DC    AL1(BLFFWCQ,BLFFWCNQ,BLFFDATQ,BLFFREFQ,BLFFRATQ)                 
         DC    AL1(BLFFNARQ,BLFFCMRQ,0)                                         
         DS    0H                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR VALIDATE THIS LINE                                        *         
***********************************************************************         
                                                                                
LINFRST  MVC   THISBLF,BLFEL                                                    
         TM    TLFT1IND,CIFTYP     IF THIS IS FIRST FOR DATA TYPE               
         BZ    EXITY                                                            
         XC    SV1STBLF,SV1STBLF   CLEAR SAVED BLF                              
         B     EXITY                                                            
                                                                                
***********************************************************************         
* VALIDATE COLUMNS                                                    *         
***********************************************************************         
                                                                                
VALCLM   TM    TLINDS1,TLIPRO      IF PROTECTED THEN ITS A BLANK LINE           
         BO    EXITY                                                            
         LA    R2,TLFMTTAB                                                      
         ST    R2,AFMT                                                          
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     EXITY               DATA TYPE                                    
         B     EXITY               SECTION                                      
         B     VALLIN              LINE                                         
         B     VALCOL              COLUMN                                       
         B     VALHD1              HEADING 1                                    
         B     VALHD2              HEADING 2                                    
         B     VALALGN             ALIGN                                        
         B     VALHDLN             HEADING LINE                                 
         B     VALHDPR             HEADING PER                                  
         B     VALPRT              PRINT                                        
         B     VALKEY              KEY                                          
                                                                                
***********************************************************************         
* VALIDATE LINE                                                       *         
***********************************************************************         
                                                                                
VALLIN   CLI   FVIFLD,C'*'                                                      
         BNE   VALLIN02                                                         
         XC    BLFLINF(BLFLNQ-(BLFLINF-BLFELD)),BLFLINF                         
         B     EXITY                                                            
                                                                                
VALLIN02 CLI   BLFLINF,0           IF ELEMENT ALREADY EXISTS                    
         BNE   VALLIN10            THEN LEAVE IT                                
         MVC   BLFDATA,SV1STBLF+(BLFDATA-BLFEL) NO, SO COPY FIRST               
         OC    BLFDATA,BLFDATA     TEST IF 1ST IS EMPTY                         
         BNZ   VALLIN10                                                         
         GOTO1 SETDEF              YES, SO SET DEFAULT                          
                                                                                
VALLIN10 CLI   FVILEN,0            TEST ANY INPUT                               
         BE    EXITY                                                            
         NI    BLFOPT2,FF-BLFOBOT                                               
         GOTO1 VALNOS,BOPARM,BLFLINF,BLFLINN,9,FMTLINMN,A(AE$PBMAX)             
         BE    EXITY               CC=EQUAL FOR OKAY AND TOP                    
         BL    VALLINXN            CC=LOW FOR ERROR                             
         TM    FMTINDS1,FMTITFP    BOTTOM ONLY ALLOWED FOR TOTALS               
         BZ    VALLINXN                                                         
         OI    BLFOPT2,BLFOBOT     CC=HIGH AND FROM THE BOTTOM                  
         B     EXITY                                                            
                                                                                
VALLINXN OI    CINDS,CIERR                                                      
         B     EXITN                                                            
                                                                                
***********************************************************************         
* VALIDATE COLUMN                                                     *         
***********************************************************************         
                                                                                
VALCOL   CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         CLI   FVILEN,0                                                         
         BE    EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,BOFMAXWD                                                      
         GOTO1 VALNOS,BOPARM,BLFCOLF,BLFCOLN,(RF),FMTCOLMN,A(AE$FNFPL)          
         BNL   EXITY                                                            
         OI    CINDS,CIERR                                                      
         B     EXITN                                                            
                                                                                
***********************************************************************         
* VALIDATE HEADING 1                                                  *         
***********************************************************************         
                                                                                
VALHD1   CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         CLI   FVILEN,0                                                         
         BNE   VALHD104                                                         
         TM    TLFT1IND,CIFTYP     TEST IF 1ST OF THIS DATA TYPE                
         BO    VALHD102                                                         
         MVC   BLFHED1,SV1STBLF+(BLFHED1-BLFEL) NO, SO COPY 1ST                 
         B     EXITY                                                            
VALHD102 GOTO1 HEDDIC,BOPARM,BLFELD,FMTTABD,BLFHED1                             
         B     EXITY                                                            
                                                                                
VALHD104 MVC   BLFHED1,FVIFLD                                                   
         CLI   BLFHED1,C'*'                                                     
         BNE   EXITY                                                            
         MVC   BLFHED1+1(L'BLFHED1-1),BCSPACES                                  
         B     EXITY                                                            
                                                                                
***********************************************************************         
* VALIDATE HEADING 2                                                  *         
***********************************************************************         
                                                                                
VALHD2   CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         CLI   FVILEN,0            TEST DEFAULT                                 
         BE    VALHD202                                                         
                                                                                
         MVC   BLFHED2,FVIFLD                                                   
         CLI   BLFHED2,C'*'                                                     
         BNE   EXITY                                                            
         MVC   BLFHED2+1(L'BLFHED2-1),BCSPACES                                  
         B     EXITY                                                            
                                                                                
VALHD202 TM    TLFT1IND,CIFTYP     TEST IF 1ST OF THIS DATA TYPE                
         BO    VALHD204                                                         
         CLC   BLFHED1,SV1STBLF+(BLFHED1-BLFEL) NO, ARE HED1'S SAME ?           
         BNE   VALHD204                                                         
         MVC   BLFHED2,SV1STBLF+(BLFHED2-BLFEL) HED2'S CAN BE TOO               
         B     EXITY                                                            
                                                                                
VALHD204 CLI   BLFHED1,C'*'        TEST HEADING 1 DEFINED                       
         BE    VALHD206            YES - UNDERLINE IT                           
         GOTO1 HEDUND,BLFHED1                                                   
         B     EXITY                                                            
VALHD206 GOTO1 HEDDIC,BOPARM,BLFELD,FMTTABD,BLFHED2                             
         B     EXITY                                                            
                                                                                
***********************************************************************         
* VALIDATE HEADING ALIGNMENT                                          *         
***********************************************************************         
                                                                                
VALALGN  LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         CLI   FVILEN,0                                                         
         BE    VALALI02                                                         
         MVI   BLFOPT1,BLFOLFTQ                                                 
         GOTO1 CMPWRD,UC@LEFT                                                   
         BE    VALALI02                                                         
         MVI   BLFOPT1,BLFOCTRQ                                                 
         GOTO1 CMPWRD,UC@CENTR                                                  
         BE    VALALI02                                                         
         MVI   BLFOPT1,BLFORGTQ                                                 
         GOTO1 CMPWRD,UC@RIGHT                                                  
         BE    VALALI02                                                         
         MVI   BLFOPT1,BLFOMAXQ                                                 
         GOTO1 CMPWRD,UC@MMIZE                                                  
         BE    VALALI02                                                         
         CLI   BLFHED1,C'*'                                                     
         BNE   VALALIXN                                                         
         CLI   BLFHED2,C'*'                                                     
         BNE   VALALIXN                                                         
         MVI   BLFOPT1,0                                                        
                                                                                
VALALI02 GOTO1 AFMTHED,BOPARM,(1,BLFELD)                                        
         BH    VALALIXN                                                         
         GOTO1 (RF),(R1),(2,BLFELD)                                             
         BNH   EXITY                                                            
                                                                                
VALALIXN OI    CINDS,CIERR                                                      
         B     EXITN                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
* VALIDATE HEADING LINE                                               *         
***********************************************************************         
                                                                                
VALHDLN  CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         MVI   BLFHEDL,0           DEFAULT TO FIRST LINE                        
         CLI   FVILEN,0                                                         
         BE    EXITY                                                            
         CLI   FVILEN,1            ONE CHARACTER ONLY                           
         BNE   VALHDLXN                                                         
         CLI   FVIFLD,C'1'         MUST BE 1-9                                  
         BL    VALHDLXN                                                         
         CLI   FVIFLD,C'9'         MAXIMUM 9 PARA HEADING LINES                 
         BH    VALHDLXN                                                         
         TM    BLFOPT2,BLFOHPG     TEST HEADING PER PAGE                        
         BZ    *+12                                                             
         CLI   FVIFLD,C'4'         MAXIMUM 4 PAGE HEADING LINES                 
         BH    VALHDLXN                                                         
         MVC   BOBYTE1,FVIFLD                                                   
         NI    BOBYTE1,X'0F'                                                    
         IC    R1,BOBYTE1                                                       
         BCTR  R1,0                                                             
         STC   R1,BLFHEDL          SAVE 0-3 OR 0-8                              
         B     EXITY                                                            
                                                                                
VALHDLXN OI    CINDS,CIERR                                                      
         B     EXITN                                                            
                                                                                
***********************************************************************         
* VALIDATE HEADING PER                                                *         
***********************************************************************         
                                                                                
VALHDPR  CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         NI    BLFOPT2,FF-(BLFOHPG+BLFOHPR)                                     
         GOTO1 CMPWRD,UC@PAGE                                                   
         BNE   VALHDP02                                                         
         CLI   BLFHEDL,X'03'       TEST 0-3 HEADLINES                           
         BH    VALHDPXN                                                         
         OI    BLFOPT2,BLFOHPG                                                  
         B     EXITY                                                            
VALHDP02 GOTO1 CMPWRD,UC@PRGRP                                                  
         BNE   VALHDP04                                                         
         OI    BLFOPT2,BLFOHPR                                                  
         B     EXITY                                                            
VALHDP04 CLI   BLFHED1,C'*'                                                     
         BNE   VALHDPXN                                                         
         CLI   BLFHED2,C'*'                                                     
         BE    EXITY                                                            
                                                                                
VALHDPXN OI    CINDS,CIERR                                                      
         B     EXITN                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
* VALIDATE PRINT                                                      *         
***********************************************************************         
                                                                                
VALPRT   LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         NI    BLFOPT1,FF-(BLFONPRQ)                                            
         GOTO1 CMPWRD,UC@YES                                                    
         BE    EXITY                                                            
         GOTO1 CMPWRD,UC@NO                                                     
         BNE   VALPRTN                                                          
         OI    BLFOPT1,BLFONPRQ                                                 
         B     EXITY                                                            
                                                                                
VALPRTN  OI    CINDS,CIERR                                                      
         B     EXITN                                                            
         DROP  R4                                                               
                                                                                
***********************************************************************         
* VALIDATE KEY                                                        *         
***********************************************************************         
                                                                                
VALKEY   LH    R4,=Y(BSDICT-TWAD)                                               
         LA    R4,TWAD(R4)                                                      
         USING BSDICT,R4                                                        
         NI    BLFOPT1,FF-(BLFONKYQ)                                            
         LA    RE,BLFKEYS                                                       
VALK02   CLI   0(RE),0             TEST E-O-LIST                                
         BE    EXITY               YES - IT CANNOT BE A KEY                     
         CLC   BLFFLD,0(RE)                                                     
         BE    VALK04                                                           
         LA    RE,1(RE)                                                         
         B     VALK02                                                           
VALK04   GOTO1 CMPWRD,UC@YES                                                    
         BE    EXITY                                                            
         GOTO1 CMPWRD,UC@NO                                                     
         BNE   VALKEYN                                                          
         OI    BLFOPT1,BLFONKYQ                                                 
         B     EXITY                                                            
                                                                                
VALKEYN  OI    CINDS,CIERR                                                      
         B     EXITN                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LAST FOR VALIDATE THIS LINE                                         *         
***********************************************************************         
                                                                                
LINLAST  CLC   THISBLF,BLFEL       HAS BLFEL CHANGED ?                          
         BE    *+8                                                              
         OI    SVINDS,SVIPWRT      SET WRITE PENDING                            
         TM    TLINDS1,TLIPRO      IF PROTECTED THEN ITS A BLANK LINE           
         BO    EXITY                                                            
         CLI   BLFLINF,0           SAVE THIS BLFEL IF ANY DATA                  
         BE    EXITY                                                            
         MVC   SV1STBLF,BLFEL                                                   
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET DEFAULT BLFELD                                       *         
***********************************************************************         
                                                                                
SETDEF   NTR1  ,                                                                
         MVC   BLFLINF,FMTLINF                                                  
         MVC   BLFLINN,FMTLINN                                                  
         MVC   BLFCOLF,FMTCOLF                                                  
         MVC   BLFCOLN,FMTCOLN                                                  
         MVI   BLFHED1,AC#ESCL                                                  
         MVI   BLFOPT1,BLFOLFTQ                                                 
         TM    FMTINDS1,FMTINUM                                                 
         BZ    *+8                                                              
         MVI   BLFOPT1,BLFORGTQ                                                 
         MVI   BLFOPT2,BLFOHPR                                                  
         TM    FMTINDS1,FMTINUM                                                 
         BZ    *+8                                                              
         MVI   BLFOPT2,BLFOHPG                                                  
         GOTO1 HEDDIC,BOPARM,BLFELD,FMTTABD,BLFHED1                             
         GOTO1 HEDUND,BLFHED1                                                   
SETDEFX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST FOR THIS SCREEN                                                *         
***********************************************************************         
                                                                                
SCRLAST  TM    CINDS,CIERR                                                      
         BO    SCRLST02                                                         
         GOTO1 PRTSET,0                                                         
         BE    SCRLST04                                                         
SCRLST02 MVI   FVOMTYP,GTMERR                                                   
         B     EXITN                                                            
                                                                                
SCRLST04 TM    SVINDS,SVIPWRT                                                   
         BNO   EXITY                                                            
         MVC   FVMSGNO,=AL2(AI$RPF6S)                                           
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PRTSET ROUTINE TO CHECK FOR OVERLAP ERRORS                          *         
***********************************************************************         
                                                                                
PRTSET   NTR1  ,                                                                
         LA    RE,PBLOCK           SPACERIZE PRINT BLOCK                        
         LA    RF,PBLOCKL                                                       
         LA    R5,C' '                                                          
         SLL   R5,24                                                            
         MVCL  RE,R4                                                            
         MVC   TLNUM,CSPSRECN                                                   
         LTR   R1,R1                                                            
         BZ    *+12                                                             
         MVI   0(R1),FF                                                         
         B     PRTSET06                                                         
                                                                                
         USING SECDATAD,R3                                                      
         LA    R3,SECDATA          CLEAR SECDATA LIST                           
         LA    R0,SECNM#Q                                                       
         XC    SDATA,SDATA                                                      
         LA    R3,L'SDATA(R3)                                                   
         BCT   R0,*-10                                                          
                                                                                
PRTSET02 GOTO1 GETTSAR,0           READ BACK NEXT TSAR RECORD                   
         BNE   PRTSET04                                                         
                                                                                
         TM    TLFT1IND,CIFTYP     IF FIRST SECTION OF THIS TYPE                
         BZ    *+8                                                              
         LA    R3,SECDATA          POINT TO FIRST SECDATA ENTRY                 
         GOTO1 SETMAX,SDATA        SET DATA FOR THIS TYPE/SECTION               
         LA    R3,L'SDATA(R3)                                                   
         B     PRTSET02                                                         
                                                                                
PRTSET04 LA    R4,SECNM#Q          GOTO SETDSP ROUTINE FOR EACH ENTRY           
         LA    R3,SECDATA          IN SECDATA LIST                              
PRTSET06 CLI   SSEC,0                                                           
         BE    PRTSET22                                                         
         GOTO1 SETDSP,SDATA                                                     
         BNE   EXITN                                                            
                                                                                
         MVC   TLNUM,CSPSRECN                                                   
PRTSET08 GOTO1 GETTSAR,SSEC        READ BACK NEXT TSAR FOR THIS SECTION         
         BNE   PRTSET20                                                         
                                                                                
         MVI   CPRTFLD,AC#ESCL                                                  
         MVC   CPRTFLD+1(2),FMTDIC                                              
         GOTO1 VDICTAT,BOPARM,C'SL  ',CPRTFLD                                   
         NI    CPRTFLD,FF-X'40'                                                 
         MVC   CPRTFLD+1(L'CPRTFLD-1),CPRTFLD                                   
                                                                                
         TM    FMTINDS1,FMTINUM    TEST A NUMBER                                
         BZ    PRTSET12                                                         
         MVI   BODUB1,X'99'        SET MAX NEGATIVE PACKED IN DUB1              
         MVC   BODUB1+1(6),BODUB1                                               
         MVI   BODUB1+7,X'9D'                                                   
                                                                                
         XC    BODUB2,BODUB2       SET NO. OF DECIMAL PLACES                    
         MVC   BODUB2+3(1),FMTNDECP                                             
         CLI   FMTNDECP,FMTNCURQ   OR AS PER CURRENCY                           
         BNE   *+10                                                             
         MVC   BODUB2,CSCURBIL                                                  
         CURED (P8,BODUB1),(16,CPRTFLD+116),BODUB2,COMMAS=YES,MINUS=YES         
                                                                                
         LA    R0,16                                                            
         LA    RF,CPRTFLD+L'CPRTFLD-1                                           
PRTSET10 CLI   0(RF),C'9'                                                       
         BNE   *+10                                                             
         MVC   0(1,RF),CPRTFLD                                                  
         BCTR  RF,0                                                             
         BCT   R0,PRTSET10                                                      
                                                                                
PRTSET12 GOTO1 PRTELM                                                           
         BNE   EXITN                                                            
         B     PRTSET08                                                         
                                                                                
PRTSET20 CLI   BCPFKEY,PFKCPRTQ                                                 
         BNE   PRTSET22                                                         
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         GOTO1 PRTBLK,SSEC                                                      
         MVI   REPPRNSA,5                                                       
         GOTO1 VREPORT,REPD                                                     
         DROP  R5                                                               
                                                                                
PRTSET22 LA    R3,L'SECDATA(R3)                                                 
         LA    RE,PBLOCK           SPACERIZE PRINT BLOCK                        
         LA    RF,PBLOCKL                                                       
         LA    R1,C' '                                                          
         SLL   R1,24                                                            
         MVCL  RE,R0                                                            
         BCT   R4,PRTSET06                                                      
         DROP  R3                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT ELEMENT INTO PRINT BLOCK                             *         
***********************************************************************         
                                                                                
PRTELM   NTR1  ,                                                                
         USING SECDATAD,R3                                                      
         CLI   BLFLINF,0                                                        
         BE    EXITY                                                            
         IC    RE,BLFCOLF                                                       
         IC    RF,BLFCOLN                                                       
         AR    RF,RE                                                            
         BCTR  RF,0                                                             
         STC   RF,CCOLL            SAVE END POSITION OF LINE (1-132)            
                                                                                
         XR    R5,R5                                                            
         IC    R5,BLFLINF                                                       
         TM    BLFOPT2,BLFOBOT                                                  
         BZ    PELM02                                                           
         BCTR  R5,0                                                             
         LNR   R5,R5                                                            
         XR    RE,RE                                                            
         IC    RE,SDISBOT                                                       
         AR    R5,RE                                                            
         B     PELM04                                                           
PELM02   XR    RE,RE                                                            
         IC    RE,SDISTOP                                                       
         AR    R5,RE                                                            
                                                                                
PELM04   MH    R5,=Y(L'PRL1)                                                    
         LA    R5,PRL1-L'PRL1-1(R5)                                             
         XR    RE,RE                                                            
         IC    RE,CCOLL                                                         
         AR    R5,RE               R5=END POSN. OF THE LINE                     
                                                                                
         XR    R0,R0                                                            
         IC    R0,BLFLINN                                                       
PELM12   LR    R2,R5               R2=END OF FIELD                              
         LA    R1,CPRTFLD+L'CPRTFLD-1                                           
                                                                                
         XR    RF,RF                                                            
         IC    RF,BLFCOLN                                                       
PELM14   CLI   0(R2),C' '          TEST FOR OVERLAP                             
         BE    PELM18                                                           
         MVC   FVMSGNO,=AL2(AE$FLDOV)                                           
         B     PRTELMN                                                          
PELM18   MVC   0(1,R2),0(R1)                                                    
         BCTR  R1,0                                                             
         BCTR  R2,0                                                             
         BCT   RF,PELM14                                                        
                                                                                
         CLI   BLFCOLF,1           TEST TOUCHED ON THE LEFT                     
         BE    PELM20                                                           
         CLI   0(R2),C' '                                                       
         BE    PELM20                                                           
         MVC   FVMSGNO,=AL2(AE$FLDOV)                                           
         LA    R2,1(R2)                                                         
         B     PRTELMN                                                          
                                                                                
PELM20   CLI   CCOLL,L'REPP1       TEST TOUCHED ON THE RIGHT                    
         BE    PELM28                                                           
         LR    R2,R5                                                            
         LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BE    PELM28                                                           
         MVC   FVMSGNO,=AL2(AE$FLDTC)                                           
         BCTR  R2,0                                                             
         B     PRTELMN                                                          
                                                                                
PELM28   LA    R5,L'PRL1(R5)                                                    
         BCT   R0,PELM12                                                        
                                                                                
         GOTO1 PRTHED,1            PRINT HEAD LINE 1                            
         BNE   PRTELMN                                                          
         GOTO1 (RF),2              PRINT HEAD LINE 2                            
         BNE   PRTELMN                                                          
                                                                                
PRTELMY  B     EXITY                                                            
                                                                                
PRTELMN  MVC   COVRLAP1,BLFFLD                                                  
         MVC   COVRLIN1,TLNUM                                                   
         CLI   COVRLAP2,0                                                       
         BNE   EXITN             TEST ALREADY RECURSED                          
                                                                                
         MVC   COVRLAP2,COVRLAP1                                                
         MVC   COVRLIN2,COVRLIN1                                                
         GOTO1 PRTSET,(R2)         RECURSE TO FIND 1ST OVERLAP FIELD            
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 PRTERR,BOPARM,SSEC,COVRLAP2,COVRLAP1                             
         B     EXITN                                                            
         DROP  R3                                                               
                                                                                
***********************************************************************         
* OVERLAPPING ERRORS OUTPUT                                           *         
* NTRY: P1=A(SECTION NO) TO SUB IN &3                                 *         
*       P2=A(FIELD 1)    TO SUB IN &1                                 *         
*       P3=A(FIELD 2)    TO SUB IN &2                                 *         
*  FVMSGNO=MESSAGE NUMBER                                             *         
* EXIT: FVMSGNO, FVADDR, FVPARMS SET UP                               *         
***********************************************************************         
                                                                                
PRTERR   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         XC    BOADDR1,BOADDR1                                                  
         XC    BOADDR2,BOADDR2                                                  
                                                                                
         L     R5,AFMTTAB                                                       
T        USING FMTTABD,R5                                                       
         B     *+8                                                              
PERR02   LA    R5,FMTTABL(R5)                                                   
         CLI   T.FMTFLD,EOT                                                     
         BE    PERR06                                                           
         CLC   T.FMTFLD,0(R3)                                                   
         BNE   PERR04                                                           
         LA    R6,BOADDR1                                                       
         GOTO1 SUBDESC,T.FMTDIC                                                 
         B     PERR02                                                           
PERR04   CLC   T.FMTFLD,0(R4)                                                   
         BNE   PERR02                                                           
         LA    R6,BOADDR2                                                       
         GOTO1 SUBDESC,T.FMTDIC     ADD DESCRIPTION TO SUB. PARMS               
         B     PERR02                                                           
         DROP  T                                                                
                                                                                
PERR06   LA    R1,SECTAB           ADD SECTION NAME TO SUB PARMS                
         USING SECTABD,R1                                                       
         CLC   SECTNUM,0(R2)                                                    
         BE    *+12                                                             
         LA    R1,SECTABLN(R1)                                                  
         B     *-14                                                             
         LA    R2,FVPARMS                                                       
         XR    RF,RF                                                            
         ICM   RF,1,0(R2)                                                       
         BZ    *+8                                                              
         BXH   R2,RF,*-8                                                        
         MVI   0(R2),L'SECTNAME+1                                               
         MVC   1(L'SECTNAME,R2),SECTNAME                                        
         OC    1(L'SECTNAME,R2),BCSPACES                                        
         DROP  R1                                                               
                                                                                
         XR    RE,RE                                                            
         XR    R1,R1                                                            
         XR    R2,R2                                                            
         LH    R1,COVRLIN1         R1=LOWEST LINE NUMBER                        
         LH    R2,COVRLIN2         R2=HIGHEST LINE NUMBER                       
         CLC   COVRLIN1,COVRLIN2                                                
         BNH   *+10                                                             
         XR    R1,R2                                                            
         XR    R2,R1                                                            
         XR    R1,R2                                                            
                                                                                
         CH    R2,CSPAG#LO                                                      
         BL    PERR10              HIGHEST < FIRST                              
         CH    R1,CSPAG#HI                                                      
         BH    PERR10              LOWEST > LAST                                
         LR    RE,R1               R0=LINE NO. FOR CURSOR                       
         CH    R1,CSPAG#LO                                                      
         BNL   *+6                                                              
         LR    RE,R2                                                            
         SH    RE,CSPAG#LO         SUBTRACT 1ST SCREEN LINE NUMBER              
         MH    RE,CSLINLEN         MULTIPLY BY LINE LENGTH                      
         AH    RE,CS1STLIN         ADD ON DISP INTO TWA OF 1ST LIN              
         B     *+8                                                              
                                                                                
PERR10   LH    RE,CS1STLIN         DISP TO 1ST LINE                             
         AR    RE,RA                                                            
         XR    RF,RF               BUMP TO 1ST UNPROT ON THIS LINE              
         IC    RF,0(RE)                                                         
         AR    RE,RF                                                            
         TM    1(RE),X'20'                                                      
         BO    *-10                                                             
         ST    RE,FVADDR                                                        
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT HEADLINE   (R1=1 OR 2)                             *         
***********************************************************************         
                                                                                
PRTHED   NTR1  ,                                                                
         STC   R1,BOBYTE1                                                       
         GOTO1 AFMTHED,BOPARM,(BOBYTE1,BLFELD)                                  
         BL    EXITY                                                            
         L     R0,4(R1)            R0=LENGTH OF HEADLINE                        
         LA    R2,PRH1                                                          
         CLI   BOBYTE1,2                                                        
         BNE   *+8                                                              
         LA    R2,PRH2                                                          
         A     R2,0(R1)            R2=A(PRINT LINE SPACE FOR HEADLINE)          
         LA    RE,BOELEM                                                        
                                                                                
PHED12   CLI   0(RE),C' '                                                       
         BNH   PHED14                                                           
         CLI   0(R2),C' '          TEST OVERLAP                                 
         BNE   PRTHEDN                                                          
         MVC   0(1,R2),0(RE)       COPY HEADLINE                                
PHED14   LA    RE,1(RE)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,PHED12                                                        
PRTHEDY  B     EXITY                                                            
                                                                                
PRTHEDN  MVC   FVMSGNO,=AL2(AE$HEDOV)                                           
         MVC   FVXTRA(1),BOBYTE1   HEADLINE 1 OR 2                              
         OI    FVXTRA,C'0'                                                      
         LTR   RB,RB               CC=NOT EQUAL                                 
         XIT1  REGS=(R2)                                                        
         B     EXITN                                                            
                                                                                
***********************************************************************         
* PF KEY ROUTINES                                                     *         
***********************************************************************         
                                                                                
PFKRTN   CLI   BCPFKEY,PFKCCLRQ                                                 
         BE    CLEAR                                                            
         CLI   BCPFKEY,PFKCPRTQ                                                 
         BE    PRINT                                                            
         CLI   BCPFKEY,PFKCSAVQ                                                 
         BE    SAVE                                                             
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* ROUTINE TO CLEAR EACH LINE (PF 1)                                   *         
***********************************************************************         
                                                                                
CLEAR    MVC   TLNUM,CSPSRECN                                                   
CLEAR02  GOTO1 GETTSAR,0           GET NEXT NON BLANK TSAR RECORD               
         BNE   EXITY                                                            
         XC    BLFLINF(BLFLNQ-(BLFLINF-BLFELD)),BLFLINF                         
         GOTO1 ATSARIO,TSAPUT                                                   
         B     CLEAR02                                                          
                                                                                
***********************************************************************         
* ROUTINE TO PRINT (PF 5)                                             *         
***********************************************************************         
                                                                                
PRINT    L     R5,AREP                                                          
         USING REPD,R5                                                          
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         XC    REPSUBID,REPSUBID                                                
         OC    REPSUBID,CSREPID                                                 
         BNZ   *+10                                                             
         MVC   REPSUBID,=C'AFL'                                                 
         MVCDD REPDESC,AC#FRMAT                                                 
         GOTO1 VDICTAT,BOPARM,C'SL  ',REPDESC                                   
                                                                                
         MVI   REPACTN,REPAINI                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         BASR  RE,RF                                                            
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI  REPACTN,REPAPUT                                                   
                                                                                
         MVC   REPH1+119(L'BCUSERID),BCUSERID                                   
         MVC   REPH2+119(L'CSBPID),CSBPID                                       
         MVC   REPM1+017(L'CONFMT),CONFMT                                       
         MVC   REPM1+021(L'CONNAM),CONNAM                                       
         MVC   REPM1+106(L'CONLNGW),CONLNGW                                     
         MVC   REPM1+119(L'CONLNG),CONLNG                                       
         MVI   REPPRNSA,3                                                       
         GOTO1 VREPORT,REPD                                                     
                                                                                
         GOTO1 PRTSET,0                                                         
         BNE   EXITN                                                            
                                                                                
         MVI   REPACTN,REPACLO                                                  
         GOTO1 VREPORT,REPD                                                     
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RPSPL)                                           
         MVI   FVPARMS,9                                                        
         MVC   FVPARMS+1(L'REPSUBID),REPSUBID                                   
         MVC   FVPARMS+1+L'REPSUBID(1),BCCOMMA                                  
         LA    RF,FVPARMS+1+L'REPSUBID+1                                        
         EDIT (2,REPREPNO),(5,(RF)),ALIGN=LEFT                                  
         MVC   BASSRV,DQU                                                       
         OI    BASSRVH+FHOID,FHOITR                                             
         B     EXITY                                                            
                                                                                
***********************************************************************         
* PRTBLK ROUTINE PASSES EACH LINE OF PBLOCK TO REPORT                 *         
* NTRY: R1=SECTION NO.                                                *         
***********************************************************************         
                                                                                
PRTBLK   NTR1  ,                                                                
         LR    R3,R1                                                            
         LA    R1,SECTAB           FIND SECTION NAME                            
         USING SECTABD,R1                                                       
         CLC   SECTNUM,0(R3)                                                    
         BE    *+12                                                             
         LA    R1,SECTABLN(R1)                                                  
         B     *-14                                                             
                                                                                
         MVI   REPP1,C'-'                                                       
         MVC   REPP1+1(L'REPP1-1),REPP1                                         
         MVC   REPP1+62(L'SECTNAME),SECTNAME                                    
         DROP  R1                                                               
                                                                                
         LA    RE,REPP1+50                                                      
PBLK02   CLI   0(RE),C'-'                                                       
         BE    PBLK04                                                           
         CLI   0(RE),C' '                                                       
         BNH   *+12                                                             
         LA    RE,1(RE)                                                         
         B     PBLK02                                                           
         MVI   0(RE),C'-'                                                       
         B     *-12                                                             
                                                                                
PBLK04   MVI   REPPRNSA,2                                                       
         GOTO1 VREPORT,REPD                                                     
         MVC   REPP1,PRH1                                                       
         GOTO1 VREPORT,REPD                                                     
         MVC   REPP1,PRH2                                                       
         GOTO1 VREPORT,REPD                                                     
         LA    R0,PRLN                                                          
         LA    R2,PRL1                                                          
PBLK10   MVC   REPP1,0(R2)                                                      
         GOTO1 VREPORT,REPD                                                     
         LA    R2,L'PRL1(R2)                                                    
         BCT   R0,PBLK10                                                        
         B     EXITY                                                            
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SAVE (PF 6)                                              *         
***********************************************************************         
                                                                                
SAVE     DS    0H                                                               
         MVC   TLNUM,CSPSRECN                                                   
         NI    SVINDS,FF-SVICFND                                                
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY                                                         
         USING PBCRECD,R3                                                       
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,THISFORM                                                 
         MVC   PBCKLANG,THISLANG                                                
                                                                                
         GOTO1 AIO,IO1+IOACCMST+IORDUP                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO1                                                          
         GOTO1 VALNAM              TAKE CARE OF ANY NAME CHANGE                 
         BNE   EXITN                                                            
                                                                                
         XC    IOKEY,IOKEY         READ CONTINUATION RECORD INTO IO2            
         MVC   IOKEY(L'PBCKEY),PBCKEY                                           
         MVI   IOKEY+(PBCKSEQ-PBCKEY),1  SET SEQUENCE NUMBER                    
         L     R2,AIO2                                                          
C        USING PBCRECD,R2                                                       
         GOTO1 AIO,IO2+IOACCDIR+IORDD                                           
         BNE   SAVE01                                                           
         OI    SVINDS,SVICFND                                                   
         GOTO1 AIO,IO2+IOACCMST+IOGETRUP                                        
         B     SAVE02                                                           
SAVE01   TM    IOERR,IOEDEL                                                     
         BZ    SAVE01A                                                          
         OI    SVINDS,SVICFND                                                   
         GOTO1 AIO,IO2+IOACCMST+IOGETRUP                                        
SAVE01A  MVC   C.PBCKEY,IOKEYSAV   BUILD KEY IN CASE OF ADD                     
         MVC   C.PBCRLEN,=AL2(PBCRFST+1-PBCRECD)                                
         XC    C.PBCRSTA,C.PBCRSTA                                              
         XC    C.PBCRLNK,C.PBCRLNK                                              
         MVI   C.PBCRFST,0                                                      
                                                                                
SAVE02   GOTO1 GETTSAR,0           GET NEXT NON BLANK TSAR RECORD               
         BNE   SAVE08                                                           
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('BLFELQ',PBCKEY),(2,BLFFLD)         
         TM    SVINDS,SVICFND                                                   
         BZ    SAVE04                                                           
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('BLFELQ',AIO2),(2,BLFFLD)           
                                                                                
SAVE04   CLI   BLFLINF,0           TEST IF ANY BLFEL DATA ON THIS TSAR          
         BE    SAVE02                                                           
         LH    R1,PBCRLEN          TEST IF ELEMENT WILL FIT ON RECORD 1         
         AH    R1,=Y(BLFLNQ)                                                    
         CH    R1,=Y(IOAREALN-L'IODA-L'IOWORK-PBCRRESQ)                         
         BH    SAVE06              NO                                           
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),PBCKEY,BLFEL,0,0                     
         CLI   12(R1),0                                                         
         BE    SAVE02                                                           
         DC    H'0'                                                             
SAVE06   GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO2,BLFEL,0,0                       
         CLI   12(R1),0                                                         
         BE    SAVE02                                                           
         DC    H'0'                                                             
                                                                                
SAVE08   GOTO1 AIO,IO1+IOACCMST+IOWRITE  WRITE BACK CHANGED RECORD 0            
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    SVINDS,SVICFND      TEST CONT REC. ALREADY IN USE                
         BO    SAVE10                                                           
         CLI   C.PBCRFST,0         NO, ANYTHING TO ADD ?                        
         BE    SAVE20                                                           
         GOTO1 AIO,IO2+IOACCMST+IOADDREC    ADD NEW CONTINUATION REC            
         BE    SAVE20                                                           
         DC    H'0'                                                             
                                                                                
SAVE10   CLI   C.PBCRFST,0                                                      
         BNE   *+8                                                              
         OI    C.PBCRSTAT,X'80'                                                 
         GOTO1 AIO,IO2+IOACCMST+IOWRITE  WRITE BACK CHANGED RECORD 1            
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    IOKEY,IOKEY         AMEND DIRECTORY LEVEL STATUS                 
         MVC   IOKEY(L'PBCKEY),C.PBCKEY                                         
         GOTO1 AIO,IO2+IOACCDIR+IORDUPD                                         
         MVC   IOKEY+(PBCKSTAT-PBCKEY)(1),C.PBCRSTAT                            
         GOTO1 AIO,IO2+IOACCDIR+IOWRITE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
SAVE20   MVC   FVMSGNO,=AL2(AI$RCENK) RECORD CHANGED ENTER NEXT KEY             
         NI    SVINDS,FF-(SVIPWRT+SVICFND)                                      
         B     EXITY                                                            
         DROP  R3,C                                                             
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE FORMAT NAME                                     *         
***********************************************************************         
                                                                                
VALNAM   NTR1  ,                                                                
         GOTO1 AFVAL,CONNAMH                                                    
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITN                                                            
         GOTO1 VHELLO,BOPARM,(C'D',ACCMST),('NAMELQ',AIO1),0                    
         XC    BOELEM,BOELEM                                                    
         LA    R2,BOELEM                                                        
         USING NAMELD,R2                                                        
         MVI   NAMEL,NAMELQ                                                     
         IC    RF,FVILEN                                                        
         LA    RF,NAMLN1Q(RF)                                                   
         STC   RF,NAMLN                                                         
         MVC   NAMEREC,FVIFLD                                                   
         GOTO1 VHELLO,BOPARM,(C'P',ACCMST),AIO1,NAMELD                          
         B     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET TOP/BOTTOM MAXIMUM LINE NUMBERS                      *         
* NTRY: P1 = A(ENTRY IN SECDATA LIST)                                 *         
***********************************************************************         
                                                                                
SETMAX   NTR1  ,                                                                
         LR    R3,R1                                                            
         USING SECDATAD,R3                                                      
         MVC   SSEC,SORTSEC                                                     
         CLI   BLFLINF,0                                                        
         BE    EXIT                                                             
                                                                                
         CLI   BLFFLD,BLFFGRSQ                                                  
         BE    SETMAX02                                                         
         CLI   STOTTYP,0                                                        
         BNE   SETMAX04                                                         
         CLI   BLFFLD,BLFFNETQ                                                  
         BNE   SETMAX04                                                         
SETMAX02 MVC   STOTTYP,BLFFLD                                                   
SETMAX04 IC    RF,BLFLINF                                                       
         IC    RE,BLFLINN                                                       
         BCTR  RE,0                                                             
         AR    RF,RE                                                            
         LA    R1,SMAXTOP                                                       
         TM    BLFOPT2,BLFOBOT                                                  
         BZ    *+8                                                              
         LA    R1,SMAXBOT                                                       
         CLM   RF,1,0(R1)                                                       
         BNH   *+8                                                              
         STC   RF,0(R1)                                                         
         B     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ROUTINE TO SET DISPLACEMENT                                         *         
***********************************************************************         
                                                                                
SETDSP   NTR1  ,                                                                
         LR    R3,R1                                                            
         USING SECDATAD,R3                                                      
         CLI   STOTTYP,0                                                        
         BNE   SETDSP02                                                         
         MVC   FVMSGNO,=AL2(AE$FLDDE)                                           
         MVI   BOHALF1,BLFFGRSQ                                                 
         MVI   BOHALF2,BLFFNETQ                                                 
         GOTO1 PRTERR,BOPARM,SSEC,BOHALF1,BOHALF2                               
         B     EXITN                                                            
                                                                                
SETDSP02 MVI   SDISTOP,0           TOP DISPLACEMENT = 0                         
         MVC   SDISBOT,SMAXTOP     BOTTOM DISPLACEMENT = TOP MAX.               
         CLC   SMAXBOT,SMAXTOP     TEST BOTTOM MAX > TOP MAX                    
         BNH   EXITY                                                            
         MVC   SDISBOT,SMAXBOT     BOTTOM DISPLACEMNT = BOTTOM MAX.             
         IC    RF,SMAXBOT                                                       
         IC    RE,SMAXTOP          TOP DISLACEMENT                              
         SR    RF,RE                 = BOTTOM MAX - TOP MAX                     
         STC   RF,SDISTOP                                                       
         B     EXITY                                                            
         EJECT                                                                  
                                                                                
ERRNOTV  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITN                                                            
                                                                                
ERRRECNF MVC   FVMSGNO,=AL2(AE$RECNF)                                           
         B     EXITN                                                            
                                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
                                                                                
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET HEADLINE TO DEFAULT DICTIONARY WORD                  *         
* NTRY: P1=A(BLFEL LIST ENTRY), P2=A(FMTTAB ENTRY), P3=A(HEADLINE)    *         
***********************************************************************         
                                                                                
HEDDIC   NTR1  ,                                                                
         LM    R2,R4,0(R1)                                                      
         USING BLFELD,R2                                                        
         USING FMTTABD,R3                                                       
         USING BLFHED1,R4                                                       
                                                                                
         MVC   BLFHED1,BCSPACES                                                 
         MVI   BLFHED1,AC#ESCL                                                  
         MVC   BLFHED1+1(2),FMTDIC                                              
         CLI   BLFCOLN,3                                                        
         BH    HEDDIC02                                                         
         MVI   BLFHED1,AC#ESCL3                                                 
         BE    HEDDIC04                                                         
         MVI   BLFHED1,AC#ESCL2                                                 
         B     HEDDIC04                                                         
                                                                                
HEDDIC02 IC    RE,BLFCOLN                                                       
         TM    FMTINDS1,FMTINUM                                                 
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         STC   RE,BLFHED1+3                                                     
         CLI   BLFHED1+3,L'BLFHED1                                              
         BNH   *+8                                                              
         MVI   BLFHED1+3,L'BLFHED1                                              
                                                                                
HEDDIC04 ICM   RF,15,=C'SL  '                                                   
         ICM   RF,1,SVLANG                                                      
         GOTO1 VDICTAT,BOPARM,(RF),BLFHED1                                      
                                                                                
HEDDICX  B     EXIT                                                             
         DROP  R2,R3,R4                                                         
                                                                                
***********************************************************************         
* ROUITINE TO UNDERLINE HEADLINE   NTRY: R1 = A(BLFHED1)              *         
***********************************************************************         
                                                                                
HEDUND   NTR1  ,                                                                
         USING BLFHED1,R1                                                       
         MVC   BLFHED2,BLFHED1                                                  
         LA    R0,L'BLFHED2                                                     
         LA    RF,BLFHED1+L'BLFHED1-1                                           
HEDUND02 CLI   0(RF),C' '                                                       
         BH    HEDUND04                                                         
         BCTR  RF,0                                                             
         BCT   R0,HEDUND02                                                      
         B     HEDUNDX                                                          
HEDUND04 MVI   L'BLFHED1(RF),C'-'                                               
         BCTR  RF,0                                                             
         BCT   R0,HEDUND04                                                      
HEDUND06 CLI   1(RF),C' '                                                       
         BH    HEDUNDX                                                          
         MVI   L'BLFHED1+1(RF),C' '                                             
         LA    RF,1(RF)                                                         
         B     HEDUND06                                                         
                                                                                
HEDUNDX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO VALIDATE TWO NUMBERS SEPERATED BY A COMMA               *         
* NTRY: P1 = (1ST NUMBER DEFAULT, A(OUTPUT FOR 1ST NUMBER))           *         
*       P2 = (2ND NUMBER DEFAULT, A(OUTPUT FOR 2ND NUMBER))           *         
*       P3 = MAXIMUM LINE/COLUMN NUMBER                               *         
*       P4 = A(MIMIMUM/MAXIMUM)                                       *         
*       P5 = BASE ERROR MESSAGE NUMBER                                *         
*   FVIFLD = INPUT                                                    *         
* EXIT: CC LOW=ERROR, CC HI=OK-VE, CC EQ=OK+VE                        *         
***********************************************************************         
                                                                                
VALNOS   NTR1  ,                                                                
         LM    R2,R6,0(R1)                                                      
         XC    BOELEM,BOELEM                                                    
         ICM   RF,12,=C',='                                                     
         ICM   RF,2,BCCOMMA                                                     
         ICM   RF,1,BCEQUAL                                                     
         GOTO1 VSCANNER,BOPARM,FVIHDR,(2,BOELEM),(RF)                           
                                                                                
         GOTO1 VALNO,BOPARM,BOELEM,(R2)                                         
         BL    EXITL                                                            
         GOTO1 (RF),(R1),BOELEM+SCBLKLQ,(R3)                                    
         BNE   EXITL                                                            
                                                                                
         CLM   R4,1,0(R2)          TEST FIRST NUMBER OKAY                       
         BNL   *+12                                                             
         STCM  R6,3,FVMSGNO                                                     
         B     EXITL                                                            
                                                                                
         CLC   0(1,R3),0(R5)       TEST MINIMUM                                 
         BL    *+14                                                             
         CLC   0(1,R3),1(R5)       TEST MAXIMUM                                 
         BNH   VNOS10                                                           
         L     RF,AFMT                                                          
         GOTO1 SUBDESC,FMTDIC-FMTTABD(RF)                                       
         GOTO1 SUBNUM,0(R5)                                                     
         LA    R6,1(R6)                                                         
         CLC   0(1,R5),1(R5)       TEST MINIMUM=MAXIMUM                         
         BE    VNOS02                                                           
         LA    R6,1(R6)                                                         
         GOTO1 SUBNUM,1(R5)                                                     
VNOS02   STCM  R6,3,FVMSGNO                                                     
         B     EXITL                                                            
                                                                                
VNOS10   XR    RE,RE               TEST SUM OF NUMBERS OKAY                     
         IC    RE,0(R2)                                                         
         XR    RF,RF                                                            
         IC    RF,0(R3)                                                         
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         CR    R4,RE                                                            
         BNL   *+12                                                             
         STCM  R6,3,FVMSGNO                                                     
         B     EXITL                                                            
                                                                                
VALNOSY  CLI   BOELEM+(SC1STVAL-SCANBLKD),FF                                    
         BE    EXITH               CC=HIGH IF FIRST NUMBER IS NEGATIVE          
         B     EXITY                                                            
                                                                                
VALNO    NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING SCANBLKD,R2                                                      
         MVI   SC1STVAL,0                                                       
         CLI   SC2NDLEN,0                                                       
         BNE   EXITL                                                            
         CLI   SC1STLEN,0                                                       
         BE    VALNOX                                                           
                                                                                
         LA    R1,SC1STFLD         R1=A(INPUT)                                  
         XR    R0,R0                                                            
         IC    R0,SC1STLEN         R0=L(INPUT)                                  
         CLI   0(R1),C'-'          TEST 1ST CHARACTER FOR MINUS                 
         BNE   VALNO02                                                          
         LA    R1,1(R1)                                                         
         B     VALNO04                                                          
VALNO02  LR    RF,R0               TEST LAST CHARACTER FOR MINUS                
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         CLI   0(RF),C'-'                                                       
         BNE   VALNO10                                                          
                                                                                
VALNO04  BCTR  R0,0                                                             
         MVI   SC1STVAL,FF         SET NUMBER IS NEGATIVE                       
                                                                                
VALNO10  XR    RF,RF                                                            
         LTR   R0,R0                                                            
         BZ    EXITL                                                            
                                                                                
VALNO12  TM    0(R1),C'0'          TEST NUMERICAL FIGURE                        
         BNO   EXITL                                                            
         IC    RE,0(R1)                                                         
         SLL   RE,28                                                            
         SRL   RE,28                                                            
         MH    RF,=Y(10)                                                        
         AR    RF,RE                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,VALNO12                                                       
                                                                                
         LA    RE,256                                                           
         CR    RF,RE                                                            
         BNL   EXITL                                                            
         STC   RF,0(R3)                                                         
                                                                                
VALNOX   CLI   0(R3),0             CHECK NON-ZERO                               
         BE    EXITL                                                            
         CLI   SC1STVAL,FF                                                      
         BE    EXITH               CC=HIGH IF NUMBER IS NEGATIVE                
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
*  ROUTINE TO DISPLAY TWO NUMBERS SEPERATED BY A COMMA                *         
* NTRY: P1 BYTE 0 = X'80' TO IGNORE 2ND NUMBER IF = 1                 *         
*             1-3 = A(1ST NUMBER)                                     *         
*       P2=       = A(2ND NUMBER)                                     *         
* EXIT: FVIFLD=OUTPUT                                                 *         
***********************************************************************         
                                                                                
DISNOS   NTR1  ,                                                                
         LR    R4,R1               R4=A(PARAMETER LIST)                         
         LM    R2,R3,0(R4)                                                      
         LA    RF,FVIFLD                                                        
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         BAS   RE,DISNO                                                         
         TM    0(R4),X'80'         TEST IGNORE 2ND NUMBER IF = 1                
         BZ    *+12                                                             
         CLI   0(R3),1                                                          
         BE    EXIT                                                             
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNE   *-8                                                              
         MVC   0(1,RF),BCCOMMA                                                  
         LA    RF,1(RF)                                                         
         IC    R1,0(R3)                                                         
         BAS   RE,DISNO                                                         
         B     EXIT                                                             
                                                                                
DISNO    CVD   R1,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  0(3,RF),BODUB1                                                   
         CLI   0(RF),C'0'                                                       
         BNER  RE                                                               
         MVC   0(3,RF),1(RF)                                                    
         CLI   0(RF),C'0'                                                       
         BNER  RE                                                               
         MVC   0(3,RF),1(RF)                                                    
         BR    RE                                                               
                                                                                
***********************************************************************         
* ROUTINE TO ADD DESCRIPTION TO ERROR MESSAGE SUBSTITUTION PARMS      *         
* R1=A(DICTIONARY REF#)                                               *         
***********************************************************************         
                                                                                
SUBDESC  NTR1  ,                                                                
         LA    R2,FVPARMS                                                       
         XR    RF,RF                                                            
         ICM   RF,1,0(R2)                                                       
         BZ    *+8                                                              
         BXH   R2,RF,*-8                                                        
         MVI   0(R2),DTYPELNQ+1                                                 
         MVI   1(R2),AC#ESCL                                                    
         MVC   2(2,R2),0(R1)                                                    
         MVI   4(R2),DTYPELNQ                                                   
         GOTO1 VDICTAT,BOPARM,C'SL  ',1(R2)                                     
         OC    1(DTYPELNQ,R2),BCSPACES                                          
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO ADD NUMBER TO ERROR MESSAGE SUBSTITUTION PARMS           *         
* R1=A(1 BYTE NUMBER)                                                 *         
***********************************************************************         
                                                                                
SUBNUM   NTR1  ,                                                                
         LA    R2,FVPARMS                                                       
         XR    RF,RF                                                            
         ICM   RF,1,0(R2)                                                       
         BZ    *+8                                                              
         BXH   R2,RF,*-8                                                        
         MVI   0(R2),4                                                          
         EDIT (1,(R1)),(3,1(R2)),ALIGN=LEFT                                     
         B     EXIT                                                             
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE LANGUAGE FIELD FOR GERMANY                      *         
***********************************************************************         
                                                                                
VALLNG   NTR1  ,                                                                
         CLI   CUCTRY,CTRYGER                                                   
         BNE   EXITY                                                            
         GOTO1 AFVAL,CONLNGH                                                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         MVI   PBCKLANG,0                                                       
         B     EXITY                                                            
                                                                                
         L     R2,ALANG                                                         
         LA    R2,6(R2)                                                         
         USING LANGTABD,R2                                                      
VLNG02   CLI   LANGTABD,FF                                                      
         BNE   *+18                                                             
         MVC   FVMSGNO,=AL2(FVFELANG)                                           
         OI    CINDS,CIERR                                                      
         B     EXITN                                                            
         GOTO1 CMPWRD,LANGSHR                                                   
         BE    VLNG10                                                           
         GOTO1 CMPWRD,LANGSHRN                                                  
         BE    VLNG10                                                           
         GOTO1 CMPWRD,LANGFUL                                                   
         BE    VLNG10                                                           
         GOTO1 CMPWRD,LANGFULN                                                  
         BE    VLNG10                                                           
VLNG08   LA    R2,LANGTABL(R2)                                                  
         B     VLNG02                                                           
                                                                                
VLNG10   MVC   PBCKLANG,LANGCODE                                                
         CLC   PBCKLANG,CULANG                                                  
         BNE   *+12                                                             
         MVI   PBCKLANG,0                                                       
         B     EXITY                                                            
         CLI   PBCKLANG,LANGENG                                                 
         BNE   EXITY                                                            
         MVI   PBCKLANG,LANGEUK                                                 
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY LANGUAGE FIELD FOR GERMANY                       *         
***********************************************************************         
                                                                                
DISLNG   NTR1  ,                                                                
         CLI   CUCTRY,CTRYGER                                                   
         BNE   EXITY                                                            
         L     R2,ALANG                                                         
         LA    R2,6(R2)                                                         
         MVC   BOBYTE1,0(R1)                                                    
         CLI   BOBYTE1,0                                                        
         BNE   *+14                                                             
         MVC   BOBYTE1,CULANG                                                   
         B     DLNG02                                                           
         CLI   BOBYTE1,LANGEUK                                                  
         BNE   DLNG02                                                           
         MVI   BOBYTE1,LANGENG                                                  
         USING LANGTABD,R2                                                      
DLNG02   CLI   LANGTABD,FF                                                      
         BE    EXITN                                                            
         CLC   LANGCODE,BOBYTE1                                                 
         BE    DLNG10                                                           
         LA    R2,LANGTABL(R2)                                                  
         B     DLNG02                                                           
                                                                                
DLNG10   MVC   CONLNG(L'LANGFULN),LANGFULN                                      
         MVI   CONLNGH+5,L'LANGFULN                                             
         OI    CONLNGH+6,X'80'                                                  
         B     EXITY                                                            
         DROP  R2                                                               
                                                                                
***********************************************************************         
* COMPARE INPUT WITH SOME KIND OF A WORD    R1=A(WORD)                *         
***********************************************************************         
                                                                                
CMPWRD   LA    RF,2                                                             
         CLI   FVXLEN,2                                                         
         BH    *+8                                                              
         IC    RF,FVXLEN                                                        
         EX    RF,*+6                                                           
         BR    RE                                                               
         CLC   FVIFLD(0),0(R1)                                                  
                                                                                
***********************************************************************         
* GET NEXT TSAR RECORD OR RETURN WITH CC NEQ IF NO MORE               *         
* NTRY: R1=0 GET NEXT NON BLANK                                       *         
*       R1=A(SECTION NO.) - GET NEXT TSAR FOR THIS SECTION            *         
***********************************************************************         
                                                                                
GETTSAR  NTR   ,                                                                
         LR    R3,R1                                                            
         XR    R2,R2                                                            
GET02    ICM   R2,3,TLNUM                                                       
         LA    R2,1(R2)                                                         
         STCM  R2,3,TLNUM                                                       
         CLC   TLNUM,CSHIRECN                                                   
         BH    EXITN                                                            
         GOTO1 ATSARIO,TSAGET                                                   
         TM    TLINDS1,TLIPRO      IF PROTECTED THEN ITS A BLANK LINE           
         BO    GET02                                                            
         LTR   R3,R3                                                            
         BZ    EXITY                                                            
         CLC   SORTSEC,0(R3)                                                    
         BE    EXITY                                                            
         B     GET02                                                            
                                                                                
***********************************************************************         
* ROUTINE TO TEST SYSTEM./COUNTRY FILTERS    R1=A(SYSTEM/COUNTRY)     *         
***********************************************************************         
                                                                                
TESTSC   ICM   RF,3,0(R1)          TEST ALL SYSTEMS/COUTRIES                    
         BZR   RE                  YES - CC=EQUAL                               
                                                                                
         CLI   0(R1),0             TEST ALL SYSTEMS                             
         BE    TSC02                                                            
*&&UK*&& CLI   0(R1),SYSUK         MATCH ON UK SYSTEM                           
*&&US*&& CLI   0(R1),SYSUS         MATCH ON US SYSTEM                           
         BNER  RE                  RETURN WITH CC=NOT EQUAL                     
                                                                                
TSC02    CLI   1(R1),0             TEST ALL COUNTRIES                           
         BER   RE                                                               
         CLC   CUCTRY,1(R1)        MATCH ON CONNECTED COUNTRY                   
         BER   RE                                                               
         TM    1(R1),CTRYNOT       TEST ALL BUT A COUNTRY                       
         BZ    TESTSCN                                                          
         MVC   BYTE,1(R1)                                                       
         XI    BYTE,CTRYNOT                                                     
         CLC   BYTE,CUCTRY                                                      
         BNE   TESTSCY                                                          
                                                                                
TESTSCN  LTR   RE,RE               CC=NOT EQUAL                                 
         BR    RE                                                               
TESTSCY  CR    RE,RE               CC=EQUAL                                     
         BR    RE                                                               
         EJECT                                                                  
FF       EQU   X'FF'                                                            
DTYPELNQ EQU   11                  LENGTH OF 'DATA TYPE' FIELD                  
ACCMST   DC    C'ACCMST '                                                       
DQU      DC    CL(L'BASSRV)'=DQU'                                               
                                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,51,AC#FMTRP,32,C                                              
         SPEC  H2,51,AC#FMTRP,32,CU                                             
         SPEC  H1,107,AC#USRID,12,L                                             
         SPEC  H2,107,AC#REQR,12,L                                              
         SPEC  M1,1,AC#FRMNO,16,L                                               
         SPEC  END                                                              
                                                                                
DEFCLM   DS    0XL1                                                             
         DC    AL1(FT1#HD1)                                                     
         DC    AL1(FT1#HD2)                                                     
         DC    AL1(FT1#HAL)                                                     
         DC    AL1(FT1#HLI)                                                     
         DC    AL1(FT1#HPE)                                                     
         DC    AL1(EOT)                                                         
DEFCLML  EQU   *-DEFCLM                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
SECTABD  DSECT                     TABLE OF SECT NO.S AND NAMES                 
SECTNUM  DS    XL1                                                              
SECTNAME DS    XL(SECNMLNQ)                                                     
SECTABLN EQU   *-SECTABD                                                        
                                                                                
SORTD    DSECT                                                                  
SORTFMT  DS    XL(L'FMTFLD)        DATA TYPE/FLD NUMBER                         
SORTFMT2 DS    XL(L'FMTFLD)        FMTFLD IF (TOTAL) TYPE                       
SORTSEC  DS    XL(L'SECTNUM)       SECTION NUMBER                               
                                                                                
SECDATAD DSECT                                                                  
SDATA    DS    0XL6                                                             
SSEC     DS    XL1                 SECTION NUMBER                               
SMAXTOP  DS    XL1                 MAXIMUM LINE NUMBER FROM THE TOP             
SMAXBOT  DS    XL1                 MAXIMUM LINE NUMBER FROM THE BOTTOM          
SDISTOP  DS    XL1                 DISPLACEMENT FOR TOP LINES                   
SDISBOT  DS    XL1                 DISPLACEMNT FOR BOTTOM LINES                 
STOTTYP  DS    XL1                 BLFEL TOTAL TYPE                             
                                                                                
CLB0C    CSECT                                                                  
OPTTAB   DS    0X                                                               
*                                  DISPLAY=COLUMN CODES                         
         DC    AL2(UC8DSP-TWAD,UC3DSP-TWAD)                                     
         DC    AL1(OPTNRTN+OPTDFLTI,0)                                          
         DC    AL1(0,0,0,0,0,1,64,64)                                           
         DC    AL1(0)                                                           
         DC    AL2(OPTDISQ,0)                                                   
         DC    CL4'+'                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                  SECTION=X                                    
         DC    AL2(UC@SCNAM-TWAD,UC@SCNAM-TWAD)                                 
         DC    AL1(OPTNRTN,0)                                                   
         DC    AL1(0,0,0,0,2,1,6,L'LSSECT)                                      
         DC    AL1(1)                                                           
         DC    AL2(1,LSSECT-LSVALSD)                                            
         DC    AL4(0)                                                           
         DC    AL2(0,0)                                                         
         DC    XL4'00'                                                          
*                                                                               
OPTTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
*********************************************************************           
* VALIDATE OPTIONS                                                  *           
*********************************************************************           
                                                                                
         DROP  R6,R7,R8,RB                                                      
         DS    0D                                                               
VALOPT   NMOD1 0,**VALO**                                                       
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         LR    R4,R2               R4=A(OPTTAB ENTRY)                           
         USING OPTTABD,R4                                                       
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *(RF)                                                            
         B     VALSECT             1 SECTION=X (NUMBER OR NAME)                 
                                                                                
VALXY    CR    RB,RB                                                            
         B     *+6                                                              
VALXN    LTR   RB,RB                                                            
VALX     XIT1  ,                                                                
                                                                                
*********************************************************************           
* VALIDATE SECTION=X (NUMBER OR NAME)                               *           
*********************************************************************           
                                                                                
VALSECT  DS    0H                                                               
         XC    BCWORK,BCWORK                                                    
         CLI   FVILEN,1            TEST SINGLE CHARACTER                        
         BH    VALSEC04                                                         
         TM    FVIIND,FVINUM       TEST NUMERIC                                 
         BZ    VALSEC04                                                         
         MVC   BCWORK(1),FVIFLD                                                 
         NI    BCWORK,X'0F'                                                     
                                                                                
VALSEC04 XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         LA    R1,SECTAB                                                        
         USING SECTABD,R1                                                       
VALSEC06 CLI   SECTNUM,0                                                        
         BE    VALSECXN                                                         
         CLC   SECTNUM,BCWORK                                                   
         BE    VALXY                                                            
         MVC   WORK,SECTNAME                                                    
         OC    WORK,BCSPACES                                                    
         EX    RE,COMP                                                          
         BE    *+12                                                             
         LA    R1,SECTABLN(R1)                                                  
         B     VALSEC06                                                         
         MVC   BCWORK,SECTNUM                                                   
         B     VALXY                                                            
                                                                                
VALSECXN MVC   FVMSGNO,=AL2(FVFDINV)                                            
         B     VALXN                                                            
                                                                                
COMP     CLC   WORK(0),FVIFLD                                                   
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* INCLUDED DSECTS                                                     *         
***********************************************************************         
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
                                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
                                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
                                                                                
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
                                                                                
* ACCLBCOLS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLS                                                      
         PRINT ON                                                               
                                                                                
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORKB                                                     
                                                                                
WORKD    DSECT                                                                  
         ORG   AIO1                                                             
APBCREC  DS    A                   A(PRODUCTION BILL RECORD)                    
                                                                                
BLFELD   DSECT                     DEFINE DATA PART OF ELEMENT                  
BLFDATAL EQU   BLFLNQ-(BLFLINF-BLFELD)                                          
         ORG   BLFLINF                                                          
BLFDATA  DS    XL(BLFDATAL)                                                     
         ORG   BLFELD+BLFLNQ                                                    
BLFELC   DS    XL(BLFLNQ)          COST ELEMENT                                 
         ORG   BLFELC+(BLFDATA-BLFELD)                                          
BLFDATAC DS    XL(BLFDATAL)        COST ELEMENT DATA                            
         ORG   BLFELC+BLFLNQ                                                    
                                                                                
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBF3BD                                                      
                                                                                
***********************************************************************         
* SAVED W/S                                                           *         
***********************************************************************         
                                                                                
         ORG   OSVALS                                                           
SVWORK   DS    0XL(SVWORKL)                                                     
                                                                                
SVFKEY   DS    0XL2                                                             
SVFORM   DS    XL1                 SAVED FORMAT NUMBER                          
SVLANG   DS    XL1                 SAVED LANGUAGE CODE                          
SVR2     DS    F                   SAVED R2 FROM GNXT ROUTINE                   
SVR3     DS    F                   R3                                           
SVR4     DS    F                   R4                                           
SV1STBLF DS    XL(BLFLNQ)          BLFEL FOR 1ST SECTION OF DATA TYPE           
SECTAB   DS    (SECNM#Q+1)XL(SECTABLN)                                          
                                                                                
SVINDS   DS    XL1                 SAVED INDICATORS                             
SVIPWRT  EQU   X'80'               WRITE IS PENDING                             
SVICFND  EQU   X'40'               CONTINUATION RECORD ON FILE                  
                                                                                
SVOPS    DS    XL(LSFLTOPL)        SAVED OPTIONS                                
SVWORKL  EQU   *-SVINDS                                                         
         ORG   OSVALS+OSVALSL                                                   
LSVALSD  DSECT                                                                  
         ORG   LSOPS                                                            
LSOPTION DS    0XL(L'LSOPS-(*-LSOPS))                                           
LSSECT   DS    XL(L'SECTNUM)                                                    
         ORG   LSOPS+LSOPL                                                      
                                                                                
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
                                                                                
CWORKD   DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6A                                                               
WORK     DS    XL100                                                            
BYTE     DS    XL1                                                              
                                                                                
THISFKEY DS    0XL2                                                             
THISFORM DS    XL1                                                              
THISLANG DS    XL1                                                              
THISBLF  DS    XL(BLFLNQ)          BLFEL BEFORE VALIDATE/CHANGE                 
AFMT     DS    A                                                                
CINDS    DS    XL1                 INDICATORS                                   
CIFST    EQU   X'80'               FIRST TIME                                   
CIERR    EQU   X'40'               ERROR IN VALCLM ROUTINES                     
CIBLNK   EQU   X'02'               INSERT A BLANK LINE                          
CIFTYP   EQU   X'01'               FIRST FOR THIS DATA TYPE                     
COVRLAP1 DS    XL1                 FIELD OVERLAP 1                              
COVRLAP2 DS    XL1                 FIELD OVERLAP 2                              
         DS    0H                                                               
COVRLIN1 DS    H                   TLNUM OF LINE WITH OVERLAP 1                 
COVRLIN2 DS    H                   TLNUM OF LINE WITH OVERLAP 2                 
CCOLL    DS    XL1                 END OF FIELD COLUMN NUMBER                   
CPRTFLD  DS    CL132               OUTPUT PRINT LINE                            
CRACCHA  DS    XL(RACLNQ)          CURRENT CHANGE ACTIVITY DETAILS              
CFMTBLK  DS    XL(FBLKL)                                                        
SECDATA  DS    (SECNM#Q)XL(L'SDATA) * SECTION DATA *                            
                                                                                
PBLOCK   DS    0X                  * PRINT BLOCK *                              
PRH1     DS    CL132               HEADING 1                                    
PRH2     DS    CL132               HEADING 2                                    
PRL1     DS    CL132               PRINT LINE 1                                 
PRL2     DS    CL132               PRINT LINE 2                                 
PRL3     DS    CL132               PRINT LINE 3                                 
PRL4     DS    CL132               PRINT LINE 4                                 
PRL5     DS    CL132               PRINT LINE 5                                 
PRL6     DS    CL132               PRINT LINE 6                                 
PRL7     DS    CL132               PRINT LINE 7                                 
PRL8     DS    CL132               PRINT LINE 8                                 
PRL9     DS    CL132               PRINT LINE 9                                 
PRLN     EQU   (*-PRL1)/L'PRL1     NO. OF PRINT LINES                           
                                                                                
PBLOCKL  EQU   *-PBLOCK                                                         
                                                                                
         ORG   CWORKD+OVERWRKL                                                  
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACCLB0CB  05/01/02'                                      
         END                                                                    
