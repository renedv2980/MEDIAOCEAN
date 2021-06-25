*          DATA SET AGX5STUP   AT LEVEL 017 AS OF 04/09/20                      
*PHASE AGX5SUB                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE NUMVAL                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE ACRECTYP                                                               
                                                                                
         TITLE 'AGX5STUP - Extract Upadte via 5 step process'                   
                                                                                
***********************************************************************         
* Who  Lvl Date    Change(s)                                 Audit              
* ---- --- ------- ----------------------------------------- ----------         
* YNGX 017 13Mar20 Relink to include new TYPTAB              DSRD-23410         
* VGUP 016 09Apr20 Fix the once second add issue             ITMF-45537         
* YNGX 015 08May19 Include est deleted/restored in =Prod     DSRD-22480         
* VGUP 014 21MAY19 Used new DMRCVRHDR equates                DSRD-22598         
* YNGX 013 24Apr19 Bug fix in RESOLVE to skip test for del   DSRD-22214         
*                  Change RDATE for recs with same SIN/GIN   ITMF-35668         
* MPEN 012 18Apr19 Fix for global SINs                       ITMF-35626         
* TKLU 011 14Mar19 ACRTACTHfor ACC - TYPTABD change          DSRD-21820         
* TKLU 010 25Feb19 Order sequential delete fix               DSRD-21784         
* TKLU 009 19Feb19 Estimate sequential delete fix            DSRD-21683         
* TKLU 008 14Jan19 Snipping mechanism via SNIP=Y/SNIPFILE    ITMF-32820         
* TKLU 007 20Nov18 Handle PROMOTE of archived transaction UK DSRD-20785         
* TKLU 006 07Nov18 O&E Status Change: 'add a second'         DSRD-20686         
* TKLU 005 09Oct18 Manipulate SV_SEQ for none O&E            ITMF-29963         
* TKLU 004 04Sep18 US merge                                  DSRD-20010         
* TKLU 003 05Jan18 Add TYPE=TRN and TYPE=EXP                 DSRD-17376         
* TKLU 002 06Nov17 Bug fix ADDTIM and GIN= filter            DSRD-16206         
* TKLU 001 02Nov17 BulkAPI: Enable 'Daily Summary Updates'   DSRD-16206         
*                  Initial Version for BulkAPI Extract       DSRD-13993         
***********************************************************************         
*                                                                     *         
* Routines for '5 Step Daily Updates' Bulk API process                *         
* ====================================================                *         
*                                                                     *         
* Step 1: Utility to read in Accounting recovery file/tape and filter *         
* ======= it on required records for further steps/processing.        *         
*                                                                     *         
* Sample JCL: TKLU.DDS.JCL(GP5ST1T)                                   *         
* -----------                                                         *         
*                                                                     *         
* Mandatory SYSIN cards                                               *         
* ---------------------                                               *         
* MODE=FILTER                                                         *         
*                                                                     *         
* SINFIL= (DSN) used to output SINTAB                                 *         
*                                                                     *         
* INPUT=R(ecovery file)/T(ape)                                        *         
*   Specifies whether a recovery file (R) or a DDFA tape (T) is       *         
*   being processed.                                                  *         
*                                                                     *         
* DSPACE=T/C/A/Q/B                                                    *         
*   Standard DSPACE card.                                             *         
*                                                                     *         
* WHICHSYS=AA                                                         *         
*   Accounting system.                                                *         
*                                                                     *         
* Optional SYSIN cards                                                *         
* --------------------                                                *         
* These cards are used to filter the records read.                    *         
*                                                                     *         
* AGENCY=xx(two char alpha)                                           *         
*   Agency two character alpha ID.                                    *         
*                                                                     *         
* STTIME=HH:MM:SS                                                     *         
*   Compared against RTIME in the recovery header.                    *         
*                                                                     *         
* ENDTIME=HH:MM:SS                                                    *         
*   Compared against RTIME in the recovery header.                    *         
*                                                                     *         
* SIN=xxxx                                                            *         
*   The system input number in the recovery header (RSIN).            *         
*                                                                     *         
* GIN=xxxxxxxx                                                        *         
*   The general input number in the recovery header (RGIN).           *         
*                                                                     *         
* KEY=xx(xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)                      *         
* KY2= - use KEY for 21 bytes, if longer use KY2, too.                *         
*                                                                     *         
* SNIP=YES                                                            *         
*   Read in SNIPFILE for recovery records to be snipped out.          *         
*                                                                     *         
*                                                                     *         
* Step 2: Sort recovery file by ACCKEY, RSIN, RDATE, RTIME and RVCHR  *         
* ======= to bring it into right sequence for step 3 processing.      *         
*                                                                     *         
* Sample JCL: TKLU.DDS.JCL(GP5ST2T)                                   *         
* -----------                                                         *         
*                                                                     *         
*                                                                     *         
* Step 3: Merge recovery records first/last as specified and          *         
* ======= manipulate date/time stamps for final extract run.          *         
*                                                                     *         
* Sample JCL: TKLU.DDS.JCL(GP5ST3T)                                   *         
* -----------                                                         *         
*                                                                     *         
* Mandatory SYSIN cards                                               *         
* ---------------------                                               *         
* MODE=MERGE                                                          *         
*                                                                     *         
* SINFIL= (DSN) used to input SINTAB                                  *         
*                                                                     *         
* INPUT=T(recovery tape)                                              *         
*   Specifies step 2 recovery tape.                                   *         
*                                                                     *         
* DSPACE=T/C/A/Q/B                                                    *         
*   Standard DSPACE card.                                             *         
*                                                                     *         
* WHICHSYS=AA                                                         *         
*   Accounting system.                                                *         
*                                                                     *         
* Optional SYSIN cards                                                *         
* --------------------                                                *         
*                                                                     *         
* SNIP=YES                                                            *         
*   Skip PRO95 check (RSIN not ascending on the day)                  *         
*                                                                     *         
*                                                                     *         
* Step 4: Sort recovery file by RSIN, RTIME and RVCHR to bring it     *         
* ======= back into sequence for AGXTRACT processing.                 *         
*                                                                     *         
* Sample JCL: TKLU.DDS.JCL(GP5ST4T)                                   *         
* -----------                                                         *         
*                                                                     *         
*                                                                     *         
* Step 5: AGXTRACT in MODE=UPDATE                                     *         
* =======                                                             *         
*                                                                     *         
* Sample JCL: TKLU.DDS.JCL(GP5ST5T)                                   *         
* -----------                                                         *         
*                                                                     *         
***********************************************************************         
                                                                                
AGX5STUP CSECT                                                                  
         PRINT NOGEN                                                            
         COPY  IEABRC                                                           
         NBASE WORKX-WORKD,**AGX5**,WORK=A(WORKC),CLEAR=YES                     
         USING WORKD,RC                                                         
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         J     MAIN                                                             
                                                                                
$$DATA   LOCTR ,                                                                
$$CODE   LOCTR ,                                                                
                                                                                
MAIN     DS    0H                                                               
                                                                                
         GOTOR INITIAL             Init storage and validate cards              
         GOTOR OPENSYS,THISSENO    Open all files and read CPYRECs              
                                                                                
         MVI   RECIND,0                                                         
         MVI   LASTRTY,0                                                        
         XC    LASTKEY,LASTKEY                                                  
         XC    LASTMKY,LASTMKY                                                  
         XC    SAVDATA,SAVDATA                                                  
                                                                                
         CLI   RUNMODE,RUNM_FLT    Filter mode = Step 1?                        
         JE    M_F02                                                            
                                                                                
         CLI   RUNMODE,RUNM_MRG    Merge mode = Step 3?                         
         JE    M_M02                                                            
                                                                                
M_F02    DS    0H                                                               
*&&UK                                                                           
         GOTO1 VDATCON,DMCB,(5,0),(0,TEMP+0)                                    
         GOTO1 VADDAY,DMCB,(C'Y',TEMP+0),TEMP+6,-1                              
         GOTO1 VDATCON,DMCB,(0,TEMP+6),(2,TODL1Y)                               
*&&                                                                             
                                                                                
         CLI   SNIP,YESQ           Snipping?                                    
         JNE   M_F04                                                            
         GOTOR SNIPSUP             SNIPTAB set up                               
                                                                                
M_F04    GOTOR READRCV,AIO1        Read next recovery record                    
         JNE   M_F08               all read in                                  
                                                                                
         GOTOR FILTER,AIO1         Filter record                                
         JNE   M_F04               Record not required                          
                                                                                
         TM    ESTIND,ESTIPRDM     Est main record changed in =Prod             
         JZ    M_F06               No                                           
         GOTOR FLTEST,AIO1         Filter estimate main record                  
         JNE   M_F04               Estimate record not required                 
                                                                                
M_F06    GOTOR ADDEXT,AIO1         Add recovery extension if neccessary         
                                                                                
         GOTOR ADDSIN,AIO1         Check and add to SIN table                   
                                                                                
         GOTOR ADDTIM,AIO1         Add to or set from TIM table                 
                                                                                
         GOTOR SETSEQ,AIO1         Set SV_SEQ/SV_ENT                            
                                                                                
         GOTOR MANACT,AIO1         Manipulate action                            
                                                                                
         GOTOR PUTREC,AIO1         Put recovery record                          
                                                                                
         TM    ESTIND,ESTIPRDM+ESTICHG                                          
         JZ    M_F04               Not est main rec changed in =Prod            
         JO    M_F04               Changed record processed already             
         OI    ESTIND,ESTICHG                                                   
         L     R0,AIO1                                                          
         LA    R1,IOAREALN                                                      
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE               Copy Changed estimate record to AIO1         
         J     M_F06               Process estimate main changed record         
                                                                                
M_F08    GOTOR OUTSIN              Output SIN table                             
                                                                                
         CLI   SNIP,YESQ           Snipping?                                    
         JNE   M_F10                                                            
         GOTOR SNIPEND             SNIPTAB close                                
                                                                                
M_F10    DS    0H                                                               
         J     END                 Exit if end of file                          
                                                                                
M_M02    GOTOR GETSIN                                                           
                                                                                
M_M04    GOTOR READRCV,AIO1        Read next recovery record                    
         JE    M_M06               Exit if end of file                          
                                                                                
         OI    RECIND,RECIEND                                                   
                                                                                
M_M06    GOTOR MANACT,AIO1         Manipulate action                            
                                                                                
         GOTOR PROCIT,AIO1         Process filter record                        
                                                                                
         TM    RECIND,RECIEND                                                   
         JZ    M_M04                                                            
                                                                                
END      DS    0H                                                               
         GOTOR CLOSE               Close files                                  
                                                                                
         XBASE RC=RETCODE,RL=1                                                  
                                                                                
* AGXTYPTAB                                                                     
       ++INCLUDE AGXTYPTAB                                                      
                                                                                
* ACRECEQUS                                                                     
       ++INCLUDE ACRECEQUS                                                      
                                                                                
LOADALL  DC    A(0)                                                             
UPDTALL  EQU   LOADALL                                                          
LOADCPY  EQU   LOADALL                                                          
UPDTCPY  EQU   LOADALL                                                          
LOADCLI  EQU   LOADALL                                                          
UPDTCLI  EQU   LOADALL                                                          
LOADPRO  EQU   LOADALL                                                          
UPDTPRO  EQU   LOADALL                                                          
LOADJOB  EQU   LOADALL                                                          
UPDTJOB  EQU   LOADALL                                                          
LOADETY  EQU   LOADALL                                                          
UPDTETY  EQU   LOADALL                                                          
LOADWCO  EQU   LOADALL                                                          
UPDTWCO  EQU   LOADALL                                                          
LOADCAT  EQU   LOADALL                                                          
UPDTCAT  EQU   LOADALL                                                          
LOADACC  EQU   LOADALL                                                          
UPDTACC  EQU   LOADALL                                                          
LOADPER  EQU   LOADALL                                                          
UPDTPER  EQU   LOADALL                                                          
LOADTRN  EQU   LOADALL                                                          
UPDTTRN  EQU   LOADALL                                                          
LOADXTR  EQU   LOADALL                                                          
UPDTXTR  EQU   LOADALL                                                          
LOADORD  EQU   LOADALL                                                          
UPDTORD  EQU   LOADALL                                                          
LOADEST  EQU   LOADALL                                                          
UPDTEST  EQU   LOADALL                                                          
LOADEXP  EQU   LOADALL                                                          
UPDTEXP  EQU   LOADALL                                                          
LOADTIM  EQU   LOADALL                                                          
UPDTTIM  EQU   LOADALL                                                          
LOADITM  EQU   LOADALL                                                          
UPDTITM  EQU   LOADALL                                                          
LOADCUR  EQU   LOADALL                                                          
UPDTCUR  EQU   LOADALL                                                          
LOADOFF  EQU   LOADALL                                                          
UPDTOFF  EQU   LOADALL                                                          
LOADTOF  EQU   LOADALL                                                          
UPDTTOF  EQU   LOADALL                                                          
                                                                                
FFQ      EQU   X'FF'                                                            
SEQ_IS_1 EQU   X'01'                                                            
SEQ_IS_0 EQU   X'00'                                                            
SEQ_OFST EQU   X'80'                                                            
                                                                                
***********************************************************************         
* Subroutines                                                         *         
* -----------                                                         *         
***********************************************************************         
                                                                                
***********************************************************************         
* Output SIN table                                                    *         
***********************************************************************         
                                                                                
OUTSIN   NTR1  ,                                                                
                                                                                
         OPEN  (SINOUT,OUTPUT)     Open file for output                         
                                                                                
         SAM31 ,                                                                
         USING SINTABD,R2                                                       
         L     R2,ASINTAB                                                       
                                                                                
OUTS2    CLI   SINTIND,FFQ         Done?                                        
         JE    OUTS4                                                            
         XC    ELEM,ELEM                                                        
         MVC   ELEM(SINTLNQ),SINTABD                                            
         SAM24 ,                                                                
         LA    R3,ELEM                                                          
         PUT   SINOUT,0(R3)        Move it out                                  
         SAM31 ,                                                                
         AHI   R2,SINTLNQ                                                       
         J     OUTS2                                                            
                                                                                
OUTS4    SAM24 ,                                                                
         DROP  R2                                                               
                                                                                
         CLOSE (SINOUT)            Close file                                   
                                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Get SIN table                                                       *         
***********************************************************************         
                                                                                
GETSIN   NTR1  ,                                                                
                                                                                
         OPEN  (SININ,INPUT)       Open file for input                          
                                                                                
         SAM31 ,                                                                
         USING SINTABD,R2                                                       
         L     R2,ASINTAB                                                       
         LHI   R4,SINTMAX                                                       
                                                                                
GETS2    SAM24 ,                                                                
         LA    R3,ELEM                                                          
         GET   SININ,(R3)                                                       
         SAM31 ,                                                                
         MVC   SINTABD(SINTLNQ),0(R3)                                           
         AHI   R2,SINTLNQ                                                       
         AP    COUNTSIN,PONE                                                    
         JCT   R4,GETS2                                                         
         SAM24 ,                                                                
                                                                                
         MVC   P(28),=CL28'Increase of SINTMAX required'                        
         LA    R5,520                                                           
         GOTOR VPRINTER                                                         
         ABEND (R5),DUMP                                                        
         DROP  R2                                                               
                                                                                
         CLOSE (SININ)             Close file                                   
                                                                                
GETS8    DS    0H                  (End of Tape entry point)                    
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Filter record                                                       *         
* NTRY - R1=A(I/O Area)                                               *         
* EXIT - CC EQ: Record required                                       *         
*        CC NE: Record not required                                   *         
***********************************************************************         
         USING RRECD,R3            R3 used for recovery base                    
         USING ACCRECD,RRECORD                                                  
FILTER   NTR1  ,                                                                
         L     R3,0(R1)            A(Record)                                    
                                                                                
* Apply SNIPFILE if set up                                                      
         CLI   SNIP,YESQ                                                        
         JNE   FILT08                                                           
         SAM31 ,                                                                
         L     R4,ASNIPTAB                                                      
                                                                                
FILT02   CLI   0(R4),0                                                          
         JE    FILT06                                                           
         CLC   0(SNIPTLNQ,R4),RECVHDR                                           
         JE    FILT04                                                           
         AHI   R4,SNIPTLNQ                                                      
         J     FILT02                                                           
                                                                                
FILT04   SAM24 ,                                                                
         J     FILTBAD                                                          
                                                                                
FILT06   SAM24 ,                                                                
                                                                                
* File records (ACCMST/ACCARC only)                                             
FILT08   CLI   RFILTY,ACCMQ                                                     
         JE    FILT10                                                           
         CLI   RFILTY,ACCAQ                                                     
         JE    FILT10                                                           
         J     FILTBAD                                                          
                                                                                
* Exclude pointers and odd types                                                
FILT10   TM    RRECTY,X'80'                                                     
         JNZ   FILTBAD                                                          
         CLI   RRECTY,RRECTCPY                                                  
         JE    FILT12                                                           
         CLI   RRECTY,RRECTCHG                                                  
         JE    FILT12                                                           
         CLI   RRECTY,RRECTADD                                                  
         JE    FILT12                                                           
         J     FILTBAD                                                          
                                                                                
* System input number filter                                                    
FILT12   CLI   RSIN,RBACKOUT       Skip unwinds                                 
         JE    FILTBAD                                                          
         OC    FLTSIN,FLTSIN       Any system input number?                     
         JZ    FILT14                                                           
         CLC   RSIN+1(3),FLTSIN                                                 
         JNE   FILTBAD                                                          
                                                                                
* Time filters                                                                  
FILT14   DS    0H                                                               
         MVC   FULL,RTIME          Time from recovery                           
         NI    FULL,X'FF'-X'80'-X'40'   Clear flags                             
         OI    FULL+3,X'0F'        Force sign                                   
         OC    FLTSTTM,FLTSTTM                                                  
         JZ    FILT16                                                           
         CLC   FULL,FLTSTTM        Before start time ?                          
         JL    FILTBAD                                                          
                                                                                
FILT16   OC    FLTEDTM,FLTEDTM     Any end date filter?                         
         JZ    FILT18                                                           
         CLC   FULL,FLTEDTM        After end time ?                             
         JH    FILTBAD                                                          
                                                                                
* Company filter                                                                
FILT18   GOTO1 VRECTYP,DMCB,(C'D',ACCRECD)                                      
         MVC   RECTYP,0(R1)                                                     
         MVC   RECCPY,1(R1)                                                     
         LA    R1,AGYLIST                                                       
                                                                                
FILT20   CLI   0(R1),0                                                          
         JE    FILTBAD                                                          
         CLC   RECCPY(0),0(R1)                                                  
         JE    FILT22                                                           
         AHI   R1,AGYLNQ                                                        
         J     FILT20                                                           
                                                                                
* Type table record type filter                                                 
         USING TYPTABD,RF                                                       
FILT22   LA    RF,TYPTAB                                                        
                                                                                
FILT24   CLI   TYPNAME,FFQ                                                      
         JE    FILTBAD                                                          
         CLC   RECTYP,TYPRTYP      Does the record type match?                  
         JE    FILT28                                                           
         CLC   RECTYP,TYPRTY2                                                   
         JE    FILT28                                                           
         TM    TYPTYPE,AUDITQ      Audit required?                              
         JZ    FILT26                                                           
         CLI   RECTYP,ACRTAUDT                                                  
         JNE   FILT26                                                           
         CLC   TYPATYP,ACCRECD+AUDKAUDT-AUDKEY                                  
         JE    FILT28                                                           
                                                                                
FILT26   AHI   RF,TYPTABLQ                                                      
         J     FILT24                                                           
         DROP  RF                                                               
                                                                                
* Key filter                                                                    
FILT28   XR    RE,RE                                                            
         ICM   RE,B'0001',FLTKEYL  Length of key to filter                      
         JZ    FILT30                                                           
         BCTR  RE,0                                                             
         CLC   RRECORD(0),FLTKEY                                                
         EXRL  RE,*-6                                                           
         JNE   FILTBAD                                                          
                                                                                
* Program and other filters                                                     
FILT30   MVI   ESTIND,0            Clear estimate indicator                     
         CLI   RECTYP,ACRTESTR     =AURA and =PROD only for estimates           
         JNE   FILT32                                                           
         CLI   RPRG,RCVPBRAQ       From =AURA                                   
         JE    FILT36              Yes                                          
         CLI   RPRG,RCVPPROQ       From =Prod                                   
         JNE   FILTBAD             No - skip                                    
         CLI   ACCRECD+ESTKSEQ-ESTRECD,ESTKSMQ                                  
         JNE   FILT36              OK - not main estimate record                
         OI    ESTIND,ESTIPRDM     Set main estimate changed in =Prod           
         J     FILT36                                                           
                                                                                
FILT32   CLI   RECTYP,ACRTORD                                                   
         JNE   FILT34                                                           
         CLC   ACCRECD+ORDKORD-ORDRECD(L'ORDKORD),ORDCNT                        
         JE    FILTBAD             skip order control                           
         CLI   RPRG,RCVPPFMQ                                                    
         JE    FILTBAD             skip =PFM activity                           
*&&UK*&& CLI   RPRG,RCVPFLIQ                                                    
*&&US*&& CLI   RPRG,RCVPAFMQ                                                    
         JE    FILTBAD             skip =FLI/=AFM activity                      
         J     FILT36                                                           
                                                                                
FILT34   CLI   RECTYP,ACRTTRN                                                   
         JNE   FILT36                                                           
         CLC   ACCRECD+TRNKULA-TRNRECD(L'SJUL),SJUL                             
         JE    FILT36                                                           
         LA    R1,EXPLDGS                                                       
FILT35   CLC   ACCRECD+TRNKULA-TRNRECD(L'SJUL),0(R1)                            
         JE    FILT36                                                           
         LA    R1,L'SJUL(R1)                                                    
         CLI   0(R1),X'FF'         End of expense ledgers                       
         JNE   FILT35              No - check next ledger                       
         J     FILTBAD             Yes - skip none exp or SJ trans              
                                                                                
* General input number filter                                                   
FILT36   OC    FLTGIN,FLTGIN                                                    
         JZ    FILT38                                                           
         TM    RTIME,X'40'         skip if no GIN                               
         JZ    FILTBAD                                                          
         LH    RE,RRECLN                                                        
         SHI   RE,1                                                             
         AR    RE,R3                                                            
         LLC   R1,0(RE)                                                         
         SHI   R1,1                                                             
         SR    RE,R1                                                            
         CLC   FLTGIN,RGIN-RECVEXTD(RE)                                         
         JNE   FILTBAD             skip if different GIN                        
                                                                                
FILT38   DS    0H                                                               
*&&UK                                                                           
         CLI   RRECTY,RRECTADD     Skip PROMOTEd archived transactions          
         JNE   FILT42              (APXTRACT.INITPTM)                           
         CLI   RFILTY,ACCMQ        Note: This requires ANY PROMOTE call         
         JNE   *+2                 to do PROMOTE with the unchanged             
         CLI   RECTYP,ACRTTRN      record, followed by a GETREC/PUTREC          
         JNE   FILT42              for the changes                              
                                                                                
         USING TRNRECD,R1                                                       
         LA    R1,ACCRECD                                                       
         USING TRSELD,RF                                                        
         LA    RF,TRNRFST                                                       
                                                                                
FILT40   LLC   RE,TRSLN                                                         
         AR    RF,RE                                                            
         CLI   TRSEL,0                                                          
         JE    FILT42                                                           
         CLI   TRSEL,TRSELQ                                                     
         JNE   FILT40                                                           
         CLC   TRSDATE,TODL1Y                                                   
         JNL   FILT42                                                           
         J     FILTBAD                                                          
         DROP  R1,RF                                                            
*&&                                                                             
                                                                                
FILT42   DS    0H                                                               
                                                                                
FILTOK   CR    RB,RB               Set r/c EQ                                   
         J     EXIT                                                             
                                                                                
FILTBAD  CLI   *,0                 Set r/c NE                                   
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Filter estimate records changed by =Prod,                           *         
* Only include estimate activities when deleting/restoring jobs       *         
* contianing estimates in =Prod                                       *         
* NTRY - R1=A(I/O Area - COPY)                                        *         
* EXIT - CC EQ: Record required                                       *         
*        CC NE: Record not required                                   *         
***********************************************************************         
FR       USING RRECD,R3            R3 used for recovery base                    
TO       USING RRECD,R4            R4 used for recovery base                    
FLTEST   NTR1  ,                                                                
         L     R3,0(R1)            A(Record) - Copy                             
                                                                                
         GOTOR READRCV,AIO2        Read next estimate record                    
         JNE   *+2                 Record not found!!!                          
         L     R4,AIO2             A(Record) - Change                           
                                                                                
         MVC   HALF+0(1),FR.RRECORD+ESTRSTA1-ESTRECD                            
         NI    HALF+0,ESTKDELT                                                  
         MVC   HALF+1(1),TO.RRECORD+ESTRSTA1-ESTRECD                            
         NI    HALF+1,ESTKDELT                                                  
         CLC   HALF+0(1),HALF+1    Any change in ESTKDELT                       
         JE    FLTEBAD             No - skip                                    
                                                                                
FLTEOK   CR    RB,RB               Set r/c EQ                                   
         J     EXIT                                                             
                                                                                
FLTEBAD  CLI   *,0                 Set r/c NE                                   
         J     EXIT                                                             
         DROP  FR,TO                                                            
                                                                                
***********************************************************************         
* Process sorted recovery records for first/last                      *         
*---------------------------------------------------------------------*         
*                                                                     *         
* NTRY - R1=A(I/O Area)                                               *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                     *         
* Regular records receiving sequence (ideal):                         *         
*    - Record COPY   SIN=1                                            *         
*    - Record CHANGE SIN=1                                            *         
*    - Record COPY   SIN=2                                            *         
*    - Record CHANGE SIN=2                                            *         
*    - etc.                                                           *         
*                                                                     *         
* O/E records receiving sequence (ideal):                             *         
*    - O/E SEQ=00 COPY   SIN=1                                        *         
*    - O/E SEQ=00 CHANGE SIN=1                                        *         
*    - O/E SEQ=00 COPY   SIN=2                                        *         
*    - O/E SEQ=00 CHANGE SIN=2                                        *         
*    -                   etc. for SIN                                 *         
*    - O/E SEQ=01 COPY   SIN=1                                        *         
*    - O/E SEQ=01 CHANGE SIN=1                                        *         
*    - O/E SEQ=01 COPY   SIN=2                                        *         
*    - O/E SEQ=01 CHANGE SIN=2                                        *         
*    -     etc. for SEQ                                               *         
*    - next logical record                                            *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                     *         
* PROCIT logic:                                                       *         
*    1. If RECIEND go to RESOLVE code                                 *         
*    2. If change in record type go to RESOLVE code and resume with   *         
*       MERGE/PRO40 code                                              *         
*    3. If change in O&E entity go to RESOLVE code and resume with    *         
*       MERGE/PRO40 code                                              *         
*    4. If current is O&E and change in sequence go to RESOLVE code   *         
*       and resume with MERGE/PRO40 code                              *         
*                                                                     *         
* RESOLVE logic:                                                      *         
*    1. If previous record was regular then put it out via ... .      *         
*    2. If previous record was O&E main record then ... .             *         
*    3. If previous record was O&E sequential record then ... .       *         
*                                                                     *         
* MERGE/PRO40 logic:                                                  *         
*    1. If current record is regular then ... .                       *         
*    2. If current record is O&E main record first time then ... .    *         
*    3. If current record is O&E main record encore then ... .        *         
*    4. If current record is O&E sequential record then ... .         *         
*                                                                     *         
***********************************************************************         
                                                                                
         USING RRECD,R3            R3 used for recovery base                    
         USING ACCRECD,RRECORD                                                  
PROCIT   NTR1  ,                                                                
                                                                                
         MVI   PROCIND,0                                                        
         NI    RECIND,FFQ-RECIRES                                               
                                                                                
*** Last time call then resolve any buffered data and exit ***                  
                                                                                
         TM    RECIND,RECIEND                                                   
         JZ    PRO02                                                            
         GOTOR RESOLVE                                                          
         OI    RECIND,RECIRES                                                   
         NI    RECIND,FFQ-RECIADD                                               
         J     PROCITX                                                          
                                                                                
*** Process incomming recovery record ***                                       
                                                                                
PRO02    L     R3,0(R1)            A(Record)                                    
                                                                                
*        CLC   ACCRECD(10),TSTKEY                                               
*        JNE   XXXX                                                             
*        J     XXXX                                                             
*                                                                               
*STKEY   DC    X'1A490000D7F6F5F2F8F90000000000000000000000'                    
*        DC    X'000000000000C8C2D64040F0F0C4F9F0F0F5C10100'                    
*                                                                               
*                                                                               
*XXX     DS    0H                                                               
                                                                                
         GOTO1 VRECTYP,DMCB,(C'D',ACCRECD)                                      
         MVC   RECTYP,0(R1)        Determine current record criteria            
***      MVC   RECCPY,1(R1)                                                     
         MVC   CURMKY,ACCKEY                                                    
         MVC   CURSIN,RSIN                                                      
         LH    RE,RRECLN                                                        
         SHI   RE,1                                                             
         AR    RE,R3                                                            
         LLC   R1,0(RE)                                                         
         SHI   R1,1                                                             
         SR    RE,R1                                                            
         MVC   CURGIN,RGIN-RECVEXTD(RE)                                         
                                                                                
*** Change of record type then resolve previous record ***                      
                                                                                
         CLC   RECTYP,LASTRTY                                                   
         JE    PRO04                                                            
         CLI   LASTRTY,0                                                        
         JE    PRO04                                                            
         GOTOR RESOLVE                                                          
         OI    RECIND,RECIRES                                                   
         NI    RECIND,FFQ-RECIADD                                               
                                                                                
*** If new order resolve previous data first ***                                
                                                                                
PRO04    CLI   RECTYP,ACRTORD      Order?                                       
         JNE   PRO10                                                            
         OI    PROCIND,PROCIORD                                                 
         MVC   CURMKY,ACCKEY                                                    
         MVI   CURMKY+ORDKSEQ-ORDRECD,SEQ_IS_0                                  
         MVC   CURSEQ,ACCKEY+ORDKSEQ-ORDRECD                                    
         CLC   LASTMKY,CURMKY                                                   
         JE    PRO06                                                            
         OC    LASTMKY,LASTMKY                                                  
         JZ    PRO05                                                            
         TM    RECIND,RECIRES                                                   
         JNZ   PRO05                                                            
         GOTOR RESOLVE                                                          
         OI    RECIND,RECIRES                                                   
         NI    RECIND,FFQ-RECIADD                                               
                                                                                
PRO05    CLI   ACCRECD+ORDKSEQ-ORDRECD,SEQ_IS_0                                 
         JNE   *+2                                                              
         OI    PROCIND,PROCIMAI                                                 
         XC    SAVDATA,SAVDATA     Clear save block for this order              
         NI    RECIND,FFQ-RECIADD                                               
         MVI   SAVSEQ,SEQ_IS_0                                                  
         J     PRO30                                                            
                                                                                
*** If Order new sequence number then process previous data first ***           
                                                                                
PRO06    CLC   CURMKY,ACCKEY       Main record?                                 
         JNE   PRO08                                                            
*        OC    LASTKEY,LASTKEY                                                  
*        JZ    PRO07                                                            
*        CLC   LASTKEY,LASTMKY                                                  
*        JNE   PRO08                                                            
                                                                                
PRO07    OI    PROCIND,PROCIMAI    (set current to main record)                 
                                                                                
PRO08    CLC   LASTKEY,ACCKEY                                                   
         JE    PRO30                                                            
         OC    LASTKEY,LASTKEY                                                  
         JZ    PRO30                                                            
         TM    RECIND,RECIRES                                                   
         JNZ   PRO30                                                            
         GOTOR RESOLVE                                                          
         OI    RECIND,RECIRES                                                   
         NI    RECIND,FFQ-RECIADD                                               
         J     PRO30                                                            
                                                                                
*** If new estimate resolve previous data first ***                             
                                                                                
PRO10    CLI   RECTYP,ACRTESTR     Estimate?                                    
         JNE   PRO20                                                            
         OI    PROCIND,PROCIEST                                                 
         MVC   CURMKY,ACCKEY                                                    
         MVI   CURMKY+ESTKSEQ-ESTRECD,ESTKSMQ                                   
         MVC   CURSEQ,ACCKEY+ESTKSEQ-ESTRECD                                    
         CLC   LASTMKY,CURMKY                                                   
         JE    PRO12                                                            
         OC    LASTMKY,LASTMKY                                                  
         JZ    PRO11                                                            
         TM    RECIND,RECIRES                                                   
         JNZ   PRO11                                                            
         GOTOR RESOLVE                                                          
         OI    RECIND,RECIRES                                                   
         NI    RECIND,FFQ-RECIADD                                               
                                                                                
PRO11    CLI   ACCRECD+ESTKSEQ-ESTRECD,ESTKSMQ                                  
         JNE   *+2                 (data integrity check)                       
         OI    PROCIND,PROCIMAI                                                 
         XC    SAVDATA,SAVDATA     Clear save block for this order              
         NI    RECIND,FFQ-RECIADD                                               
         MVI   SAVSEQ,SEQ_IS_0                                                  
         J     PRO30                                                            
                                                                                
*** If Estimate new seq. number then process previous data first ***            
                                                                                
PRO12    CLC   CURMKY,ACCKEY       Main record?                                 
         JNE   PRO14                                                            
*        OC    LASTKEY,LASTKEY                                                  
*        JZ    PRO13                                                            
*        CLC   LASTKEY,LASTMKY                                                  
*        JNE   PRO14                                                            
                                                                                
PRO13    OI    PROCIND,PROCIMAI    (set current to main record) )               
                                                                                
PRO14    CLC   LASTKEY,ACCKEY                                                   
         JE    PRO30                                                            
         OC    LASTKEY,LASTKEY                                                  
         JZ    PRO30                                                            
         TM    RECIND,RECIRES                                                   
         JNZ   PRO30                                                            
         GOTOR RESOLVE                                                          
         OI    RECIND,RECIRES                                                   
         J     PRO30                                                            
                                                                                
*** Change of regular record key then resolve record ***                        
                                                                                
PRO20    CLC   LASTKEY,ACCKEY                                                   
         JE    PRO30                                                            
         OC    LASTKEY,LASTKEY                                                  
         JZ    PRO30                                                            
         TM    RECIND,RECIRES                                                   
         JNZ   PRO30                                                            
         GOTOR RESOLVE                                                          
         OI    RECIND,RECIRES                                                   
         NI    RECIND,FFQ-RECIADD                                               
                                                                                
*** Process current record - get SINTAB for O/E main records ***                
                                                                                
PRO30    CLI   RECTYP,ACRTAUDT     Audit - just put out                         
         JE    PROAUD                                                           
                                                                                
         TM    PROCIND,PROCIORD+PROCIEST                                        
         JZ    PRO40               Get SIN table for any new O&E                
         TM    PROCIND,PROCIMAI                                                 
         JZ    PRO40                                                            
                                                                                
         SAM31 ,                                                                
         USING SINTABD,R2                                                       
         L     R2,ASINTAB          Locate SINTAB entry for main record          
                                                                                
PRO32    CLI   SINTIND,FFQ                                                      
         JE    *+2                                                              
         CLC   SINTIND,RECTYP                                                   
         JNE   PRO34                                                            
         CLC   SINTGIN,CURGIN                                                   
         JNE   PRO34                                                            
         CLC   SINTKEY,CURMKY                                                   
         JE    PRO36                                                            
                                                                                
PRO34    AHI   R2,SINTLNQ                                                       
         J     PRO32                                                            
                                                                                
PRO36    MVC   CURSTE,SINTABD      SINTAB entry for current main record         
         SAM24 ,                                                                
         DROP  R2                                                               
                                                                                
*** Process current record - regular record scenario ***                        
                                                                                
PRO40    TM    PROCIND,PROCIORD+PROCIEST                                        
         JNZ   PRO50                                                            
                                                                                
         CLI   RRECTY,RRECTCPY     Copy goes into 'From' Buffer if              
         JNE   PRO42               first time                                   
         TM    RECIND,RECIBUF+RECIADD                                           
         JNZ   PROCITX                                                          
                                                                                
         MVC   FULL,ABUFFR         Copy record to 'From' Buffer                 
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBUF                                                   
         J     PROCITX                                                          
                                                                                
PRO42    CLI   RRECTY,RRECTCHG     Change goes into 'To' Buffer                 
         JNE   PRO44                                                            
                                                                                
         MVC   FULL,ABUFTO         Copy record to 'To' Buffer                   
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBUT                                                   
         NI    RECIND,FFQ-RECISTA                                               
         JAS   RE,SETADD                                                        
         J     PROCITX                                                          
                                                                                
PRO44    CLI   RRECTY,RRECTADD     Add (else unknown type)                      
         JNE   *+2                                                              
                                                                                
         TM    RECIND,RECIBUF+RECIBUT                                           
         JNZ   *+2                 (on add ???)                                 
                                                                                
         MVC   FULL,ABUFTO         Copy record to 'To' Buffer                   
         JAS   RE,REC2BUF          (but note down 'add')                        
         OI    RECIND,RECIBUT+RECIADD                                           
         NI    RECIND,FFQ-RECISTA                                               
         J     PROCITX                                                          
                                                                                
*** Process current record - O/E main record scenario - general ***             
                                                                                
PRO50    TM    PROCIND,PROCIMAI                                                 
         JZ    PRO70                                                            
         CLI   CURSTE+SINTSOM-SINTABD,SINTSTQ                                   
         JNE   PRO56                                                            
                                                                                
         CLI   RRECTY,RRECTADD                                                  
         JE    *+2                 (code integrity check)                       
                                                                                
         TM    SAVIND,SAVIFSTS     First time for E/O status done?              
         JNZ   PRO52                                                            
         OI    SAVIND,SAVIFSTS     Now                                          
                                                                                
PRO52    OC    SAVFSSIN,SAVFSSIN   Saved Status From SINTAB set?                
         JNZ   PRO54                                                            
         MVC   SAVFSSIN,CURSTE     Set it                                       
                                                                                
PRO54    MVC   SAVTSSIN,CURSTE     Update saved Status To SINTAB                
         J     PRO70                                                            
                                                                                
PRO56    TM    SAVIND,SAVIFSTM     First time for E/O maintenance done?         
         JNZ   PRO58                                                            
         OI    SAVIND,SAVIFSTM     Now                                          
***      CLI   RRECTY,RRECTADD                                                  
***      JNE   PRO58                                                            
***      OI    SAVIND,SAVIADD      First is Add                                 
                                                                                
PRO58    OC    SAVFMSIN,SAVFMSIN   Saved Maintenance From SINTAB set?           
         JNZ   PRO60                                                            
         MVC   SAVFMSIN,CURSTE     Set it                                       
                                                                                
PRO60    MVC   SAVTMSIN,CURSTE     Update saved Maintenance To SINTAB           
                                                                                
*** Process current record - O/E main & seq. scenario - 1st time ***            
                                                                                
PRO70    TM    PROCIND,PROCIMAI                                                 
         JZ    PRO72                                                            
                                                                                
         CLI   CURSTE+SINTSOM-SINTABD,SINTSTQ                                   
         JE    PRO80               If Status go to status code                  
                                                                                
PRO72    TM    PROCIND,PROCIMAI                                                 
         JZ    PRO90                                                            
                                                                                
*** O/E main record maintenance scenario ***                                    
                                                                                
         CLI   RRECTY,RRECTCPY     Copy goes into 'From' Buffer if              
         JNE   PRO74               first time                                   
         TM    RECIND,RECIBUF+RECIADD                                           
         JNZ   PROCITX                                                          
                                                                                
         MVC   FULL,ABUFFR         Copy record to 'From' Buffer                 
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBUF                                                   
         J     PROCITX                                                          
                                                                                
PRO74    CLI   RRECTY,RRECTCHG     Change goes into 'To' Buffer                 
         JNE   PRO76                                                            
                                                                                
         MVC   FULL,ABUFTO         Copy record to 'To' Buffer                   
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBUT                                                   
         NI    RECIND,FFQ-RECISTA                                               
         JAS   RE,SETADD                                                        
         J     PROCITX                                                          
                                                                                
PRO76    CLI   RRECTY,RRECTADD     Add (else unknown type)                      
         JNE   *+2                                                              
                                                                                
         TM    RECIND,RECIBUF+RECIBUT                                           
         JNZ   *+2                 (on add ???)                                 
                                                                                
         MVC   FULL,ABUFTO         Copy record to 'To' Buffer                   
         JAS   RE,REC2BUF          (but note down 'add')                        
         OI    RECIND,RECIBUT+RECIADD                                           
         NI    RECIND,FFQ-RECISTA                                               
         J     PROCITX                                                          
                                                                                
*** O/E main record status scenario ***                                         
                                                                                
PRO80    TM    PROCIND,PROCIMAI                                                 
         JZ    PRO90                                                            
                                                                                
         CLI   RRECTY,RRECTCPY     Copy goes into 'From' Buffer if              
         JNE   PRO82               first time                                   
         TM    RECIND,RECIBSF                                                   
         JNZ   PROCITX                                                          
                                                                                
         MVC   FULL,ABUSFR         Copy record to 'From' Buffer                 
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBSF+RECISTA                                           
         J     PROCITX                                                          
                                                                                
PRO82    CLI   RRECTY,RRECTCHG     Change goes into 'To' Buffer                 
         JNE   PRO84                                                            
                                                                                
         MVC   FULL,ABUSTO         Copy record to 'To' Buffer                   
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBST+RECISTA                                           
         J     PROCITX                                                          
                                                                                
PRO84    CLI   RRECTY,RRECTADD     Add (else unknown type)                      
         JNE   *+2                                                              
         J     *+2                 (Add on Status?)                             
                                                                                
*** Process current record - O/E sequential record scenario ***                 
                                                                                
PRO90    CLI   RRECTY,RRECTCPY     *** process 'copy' entry                     
         JNE   PRO93                                                            
* ???    CLI   SAVSTAT,SAVADDQ     if overall add then skip it                  
* ???    JE    PROCITX                                                          
         OC    SAVFMSIN,SAVFMSIN   required?                                    
         JZ    PROCITX                                                          
         CLC   CURSIN+1(L'CURSIN-1),SAVFMSIN+SINTSIN-SINTABD+1                  
         JNE   PROCITX             if so must match                             
         CLC   CURGIN,SAVFMSIN+SINTGIN-SINTABD                                  
         JNE   PROCITX                                                          
         TM    PROCIND,PROCIEST                                                 
         JNZ   PRO91                                                            
         TM    RECIND,RECIBUF                                                   
         JNZ   *+2                 (code integrity check)                       
         J     PRO92                                                            
                                                                                
PRO91    TM    SAVIND,SAVISFPQ     Already done?                                
         JNZ   PROCITX                                                          
         OI    SAVIND,SAVISFPQ     set to done                                  
                                                                                
PRO92    MVC   FULL,ABUFFR         Copy record to 'From' Buffer                 
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBUF                                                   
         J     PROCITX                                                          
                                                                                
PRO93    CLI   RRECTY,RRECTADD     *** process 'add' entry                      
         JNE   PRO96                                                            
         OC    SAVTMSIN,SAVTMSIN                                                
         JZ    *+2                 (???)                                        
         CLC   CURGIN,SAVTMSIN+SINTGIN-SINTABD                                  
         JNE   PRO95                                                            
         TM    RECIND,RECIBUT                                                   
         JNZ   PRO94                                                            
         MVC   FULL,ABUFTO         Copy record to 'To' Buffer                   
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBUT+RECIADD                                           
         J     PROCITX                                                          
                                                                                
PRO94    TM    RECIND,RECIADD      Add scenario?                                
         JZ    *+2                                                              
         MVC   FULL,ABUFTO                                                      
         JAS   RE,REC2BUF          (RECIBUT/RECIADD already set)                
         JAS   RE,SETADD                                                        
         J     PROCITX                                                          
                                                                                
PRO95    JL    PRO95S              SNIP=Y to skip this check                    
         CLI   SNIP,YESQ                                                        
         JE    PRO95S                                                           
         J     *+2                 (data integrity check)                       
                                                                                
PRO95S   DS    0H                                                               
*delete  TM    RECIND,RECIBUF+RECIBUT+RECIADD                                   
*delete  JNZ   *+2                 (code integrity check)                       
         MVC   FULL,ABUFTO         Copy record to 'To' Buffer                   
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBUT+RECIADD                                           
         J     PROCITX                                                          
                                                                                
PRO96    CLI   RRECTY,RRECTCHG     *** process 'change' entry                   
         JNE   *+2                                                              
         CLI   SAVSTAT,SAVADDQ     if overall add then skip deletes             
         JNE   PRO97                                                            
         TM    ACCRSTA,ACTSDELT                                                 
         JNZ   PROCITX                                                          
                                                                                
PRO97    OC    SAVTMSIN,SAVTMSIN                                                
         JZ    *+2                 (data integrity check)                       
         CLC   CURGIN,SAVTMSIN+SINTGIN-SINTABD                                  
         JNE   PRO98                                                            
         MVC   FULL,ABUFTO         Copy record to 'To' Buffer                   
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBUT                                                   
         NI    RECIND,FFQ-RECISTA                                               
         JAS   RE,SETADD                                                        
         J     PROCITX                                                          
                                                                                
PRO98    JH    PROCITX             Keep 'in between' deletions                  
         TM    ACCRSTA,ACTSDELT                                                 
         JZ    PROCITX                                                          
         MVC   FULL,ABUFTO         Copy record to 'To' Buffer                   
         JAS   RE,REC2BUF                                                       
         OI    RECIND,RECIBUT                                                   
         NI    RECIND,FFQ-RECISTA                                               
         JAS   RE,SETADD                                                        
                                                                                
*** General Process exit (set data for next time call) ***                      
                                                                                
PROCITX  MVC   LASTKEY,ACCKEY                                                   
         MVC   LASTMKY,CURMKY                                                   
         MVC   LASTRTY,RECTYP                                                   
         MVC   SAVSEQ,CURSEQ       Save O/E sequence                            
         J     EXIT                                                             
                                                                                
*** Audit record all go out - adjust O&E related ones for SIN etc.              
                                                                                
PROAUD   XC    LASTKEY,LASTKEY                                                  
         XC    LASTMKY,LASTMKY                                                  
         XC    LASTRTY,LASTRTY                                                  
         XC    SAVSEQ,SAVSEQ                                                    
                                                                                
         GOTOR SETAUD,RRECD        Set audit properties                         
         JNE   PROAUDX                                                          
                                                                                
         LA    R1,RRECD                                                         
         ST    R1,FULL                                                          
         GOTOR PUTREC,FULL         Put record out                               
                                                                                
PROAUDX  DS    0H                                                               
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Resolve buffered data                                               *         
***********************************************************************         
                                                                                
RESOLVE  NTR1                                                                   
                                                                                
         CLI   LASTRTY,ACRTESTR                                                 
         JE    RES02                                                            
         CLI   LASTRTY,ACRTORD                                                  
         JNE   RES20                                                            
                                                                                
*** O/E - main record ***                                                       
                                                                                
RES02    GOTOR SETOUT              Sets OUTIND and OUTIND2                      
                                                                                
         CLI   SAVSEQ,SEQ_IS_0                                                  
         JNE   RES10                                                            
                                                                                
         TM    OUTIND,OUTIT_A+OUTIT_D                                           
         JO    RES04               If 'add' -> 'deleted' skip both              
         TM    OUTIND,OUTIF_D+OUTIT_D                                           
         JO    RES04               If 'deleted' -> 'deleted' skip both          
         TM    OUTIND,OUTIF_R+OUTIT_R                                           
         JNO   *+8                 If 'restored'->'restored' skip both          
RES04    NI    RECIND,FFQ-(RECIBUF+RECIBUT)                                     
*                                                                               
         GOTOR OEPROC              O/E Maint vs. Status checks                  
                                                                                
         CLI   SAVSTAT,SAVSKIPQ    Skip or carry on?                            
         JE    RES06                                                            
                                                                                
         GOTOR SETREC,STATQ                                                     
                                                                                
         GOTOR SETREC,MAINQ                                                     
         J     RES30                                                            
                                                                                
RES06    NI    RECIND,FFQ-(RECIBUF+RECIBUT+RECIADD)                             
         J     RESOLVEX            Ensure Buffers are freed                     
                                                                                
*** O/E - sequential records ***                                                
                                                                                
RES10    CLI   LASTRTY,ACRTORD     Order Extension special case                 
         JNE   RES12                                                            
RES12    CLI   SAVSTAT,SAVSKIPQ    Skip or carry on?                            
         JE    RES16                                                            
         TM    OUTIND,OUTIT_A+OUTIT_D                                           
         JO    RES16               If 'add' -> 'deleted' skip both              
         TM    OUTIND,OUTIF_D+OUTIT_D                                           
         JO    RES16               If 'deleted' -> 'deleted' skip both          
         TM    OUTIND,OUTIF_R+OUTIT_R                                           
         JO    RES16               If 'restored'->'restored' skip both          
                                                                                
         CLI   LASTRTY,ACRTESTR    DSRD-21683: check sequential record          
         JE    RES13               in ABUFTO is of same SIN/TIME as             
         CLI   LASTRTY,ACRTORD     main record, if not equal skip               
         JNE   RES14               (also DSRD-21784)                            
                                                                                
RES13    TM    RECIND,RECIBUT                                                   
         JZ    RES14                                                            
         TM    OUTIND,OUTIT_D                                                   
         JO    RES14               If -> 'deleted' skip test                    
         L     RE,ABUFTO                                                        
         CLC   SAVTMDET(L'RTIME),RTIME-RRECD(RE)                                
         JE    RES14                                                            
         CLC   SAVTMSIN+SINTSIN-SINTABD+1(L'SINTSIN-1),RSIN+1-RRECD(RE)         
         JNE   RES16                                                            
                                                                                
RES14    GOTOR SETREC,SEQUQ                                                     
                                                                                
         J     RES30                                                            
                                                                                
RES16    NI    RECIND,FFQ-(RECIBUF+RECIBUT+RECIADD)                             
         J     RESOLVEX            Ensure Buffers are freed                     
                                                                                
*** Regular records ***                                                         
                                                                                
RES20    GOTOR SETOUT              Catch special scenarios                      
                                                                                
         TM    OUTIND,OUTIF_D+OUTIT_D                                           
         JO    RES22               If 'deleted' -> 'deleted' skip both          
         TM    OUTIND,OUTIT_A+OUTIT_D                                           
         JO    RES22               If 'add' -> 'deleted' skip both              
*        TM    OUTIND,OUTIF_A+OUTIT_X                                           
*        JNO   RES30               If 'add' -> 'change' make it add             
*        SAM31 ,                                                                
*        L     R0,ABUFFR           Copy 'to' into 'from' buffer, set            
*        LA    R1,BUSFLNQ          to 'Add' and clear 'to' buffer               
*        L     RE,ABUFTO           status                                       
*        LR    RF,R1                                                            
*        MVCL  R0,RE                                                            
*        L     RE,ABUFFR                                                        
*        MVI   RRECTY-RRECD(RE),RRECTADD                                        
*        NI    RECIND,FFQ-RECIBUT                                               
*        OI    RECIND,RECIBUF                                                   
*        SAM24 ,                                                                
                                                                                
         GOTOR SETREC,REGUQ                                                     
         J     RES30                                                            
                                                                                
RES22    NI    RECIND,FFQ-(RECIBUF+RECIBUT+RECIADD)                             
         J     RESOLVEX            Ensure Buffers are freed                     
                                                                                
*** All record output ***                                                       
                                                                                
RES30    TM    RECIND,RECIBUF      Anything in 'From' Buffer                    
         JZ    RES32                                                            
         MVC   FULL,ABUFFR                                                      
         JAS   RE,MOVAPUT          Move and put record out                      
         GOTOR ESTLOG,FROMQ        Estimate special logic                       
         IPM   RE                                                               
         NI    RECIND,FFQ-RECIBUF                                               
         SPM   RE                                                               
         JNE   RESOLVEX                                                         
                                                                                
RES32    TM    RECIND,RECIBUT      Anything in 'To' Buffer                      
         JZ    RES34                                                            
         GOTOR ESTLOG,TOQ          Estimate special logic                       
         JE    RES33                                                            
         GOTOR ADDAUD,RECIBUT      Audit handling                               
         NI    RECIND,FFQ-RECIBUT                                               
         J     RESOLVEX                                                         
                                                                                
RES33    MVC   FULL,ABUFTO                                                      
         JAS   RE,MOVAPUT          Move and put record out                      
         GOTOR ADDAUD,RECIBUT      Audit handling                               
         NI    RECIND,FFQ-RECIBUT                                               
                                                                                
RES34    TM    RECIND,RECIBSF      Anything in 'From' Buffer                    
         JZ    RES36                                                            
         MVC   FULL,ABUSFR                                                      
         JAS   RE,MOVAPUT          Move and put record out                      
         NI    RECIND,FFQ-RECIBSF                                               
                                                                                
RES36    TM    RECIND,RECIBST      Anything in 'To' Buffer                      
         JZ    RES38                                                            
         MVC   FULL,ABUSTO                                                      
         JAS   RE,MOVAPUT          Move and put record out                      
         GOTOR ADDAUD,RECIBST      Audit handling                               
         NI    RECIND,FFQ-RECIBST                                               
                                                                                
RES38    DS    0H                                                               
                                                                                
RESOLVEX NI    SAVIND,FFQ-SAVISFPQ                                              
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Set OUTIND and OUTIND2 to detect special scenarios                  *         
***********************************************************************         
                                                                                
SETOUT   ST    RE,SAVERE                                                        
                                                                                
         SAM31 ,                                                                
         MVI   OUTIND,0                                                         
         MVI   OUTIND2,0                                                        
         OI    OUTIND2,OUTIF_E                                                  
         TM    RECIND,RECIBUF                                                   
         JZ    SETO04                                                           
         OI    OUTIND,OUTIF_X                                                   
         NI    OUTIND2,FFQ-OUTIF_E                                              
         L     RE,ABUFFR                                                        
         CLI   RRECTY-RRECD(RE),RRECTADD                                        
         JNE   SETO02                                                           
         OI    OUTIND,OUTIF_A                                                   
                                                                                
SETO02   TM    RRECORD-RRECD+ACCRSTA-ACCRECD(RE),ACTSDELT                       
         JZ    SETO03                                                           
         OI    OUTIND,OUTIF_D      Set deleted in =Prod                         
         J     SETO04                                                           
                                                                                
SETO03   CLI   RPRG-RRECD(RE),RCVPPROQ                                          
         JNE   SETO04                                                           
         OI    OUTIND,OUTIF_R      Set restored in =Prod                        
                                                                                
SETO04   OI    OUTIND2,OUTIT_E                                                  
         TM    RECIND,RECIBUT                                                   
         JZ    SETO08                                                           
         OI    OUTIND,OUTIT_X                                                   
         NI    OUTIND2,FFQ-OUTIT_E                                              
         L     RE,ABUFTO                                                        
         CLI   RRECTY-RRECD(RE),RRECTADD                                        
         JNE   SETO06                                                           
         OI    OUTIND,OUTIT_A                                                   
                                                                                
SETO06   TM    RRECORD-RRECD+ACCRSTA-ACCRECD(RE),ACTSDELT                       
         JZ    SETO07                                                           
         OI    OUTIND,OUTIT_D      Set deleted in =Prod                         
         J     SETO08                                                           
                                                                                
SETO07   CLI   RPRG-RRECD(RE),RCVPPROQ                                          
         JNE   SETO08                                                           
         OI    OUTIND,OUTIT_R      Set restored in =Prod                        
                                                                                
SETO08   DS    0H                                                               
         SAM24 ,                                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* Add Order/Estimate to OERTAB                                        *         
***********************************************************************         
                                                                                
ADDAUD   NTR1                                                                   
         SAM31 ,                                                                
                                                                                
         STC   R1,BYTE                                                          
                                                                                
         USING AUDRECD,R4                                                       
         LA    R4,ELEM                                                          
         USING RRECD,R3                                                         
         L     R3,FULL                                                          
                                                                                
         CLI   SAVSEQ,SEQ_IS_0     Main O/E Records from 'To' buffers           
         JNE   ADDAUDX                                                          
         CLI   LASTRTY,ACRTESTR                                                 
         JE    ADDA10                                                           
         CLI   LASTRTY,ACRTORD                                                  
         JNE   ADDAUDX                                                          
                                                                                
         XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,RRECORD+ORDKCPY-ORDRECD                                  
         MVI   AUDKAUDT,AUDKORD                                                 
         MVC   AUDKORDN,RRECORD+ORDKORD-ORDRECD                                 
         J     ADDA20                                                           
                                                                                
ADDA10   LA    R5,AGYLIST                                                       
                                                                                
ADDA12   CLI   0(R5),0             (???)                                        
         JE    *+2                                                              
         CLC   0(1,R5),RRECORD+ESTKCPY-ESTRECD                                  
         JE    ADDA14                                                           
         AHI   R5,AGYLNQ                                                        
         J     ADDA12                                                           
                                                                                
ADDA14   XC    AUDKEY,AUDKEY                                                    
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVC   AUDKCPY,RRECORD+ESTKCPY-ESTRECD                                  
         MVI   AUDKAUDT,AUDKEST                                                 
         MVC   AUDKELNO,RRECORD+ESTKLNO-ESTRECD                                 
                                                                                
         MVC   TEMP(16),SPACES                                                  
         LA    R1,TEMP                                                          
         LLC   RE,1(R5)                                                         
         SHI   RE,1                                                             
         MVC   0(0,R1),RRECORD+ESTKCLI-ESTRECD                                  
         EXRL  RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R1,RE                                                            
         LLC   RE,2(R5)                                                         
         SHI   RE,1                                                             
         MVC   0(0,R1),RRECORD+ESTKPRO-ESTRECD                                  
         EXRL  RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R1,RE                                                            
         MVC   0(L'ESTKJOB,R1),RRECORD+ESTKJOB-ESTRECD                          
                                                                                
         MVC   AUDKECPJ,TEMP       see AGXROUTS.AGXEST00                        
         DROP  R4                                                               
                                                                                
         USING OERTABD,R4                                                       
ADDA20   L     R4,AOERTAB                                                       
         LHI   R5,OERTMAX                                                       
                                                                                
ADDA22   CLI   OERTKEY,FFQ                                                      
         JE    ADDA24                                                           
         AHI   R4,OERTLNQ                                                       
         JCT   R5,ADDA22                                                        
         DC    H'0'                Increase OERTMAX                             
                                                                                
ADDA24   LA    RE,SAVTMSIN                                                      
         LA    RF,SAVTMDET                                                      
         CLI   BYTE,RECIBUT                                                     
         JE    ADDA26                                                           
         LA    RE,SAVTSSIN                                                      
         LA    RF,SAVTSDET                                                      
         CLI   BYTE,RECIBST                                                     
         JNE   *+2                                                              
                                                                                
ADDA26   MVC   OERTKEY,ELEM                                                     
         MVC   OERTSIN,SINTSIN-SINTABD(RE)                                      
         MVC   OERTGIN,SINTGIN-SINTABD(RE)                                      
         MVC   OERTTIM,0(RF)                                                    
         MVC   OERTDAT,L'RTIME+L'RPRG(RF)                                       
         MVI   OERTADD,NOQ                                                      
         CLI   RRECTY,RRECTADD                                                  
         JNE   ADDA28                                                           
         MVI   OERTADD,YESQ                                                     
                                                                                
ADDA28   AHI   R4,OERTLNQ                                                       
         MVI   OERTKEY,FFQ                                                      
         DROP  R3                                                               
                                                                                
ADDAUDX  SAM24 ,                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Order/Estimate Status vs. Maintenance checks processing             *         
***********************************************************************         
                                                                                
OEPROC   ST    RE,SAVERE                                                        
                                                                                
         SAM31 ,                                                                
                                                                                
* OUTIND set from RESOLVE for regular buffer *                                  
                                                                                
         TM    RECIND,RECIBSF+RECIBST                                           
         JZ    OEPRO10             No Status data so skip                       
         JNO   *+2                 (impossible for status)                      
                                                                                
* OUTIND scenarios not applicable for status                                    
                                                                                
         OI    SAVIND,SAVISTAQ                                                  
                                                                                
         TM    RECIND,RECIBUF+RECIBUT                                           
         JNZ   OEPRO30             Status only change so take it                
                                                                                
         TM    RECIND,RECIBSF      Status only data                             
         JZ    OEPRO02                                                          
         L     RE,ABUSFR                                                        
         MVC   SAVFSDET(L'RTIME),RTIME-RRECD(RE)                                
         MVC   SAVFSDET+L'RTIME(L'RPRG),RPRG-RRECD(RE)                          
         MVC   SAVFSDET+L'RTIME+L'RPRG(L'RDATE),RDATE-RRECD(RE)                 
                                                                                
OEPRO02  TM    RECIND,RECIBST                                                   
         JZ    OEPRO04                                                          
         L     RE,ABUSTO                                                        
         MVC   SAVTSDET(L'RTIME),RTIME-RRECD(RE)                                
         MVC   SAVTSDET+L'RTIME(L'RPRG),RPRG-RRECD(RE)                          
         MVC   SAVTSDET+L'RTIME+L'RPRG(L'RDATE),RDATE-RRECD(RE)                 
                                                                                
OEPRO04  DS    0H                                                               
         J     OEPROCX                                                          
                                                                                
OEPRO10  DS    0H                  Maintenance only data                        
                                                                                
* OUTIND set - see above *                                                      
                                                                                
         OI    SAVIND,SAVIMAIQ                                                  
                                                                                
         TM    OUTIND,OUTIF_D+OUTIT_D                                           
         JNO   OEPRO12                                                          
         MVI   SAVSTAT,SAVSKIPQ    If 'deleted' -> 'deleted' then skip          
         J     OEPROCX                                                          
                                                                                
OEPRO12  TM    OUTIND,OUTIF_R+OUTIT_R                                           
         JNO   OEPRO14                                                          
         MVI   SAVSTAT,SAVSKIPQ    If 'restored'->'restored' then skip          
         J     OEPROCX                                                          
                                                                                
OEPRO14  TM    OUTIND,OUTIT_A+OUTIT_D                                           
         JNO   OEPRO16                                                          
         MVI   SAVSTAT,SAVSKIPQ    If 'add' -> 'deleted' then skip              
         J     OEPROCX                                                          
                                                                                
OEPRO16  TM    OUTIND,OUTIF_A+OUTIT_A                                           
         JZ    OEPRO18                                                          
         MVI   SAVSTAT,SAVADDQ     If 'add' set flag                            
                                                                                
OEPRO18  TM    RECIND,RECIBUF      Maintenance data                             
         JZ    OEPRO20                                                          
         L     RE,ABUFFR                                                        
         MVC   SAVFMDET(L'RTIME),RTIME-RRECD(RE)                                
         MVC   SAVFMDET+L'RTIME(L'RPRG),RPRG-RRECD(RE)                          
         MVC   SAVFMDET+L'RTIME+L'RPRG(L'RDATE),RDATE-RRECD(RE)                 
                                                                                
OEPRO20  TM    RECIND,RECIBUT                                                   
         JZ    OEPROCX                                                          
         L     RE,ABUFTO                                                        
         MVC   SAVTMDET(L'RTIME),RTIME-RRECD(RE)                                
         MVC   SAVTMDET+L'RTIME(L'RPRG),RPRG-RRECD(RE)                          
         MVC   SAVTMDET+L'RTIME+L'RPRG(L'RDATE),RDATE-RRECD(RE)                 
         J     OEPROCX                                                          
                                                                                
OEPRO30  XC    DUB,DUB             Get sequence of events status vs.            
         XC    DUB2,DUB2           maintenance                                  
                                                                                
         OI    SAVIND,SAVIMAIQ                                                  
                                                                                
         L     RE,ABUSFR                                                        
         MVC   DUB+00(L'RVCHR),RVCHR-RRECD(RE)                                  
         MVC   SAVFSDET(L'RTIME),RTIME-RRECD(RE)                                
         MVC   SAVFSDET+L'RTIME(L'RPRG),RPRG-RRECD(RE)                          
         MVC   SAVFSDET+L'RTIME+L'RPRG(L'RDATE),RDATE-RRECD(RE)                 
                                                                                
         L     RE,ABUSTO                                                        
         MVC   DUB+04(L'RVCHR),RVCHR-RRECD(RE)                                  
         MVC   SAVTSDET(L'RTIME),RTIME-RRECD(RE)                                
         MVC   SAVTSDET+L'RTIME(L'RPRG),RPRG-RRECD(RE)                          
         MVC   SAVTSDET+L'RTIME+L'RPRG(L'RDATE),RDATE-RRECD(RE)                 
                                                                                
         TM    RECIND,RECIBUF                                                   
         JZ    OEPRO32                                                          
         L     RE,ABUFFR                                                        
         MVC   DUB2+00(L'RVCHR),RVCHR-RRECD(RE)                                 
         MVC   SAVFMDET(L'RTIME),RTIME-RRECD(RE)                                
         MVC   SAVFMDET+L'RTIME(L'RPRG),RPRG-RRECD(RE)                          
         MVC   SAVFMDET+L'RTIME+L'RPRG(L'RDATE),RDATE-RRECD(RE)                 
                                                                                
OEPRO32  TM    RECIND,RECIBUT                                                   
         JZ    OEPRO34                                                          
         L     RE,ABUFTO                                                        
         MVC   DUB2+04(L'RVCHR),RVCHR-RRECD(RE)                                 
         MVC   SAVTMDET(L'RTIME),RTIME-RRECD(RE)                                
         MVC   SAVTMDET+L'RTIME(L'RPRG),RPRG-RRECD(RE)                          
         MVC   SAVTMDET+L'RTIME+L'RPRG(L'RDATE),RDATE-RRECD(RE)                 
                                                                                
OEPRO34  CLC   DUB2+04(L'RVCHR),DUB+04                                          
         JNH   OEPRO36             Seq(Maint/To) > Seq(Status/To)               
         NI    RECIND,FFQ-(RECIBSF+RECIBST)                                     
         NI    SAVIND,FFQ-SAVISTAQ                                              
         XC    SAVFSSIN,SAVFSSIN   Skip Status completely                       
         XC    SAVTSSIN,SAVTSSIN                                                
         XC    SAVFMDET,SAVFMDET                                                
         XC    SAVFSDET,SAVFSDET                                                
         J     OEPROCX                                                          
                                                                                
OEPRO36  MVC   SAVFSSIN,SAVTMSIN   Copy 'To'/Maint into 'From'/Status           
         MVC   SAVFSDET,SAVTMDET                                                
         L     RE,ABUFTO                                                        
         L     R0,ABUSFR                                                        
         LA    R1,BUSFLNQ                                                       
         L     RE,ABUFTO                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         JAS   RE,SETCOP                                                        
                                                                                
         CLC   SAVTSDET(L'RTIME),SAVTMDET   DSRD-20686: check RTIME is          
         JH    OEPROCX             at least a second out, if not set so         
***      JL    *+2                 (??? leave it for now)                       
                                                                                
         L     RE,ABUSTO           Incr. RTIME by a 1s in ABUSTO etc.           
         XC    DUBT,DUBT                                                        
         MVC   DUBT+4(L'RTIME),RTIME-RRECD(RE)                                  
         MVC   TEMP+18(L'RDATE),RDATE-RRECD(RE)                                 
         MVC   BYTE,DUBT+4                                                      
         NI    BYTE,X'F0'                                                       
         NI    DUBT+4,X'0F'        remove high order bits                       
         MP    DUBT,PTEN           align H/M/Ss                                 
         ZAP   DUB3,PTEN           add a second                                 
         AP    DUBT,DUB3                                                        
         CLI   DUBT+6,X'60'        seconds overflow                             
         JL    OEPRO38                                                          
         MVI   DUBT+6,X'00'        add to minutes                               
         MP    DUB3,PHNDRD                                                      
         AP    DUBT,DUB3                                                        
         CLI   DUBT+5,X'60'        minutes overflow                             
         JL    OEPRO38                                                          
         MVI   DUBT+5,X'00'        add to hours                                 
         MP    DUB3,PHNDRD                                                      
         AP    DUBT,DUB3                                                        
         CLI   DUBT+4,X'60'        hours overflow                               
         JL    OEPRO38                                                          
         MVI   DUBT+4,X'00'        add to days                                  
         MVC   TEMP+0(L'RDATE),RDATE-RRECD(RE)                                  
         SAM24 ,                                                                
         GOTO1 VDATCON,DMCB,(3,TEMP+0),(0,TEMP+6)                               
         GOTO1 VADDAY,DMCB,(C'D',TEMP+6),TEMP+12,1                              
         GOTO1 VDATCON,DMCB,(0,TEMP+12),(3,TEMP+18)                             
         SAM31 ,                                                                
                                                                                
OEPRO38  SRP   DUBT,64-1,0         shift back from MP above                     
         OC    DUBT+4(1),BYTE      reinstate high order bits                    
         L     RE,ABUSTO                                                        
         MVC   RTIME-RRECD(L'RTIME,RE),DUBT+4                                   
         MVC   RDATE-RRECD(L'RDATE,RE),TEMP+18                                  
         MVC   SAVTSDET(L'RTIME),RTIME-RRECD(RE)                                
         MVC   SAVTSDET+L'RTIME+L'RPRG(L'RDATE),RDATE-RRECD(RE)                 
                                                                                
OEPROCX  SAM24 ,                                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* Copy record at R3 to buffer at FULL                                 *         
***********************************************************************         
                                                                                
REC2BUF  ST    RE,SAVERE                                                        
         SAM31 ,                                                                
         L     R0,FULL                                                          
         LA    R1,BUFFLNQ                                                       
         LR    RE,R3                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         SAM24 ,                                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* Special estimate logic:                                             *         
*   Mimic COPY/CHANGE(Delete) -> COPY(delete)/CHANGE                  *         
***********************************************************************         
                                                                                
ESTLOG   ST    RE,SAVERE                                                        
                                                                                
FROMQ    EQU   C'F'                                                             
TOQ      EQU   C'T'                                                             
                                                                                
         CLI   LASTRTY,ACRTESTR                                                 
         JNE   ESTLOGY                                                          
                                                                                
         CHI   R1,FROMQ                                                         
         JNE   ESTLOG6                                                          
         USING RRECD,R4                                                         
         L     R4,AIO3                                                          
                                                                                
         CLI   RRECTY,RRECTCPY                                                  
         JNE   ESTLOGY                                                          
         MVI   RRECTY,RRECTCHG                                                  
                                                                                
         CLI   RPRG,RCVPPROQ       From =Prod                                   
         JNE   ESTLOG2             No - ok                                      
         TM    RRECORD+ACCRSTA-ACCRECD,ACTSDELT                                 
         JZ    ESTLOG2                                                          
         NI    RRECORD+ACCRSTA-ACCRECD,FFQ-ACTSDELT                             
         J     ESTLOG4             Make the COPY.Delete a CHANGE                
                                                                                
ESTLOG2  DS    0H                  Make the COPY a CHANGE.Delete                
         OI    RRECORD+ACCRSTA-ACCRECD,ACTSDELT                                 
ESTLOG4  ICM   R1,B'1111',RVCHR                                                 
         AHI   R1,1                                                             
         STCM  R1,B'1111',RVCHR                                                 
         GOTOR PUTREC,AIO3         Put record out                               
         J     ESTLOGY                                                          
         DROP  R4                                                               
                                                                                
ESTLOG6  CHI   R1,TOQ                                                           
         JNE   *+2                                                              
                                                                                
         SAM31 ,                                                                
         USING RRECD,R4                                                         
         L     R4,ABUFTO                                                        
                                                                                
         TM    RAG,SEQ_OFST        If we have so many record ... need           
         JNZ   *+2                 a better way/other bits here                 
         OI    RAG,SEQ_OFST        Set high so estimates COP/CHA.Del            
         DS    0H                  are ALL before linked COP.Del/CHA            
                                                                                
         CLI   RRECTY,RRECTCHG                                                  
         JE    ESTLOG8                                                          
         SAM24 ,                                                                
         J     ESTLOGY                                                          
                                                                                
ESTLOG8  TM    RRECORD+ACCRSTA-ACCRECD,ACTSDELT                                 
         JNZ   ESTLOGN             Skip Deleted to Deleted                      
                                                                                
         L     R0,AIO3                                                          
         LA    R1,BUFFLNQ                                                       
         L     RE,ABUFTO                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         SAM24 ,                                                                
         L     R4,AIO3             Make the CHANGE a COPY.Delete                
         MVI   RRECTY,RRECTCPY                                                  
         OI    RRECORD+ACCRSTA-ACCRECD,ACTSDELT                                 
         ICM   R1,B'1111',RVCHR                                                 
         SHI   R1,1                                                             
         STCM  R1,B'1111',RVCHR                                                 
         GOTOR PUTREC,AIO3                                                      
         DROP  R4                                                               
                                                                                
ESTLOGY  LHI   R1,1                                                             
         J     ESTLOGX                                                          
ESTLOGN  LHI   R1,0                                                             
ESTLOGX  CHI   R1,1                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* Move and Put record                                                 *         
***********************************************************************         
                                                                                
MOVAPUT  ST    RE,SAVERE                                                        
         SAM31 ,                                                                
         L     R0,AIO3             Copy for putting record out via AIO3         
         LA    R1,BUFFLNQ                                                       
         L     RE,FULL                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         SAM24 ,                                                                
         GOTOR PUTREC,AIO3         Put record out                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
***********************************************************************         
* Set Recovery overrides                                              *         
***********************************************************************         
                                                                                
SETREC   NTR1                                                                   
                                                                                
         STC   R1,BYTE2            Save call type                               
         SAM31 ,                                                                
                                                                                
REGUQ    EQU   C'1'                                                             
SEQUQ    EQU   C'2'                                                             
STATQ    EQU   C'3'                                                             
MAINQ    EQU   C'4'                                                             
                                                                                
         CLI   BYTE2,REGUQ         Regular record                               
         JE    SETRRE                                                           
         CLI   BYTE2,SEQUQ         O/E sequential record                        
         JE    SETRSQ                                                           
         CLI   BYTE2,STATQ         O/E status record                            
         JE    SETRST                                                           
         CLI   BYTE2,MAINQ         O/E main record                              
         JNE   *+2                 (called from where?)                         
                                                                                
         J     SETRRE              (same as regular)                            
                                                                                
SETRST   TM    RECIND,RECIBSF+RECIBST                                           
         JNO   SETRECX             Quit if not both buffers used                
                                                                                
         L     R4,ABUSFR                                                        
         L     R5,ABUSTO                                                        
         J     SETRDO                                                           
                                                                                
SETRSQ   TM    RECIND,RECIBUF                                                   
         JZ    SETRSQ2                                                          
                                                                                
FROM     USING RRECD,R4                                                         
         L     R4,ABUFFR                                                        
                                                                                
         MVC   FROM.RSIN,SAVTMSIN+SINTSIN-SINTABD                               
         MVC   FROM.RTIME,SAVTMDET                                              
         OC    FROM.RTIME,FROM.RTIME                                            
         JZ    *+2                                                              
         MVC   FROM.RPRG,SAVTMDET+L'RTIME                                       
         MVC   FROM.RDATE,SAVTMDET+L'RTIME+L'RPRG                               
                                                                                
         TM    FROM.RTIME,X'40'    ANY EXT REC?                                 
         JZ    SETRSQ2             NO-SKIP OVER                                 
         LR    RE,R4                                                            
         LH    R1,FROM.RRECLN                                                   
         SHI   R1,1                                                             
         AR    RE,R1                                                            
         LLC   R1,0(RE)                                                         
         SHI   R1,1                                                             
         SR    RE,R1                                                            
         USING RECVEXTD,RE                                                      
         MVC   RGIN,SAVTMSIN+SINTGIN-SINTABD                                    
         DROP  RE,FROM                                                          
                                                                                
SETRSQ2  TM    RECIND,RECIBUT                                                   
         JZ    SETRECX                                                          
                                                                                
TO       USING RRECD,R4                                                         
         L     R4,ABUFTO                                                        
                                                                                
         MVC   TO.RSIN,SAVTMSIN+SINTSIN-SINTABD                                 
         MVC   TO.RTIME,SAVTMDET                                                
         OC    TO.RTIME,TO.RTIME                                                
         JZ    *+2                                                              
         MVC   TO.RPRG,SAVTMDET+L'RTIME                                         
         MVC   TO.RDATE,SAVTMDET+L'RTIME+L'RPRG                                 
                                                                                
         TM    TO.RTIME,X'40'      ANY EXT REC?                                 
         JZ    SETRECX             NO-SKIP OVER                                 
         LR    RE,R4                                                            
         LH    R1,TO.RRECLN                                                     
         SHI   R1,1                                                             
         AR    RE,R1                                                            
         LLC   R1,0(RE)                                                         
         SHI   R1,1                                                             
         SR    RE,R1                                                            
         USING RECVEXTD,RE                                                      
         MVC   RGIN,SAVTMSIN+SINTGIN-SINTABD                                    
         J     SETRECX                                                          
         DROP  RE,TO                                                            
                                                                                
SETRRE   TM    RECIND,RECIBUF+RECIBUT                                           
         JNO   SETRECX             Quit if not both buffers used                
                                                                                
         L     R4,ABUFFR                                                        
         L     R5,ABUFTO                                                        
         J     SETRDO                                                           
                                                                                
FROM     USING RRECD,R4                                                         
TO       USING RRECD,R5                                                         
SETRDO   MVC   FROM.RSIN,TO.RSIN                                                
         MVC   FROM.RTIME,TO.RTIME                                              
         MVC   FROM.RPRG,TO.RPRG                                                
         MVC   FROM.RDATE,TO.RDATE                                              
                                                                                
         XC    DUB,DUB                                                          
                                                                                
         TM    TO.RTIME,X'40'      ANY EXT REC?                                 
         JZ    SETR6               NO-SKIP OVER                                 
         LR    RE,R4                                                            
         LH    R1,TO.RRECLN                                                     
         SHI   R1,1                                                             
         AR    RE,R1                                                            
         LLC   R1,0(RE)                                                         
         SHI   R1,1                                                             
         SR    RE,R1                                                            
         USING RECVEXTD,RE                                                      
         MVC   DUB,RGIN                                                         
         DROP  RE                                                               
                                                                                
SETR6    TM    FROM.RTIME,X'40'    ANY EXT REC?                                 
         JZ    SETR8               NO-SKIP OVER                                 
         LR    RE,R4                                                            
         LH    R1,FROM.RRECLN                                                   
         BCTR  R1,0                                                             
         AR    RE,R1                                                            
         LLC   R1,0(RE)                                                         
         SHI   R1,1                                                             
         SR    RE,R1                                                            
         USING RECVEXTD,RE                                                      
         MVC   RGIN,DUB                                                         
         OI    BYTE,X'08'                                                       
         DROP  RE                                                               
                                                                                
SETR8    CLI   BYTE2,REGUQ         Regular record                               
         JNE   SETRECX                                                          
         ICM   R1,B'1111',TO.RVCHR                                              
         SHI   R1,1                                                             
         STCM  R1,B'1111',FROM.RVCHR                                            
         DROP  FROM,TO                                                          
                                                                                
SETRECX  SAM24 ,                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Set Audit record recovery properties                                *         
***********************************************************************         
                                                                                
         USING RRECD,R3                                                         
SETAUD   NTR1                                                                   
                                                                                
         SAM31 ,                                                                
         LR    R3,R1                                                            
                                                                                
         CLI   RRECORD+AUDKAUDT-AUDRECD,AUDKORD                                 
         JE    SAUD1                                                            
         CLI   RRECORD+AUDKAUDT-AUDRECD,AUDKEST                                 
         JNE   SETAUDN                                                          
                                                                                
         USING OERTABD,R4                                                       
SAUD1    L     R4,AOERTAB                                                       
         LH    RE,RRECLN                                                        
         SHI   RE,1                                                             
         AR    RE,R3                                                            
         LLC   R1,0(RE)                                                         
         SHI   R1,1                                                             
         SR    RE,R1                                                            
                                                                                
SAUD2    CLI   OERTKEY,0                                                        
         JE    SETAUDN                                                          
         CLC   OERTKEY(AUDKSEQ-AUDRECD),RRECORD                                 
         JNE   SAUD3                                                            
         CLC   OERTSIN,RSIN                                                     
         JNE   SAUD3                                                            
         CLC   OERTGIN,RGIN-RECVEXTD(RE)                                        
         JE    SAUD4                                                            
                                                                                
SAUD3    AHI   R4,OERTLNQ                                                       
         J     SAUD2                                                            
                                                                                
SAUD4    CLI   OERTADD,YESQ        ensure it is 'ADD' scenario                  
         JNE   SAUD5                                                            
         CLI   RRECTY,RRECTCPY                                                  
         JE    SETAUDN                                                          
         CLI   RRECTY,RRECTCHG                                                  
         JNE   SAUD5                                                            
         MVI   RRECTY,RRECTADD                                                  
                                                                                
SAUD5    DS    0H                                                               
*        MVC   RSIN,OERTSIN                                                     
         MVC   RTIME,OERTTIM                                                    
         MVC   RDATE,OERTDAT                                                    
*        MVC   RGIN-RECVEXTD(L'RGIN,RE),OERTGIN                                 
                                                                                
SETAUDY  LHI   R1,1                                                             
         J     SETAUDX                                                          
SETAUDN  LHI   R1,0                                                             
SETAUDX  SAM24 ,                                                                
         CHI   R1,1                                                             
         J     EXIT                                                             
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
* Set Action for ADD Scenario                                         *         
***********************************************************************         
                                                                                
SETADD   DS    0H                                                               
                                                                                
         TM    RECIND,RECIADD      Add scenario?                                
         JZ    SETADDX                                                          
                                                                                
         TM    RECIND,RECIBUT      Any data in 'To' Buffer?                     
         JZ    SETADDX                                                          
                                                                                
         SAM31 ,                                                                
         L     RF,ABUFTO           Set any action to be an 'add'                
         MVI   RRECTY-RRECD(RF),RRECTADD                                        
         SAM24 ,                                                                
                                                                                
SETADDX  DS    0H                                                               
         BR    RE                                                               
                                                                                
***********************************************************************         
* Set Action for COPY Scenario                                        *         
***********************************************************************         
                                                                                
SETCOP   DS    0H                                                               
                                                                                
         SAM31 ,                                                                
         L     RF,ABUSFR           Set any action to be a 'copy'                
         MVI   RRECTY-RRECD(RF),RRECTCPY                                        
         SAM24 ,                                                                
                                                                                
SETCOPX  DS    0H                                                               
         BR    RE                                                               
                                                                                
***********************************************************************         
* Add recovery extension (for empty GIN) if not present               *         
* NTRY - R1=A(I/O Area)                                               *         
***********************************************************************         
                                                                                
ADDEXT   NTR1  ,                                                                
                                                                                
         USING RRECD,R3            R3 used for recovery base                    
         L     R3,0(R1)            A(Record)                                    
         TM    RTIME,X'40'         Does extension exist?                        
         JNZ   ADDEXT2                                                          
                                                                                
         LR    R4,R3                                                            
         LH    R1,RRECLN                                                        
         AR    R4,R1                                                            
         USING RECVEXTD,R4                                                      
         XC    RECVEXT(RXLENQ),RECVEXT                                          
         MVI   RXLEN,RXLENQ                                                     
         DROP  R4                                                               
                                                                                
         OI    RTIME,X'40'                                                      
                                                                                
         LH    R1,RRECLN                                                        
         AHI   R1,RXLENQ                                                        
         STH   R1,RRECLN                                                        
                                                                                
ADDEXT2  NI    RTIME+L'RTIME-1,X'F0'     'Normalize' RTIME to 'C'               
         OI    RTIME+L'RTIME-1,X'0C'     for RTIME sorting  ...                 
         DROP  R3                                                               
                                                                                
ADDEXTX  DS    0H                                                               
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Set SV_SEQ and SV_ENT 'Record sequence value' and 'Entity key'      *         
* NTRY - R1=A(I/O Area)                                               *         
***********************************************************************         
                                                                                
SETSEQ   NTR1  ,                                                                
                                                                                
         SAM31 ,                                                                
         USING RRECD,R3            R3 used for recovery base                    
         L     R3,0(R1)            A(Record)                                    
         MVI   SV_SEQ,SEQ_IS_0                                                  
         XC    TEMP(42+4),TEMP                                                  
         CLI   RECTYP,ACRTAUDT                                                  
         JE    SETSAU                                                           
         CLI   RECTYP,ACRTORD                                                   
         JE    SETSOR                                                           
         CLI   RECTYP,ACRTESTR                                                  
         JE    SETSES                                                           
         J     SETOTH                                                           
                                                                                
SETSAU   DS    0H                                                               
         MVI   SV_SEQ,FFQ                                                       
         CLI   RRECORD+AUDKAUDT-AUDRECD,AUDKORD                                 
         JE    SETSAO                                                           
         CLI   RRECORD+AUDKAUDT-AUDRECD,AUDKEST                                 
         JNE   SETSEQX                                                          
                                                                                
         MVI   TEMP+OERTKOE-OERTKEY,ACRTESTR                                    
         MVC TEMP+OERTKUN-OERTKEY(L'AUDKECPJ),RRECORD+AUDKECPJ-AUDRECD          
         MVC TEMP+OERTKUN-OERTKEY(L'AUDKELNO),RRECORD+AUDKELNO-AUDRECD          
         J     SETSUID                                                          
                                                                                
SETSAO   MVI   TEMP+OERTKOE-OERTKEY,ACRTORD                                     
         MVC TEMP+OERTKUN-OERTKEY(L'ORDKORD),RRECORD+AUDKORDN-AUDRECD           
         J     SETSUID                                                          
                                                                                
SETSOR   MVC   SV_SEQ,RRECORD+ORDKSEQ-ORDRECD                                   
         MVI   TEMP+OERTKOE-OERTKEY,ACRTORD                                     
         MVC TEMP+OERTKUN-OERTKEY(L'ORDKORD),RRECORD+ORDKORD-ORDRECD            
         J     SETSUID                                                          
                                                                                
SETSES   MVC   SV_SEQ,RRECORD+ESTKSEQ-ESTRECD                                   
         LA    R5,AGYLIST                                                       
                                                                                
SETSES2  CLI   0(R5),0             (???)                                        
         JE    *+2                                                              
         CLC   0(1,R5),RECCPY                                                   
         JE    SETSES4                                                          
         AHI   R5,AGYLNQ                                                        
         J     SETSES2                                                          
                                                                                
SETSES4  MVI   TEMP+OERTKOE-OERTKEY,ACRTESTR                                    
         MVC   TEMP+100(16),SPACES                                              
         LA    R1,TEMP+100                                                      
         LLC   RE,1(R5)                                                         
         SHI   RE,1                                                             
         MVC   0(0,R1),RRECORD+ESTKCLI-ESTRECD                                  
         EXRL  RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R1,RE                                                            
         LLC   RE,2(R5)                                                         
         SHI   RE,1                                                             
         MVC   0(0,R1),RRECORD+ESTKPRO-ESTRECD                                  
         EXRL  RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R1,RE                                                            
         MVC   0(L'ESTKJOB,R1),RRECORD+ESTKJOB-ESTRECD                          
         MVC TEMP+OERTKUN-OERTKEY(L'AUDKECPJ),TEMP+100                          
         MVC TEMP+OERTKUN-OERTKEY(L'ESTKLNO),RRECORD+ESTKLNO-ESTRECD            
                                                                                
         USING OERTABD,R4                                                       
SETSUID  L     R4,AOERTAB                                                       
         LHI   R5,OERTMAX                                                       
                                                                                
SETSUID2 CLI   OERTKOE,FFQ                                                      
         JE    SETSUID4                                                         
         CLC   OERTKEY,TEMP                                                     
         JE    SETSUID6                                                         
         AHI   R4,OERTLNQ                                                       
         JCT   R5,SETSUID2                                                      
         MVC   P(28),=CL28'Increase of OERTMAX required'                        
         LA    R5,523                                                           
         GOTOR VPRINTER                                                         
         ABEND (R5),DUMP                                                        
                                                                                
SETSUID4 XC    OERTABD(OERTLNQ),OERTABD                                         
         MVC   OERTKEY,TEMP                                                     
         L     R1,RCV_UID                                                       
         AHI   R1,1                                                             
         ST    R1,RCV_UID                                                       
         MVC   OERTUID,RCV_UID                                                  
         MVC   TEMP+42(L'RCV_UID),RCV_UID                                       
         AHI   R4,OERTLNQ                                                       
         MVI   OERTKOE,FFQ                                                      
         J     SETSEQX                                                          
                                                                                
SETSUID6 MVC   TEMP+42(L'OERTUID),OERTUID                                       
         J     SETSEQX                                                          
         DROP  R4                                                               
                                                                                
SETOTH   CLI   RECTYP,ACRTTRN                                                   
         JE    SETOTH2                                                          
         CLI   RECTYP,ACRTTRNA                                                  
         JE    SETOTH2                                                          
         CLI   RECTYP,ACRTTIM                                                   
         JE    SETOTH2                                                          
         CLI   RECTYP,ACRTEXPC                                                  
         JNE   SETSEQX                                                          
                                                                                
SETOTH2  MVC   SV_SEQ,RECTYP                                                    
                                                                                
SETSEQX  MVC   SV_ENT,TEMP+42                                                   
         SAM24 ,                                                                
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Manipulate action for Transctions - set  lue' and 'Entity key'      *         
* NTRY - R1=A(I/O Area)                                               *         
***********************************************************************         
                                                                                
MANACT   NTR1  ,                                                                
                                                                                
         SAM31 ,                                                                
         USING RRECD,R3            R3 used for recovery base                    
         L     R3,0(R1)            A(Record)                                    
                                                                                
         CLI   RUNMODE,RUNM_MRG                                                 
         JE    MANACT4                                                          
                                                                                
         CLI   RECTYP,ACRTTRN                                                   
         JE    MANACT2                                                          
         CLI   RECTYP,ACRTTRNA                                                  
         JE    MANACT2                                                          
         J     MANACTX                                                          
                                                                                
MANACT2  CLI   RRECTY,RRECTADD                                                  
         JNE   MANACTX                                                          
         MVI   RRECTY,RECADDOQ                                                  
         J     MANACTX                                                          
                                                                                
MANACT4  CLI   RRECTY,RECADDOQ                                                  
         JNE   MANACTX                                                          
         MVI   RRECTY,RRECTADD                                                  
                                                                                
MANACTX  SAM24 ,                                                                
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
RECADDOQ EQU   X'00'                                                            
                                                                                
***********************************************************************         
* Add to or set from TIM table (to make RTIME unique per transaction  *         
* NTRY - R1=A(I/O Area)                                               *         
***********************************************************************         
                                                                                
ADDTIM   NTR1  ,                                                                
                                                                                
         SAM31 ,                                                                
                                                                                
         USING RRECD,R3            R3 used for recovery base                    
         L     R3,0(R1)            A(Record)                                    
         LH    R4,RRECLN                                                        
         SHI   R4,1                                                             
         AR    R4,R3                                                            
         LLC   R1,0(R4)                                                         
         SHI   R1,1                                                             
         SR    R4,R1                                                            
         USING RECVEXTD,R4                                                      
         USING ACCRECD,RRECORD                                                  
                                                                                
         USING TIMTABD,R2                                                       
         L     R2,ATIMTAB                                                       
         L     R5,=A(TIMTMAX)                                                   
                                                                                
ADDT02   OC    TIMTSIN,TIMTSIN     OeT?                                         
         JZ    ADDT10                                                           
         CLC   TIMTSIN,RSIN                                                     
         JNE   ADDT04                                                           
         CLC   TIMTGIN,RGIN                                                     
         JE    ADDT06                                                           
                                                                                
ADDT04   AHI   R2,TIMTLNQ                                                       
         JCT   R5,ADDT02                                                        
         MVC   P(28),=CL28'Increase of TIMTMAX required'                        
         LA    R5,524                                                           
         GOTOR VPRINTER                                                         
         ABEND (R5),DUMP                                                        
                                                                                
ADDT06   MVC   RDATE,TIMTDAT       SAVE RDATE/RTIME FROM FIRST RECORD           
         MVC   RTIME,TIMTTIM       WITH SAME RSIN/RGIN                          
         J     ADDTIMX                                                          
                                                                                
ADDT10   MVC   TIMTSIN,RSIN                                                     
         MVC   TIMTGIN,RGIN                                                     
         MVC   TIMTDAT,RDATE                                                    
         MVC   TIMTTIM,RTIME                                                    
         AHI   R2,TIMTLNQ                                                       
         XC    TIMTSIN,TIMTSIN                                                  
                                                                                
ADDTIMX  SAM24 ,                                                                
         J     EXIT                                                             
         DROP  R2,R3,R4                                                         
                                                                                
***********************************************************************         
* Check record and if applicable add to SIN table                     *         
* NTRY - R1=A(I/O Area)                                               *         
***********************************************************************         
                                                                                
ADDSIN   NTR1  ,                                                                
                                                                                
         SAM31 ,                                                                
         USING RRECD,R3            R3 used for recovery base                    
         L     R3,0(R1)            A(Record)                                    
         LH    R4,RRECLN                                                        
         SHI   R4,1                                                             
         AR    R4,R3                                                            
         LLC   R1,0(R4)                                                         
         SHI   R1,1                                                             
         SR    R4,R1                                                            
         USING RECVEXTD,R4                                                      
         USING ACCRECD,RRECORD                                                  
                                                                                
         CLI   RRECTY,RRECTCHG     Change or Add entries only                   
         JE    ASIN02                                                           
         CLI   RRECTY,RRECTADD                                                  
         JNE   ADDSINX                                                          
                                                                                
ASIN02   CLI   RECTYP,ACRTORD      Order and Estimates only                     
         JE    ASIN12                                                           
         CLI   RECTYP,ACRTESTR                                                  
         JNE   ADDSINX                                                          
                                                                                
         USING ESTRECD,R5                                                       
         LA    R5,RRECORD          Estimate sequences 0/1 only                  
         MVC   TEMP(L'ESTKEY),ESTKEY                                            
         MVI   TEMP+ESTKSEQ-ESTRECD,ESTKSMQ                                     
         CLI   ESTKSEQ,SEQ_IS_1                                                 
         JH    ADDSINX                                                          
         MVI   BYTE,SINTMAQ        Maintenance                                  
         JE    ASIN04                                                           
         CLI   RPRG,RCVPPROQ       From =Prod                                   
         JE    ASIN04              =Prod is maintenance call                    
         MVI   BYTE,SINTSTQ        Status                                       
                                                                                
         USING SINTABD,R2                                                       
ASIN04   L     R2,ASINTAB                                                       
                                                                                
ASIN06   CLI   SINTIND,FFQ                                                      
         JE    ASIN30                                                           
         CLC   SINTIND,RECTYP                                                   
         JNE   ASIN08                                                           
         CLC   SINTSIN,RSIN                                                     
         JNE   ASIN08                                                           
         CLC   SINTGIN,RGIN                                                     
         JNE   ASIN08                                                           
         CLC   SINTKEY,TEMP                                                     
         JE    ASIN10                                                           
                                                                                
ASIN08   AHI   R2,SINTLNQ                                                       
         J     ASIN06                                                           
                                                                                
ASIN10   MVC   SINTSOM,BYTE                                                     
         J     ASIN30                                                           
         DROP  R5                                                               
                                                                                
         USING ORDRECD,R5                                                       
ASIN12   LA    R5,RRECORD                                                       
         MVC   TEMP(L'ORDKEY),ORDKEY                                            
                                                                                
         MVI   TEMP+ORDKSEQ-ORDRECD,SEQ_IS_0                                    
         CLI   ORDKSEQ,SEQ_IS_0    Main order record?                           
         JNE   ASIN14                                                           
         MVI   BYTE,SINTMAQ                                                     
         CLI   RPRG,RCVPORDQ       =Ord is maintenance call                     
         JE    ASIN30                                                           
         CLI   RRECTY,RRECTADD     Add is maintenance                           
         JE    ASIN30                                                           
         TM    ORDRSTA2,ORDSSTAT   New 'Status' indicator                       
         JZ    ASIN30                                                           
         MVI   BYTE,SINTSTQ        Status update                                
         J     ASIN30                                                           
                                                                                
ASIN14   CLI   RPRG,RCVPORDQ       Skip anything else from =Ord                 
         JE    ADDSINX                                                          
         CLI   RPRG,RCVPBRAQ       Skip anything outside =BRA                   
         JNE   ADDSINX                                                          
                                                                                
         USING SINTABD,R2                                                       
         L     R2,ASINTAB                                                       
                                                                                
ASIN16   CLI   SINTIND,FFQ                                                      
         JE    *+2                 (invalid/unknown data structure)             
         CLC   SINTIND,RECTYP                                                   
         JNE   ASIN18                                                           
         CLC   SINTSIN,RSIN                                                     
         JNE   ASIN18                                                           
         CLC   SINTGIN,RGIN                                                     
         JNE   ASIN18                                                           
         CLC   SINTKEY,TEMP                                                     
         JE    ASIN20                                                           
                                                                                
ASIN18   AHI   R2,SINTLNQ                                                       
         J     ASIN16                                                           
                                                                                
ASIN20   CLI   ORDKSEQ,SEQ_IS_1    Ignore all but sequences 1 and E             
         JE    ASIN22                                                           
         CLI   ORDKSEQ,ORDKEXTN                                                 
         JNE   ADDSINX                                                          
                                                                                
ASIN22   MVI   SINTSOM,SINTMAQ     Make it a maintenance call                   
         J     ADDSINX                                                          
         DROP  R2,R5                                                            
                                                                                
         USING SINTABD,R2                                                       
ASIN30   L     R2,ASINTAB                                                       
         LHI   RE,SINTMAX                                                       
                                                                                
ASIN32   CLI   SINTIND,FFQ                                                      
         JE    ASIN34                                                           
         AHI   R2,SINTLNQ                                                       
         JCT   RE,ASIN32                                                        
                                                                                
         SAM24 ,                                                                
         MVC   P(28),=CL28'Increase of SINTMAX required'                        
         LA    R5,521                                                           
         GOTOR VPRINTER                                                         
         ABEND (R5),DUMP                                                        
                                                                                
ASIN34   AP    COUNTSIN,PONE                                                    
         MVC   SINTIND,RECTYP                                                   
         MVC   SINTSOM,BYTE                                                     
         MVC   SINTSIN,RSIN                                                     
         MVC   SINTGIN,RGIN                                                     
         MVC   SINTKEY,TEMP                                                     
         MVI   SINTIND+SINTLNQ,FFQ                                              
         DROP  R2                                                               
                                                                                
ADDSINX  SAM24 ,H                                                               
         J     EXIT                                                             
         DROP  R3,R4                                                            
                                                                                
***********************************************************************         
* Routine to open all files.                                          *         
* Either open the recovery file (ACC.ACCRCV%%) or the recovery        *         
* tape (DD of RCVTAPE). RCVOUT is always opened.                      *         
*                                                                     *         
* NTRY - R1=A(SE number of systen to be opened)                       *         
*        "INPUT"=R(ecover)/T(ape)                                     *         
* EXIT - SE number in UTL set to requested system with files open     *         
*        Tables COMPTAB built                                         *         
***********************************************************************         
                                                                                
OPENSYS  NTR1  ,                                                                
         L     RF,VUTL                                                          
         MVC   4(1,RF),0(R1)       Set UTL Value                                
         GOTOR VDATAMGR,DMCB,DMOPEN,ACCSYS,OPENLST,AIO3                         
                                                                                
         CLI   INPUT,C'T'          Input of T(ape)?                             
         JNE   OPENS02                                                          
         OPEN  (RCVIN,INPUT)       Open recovery tape                           
                                                                                
OPENS02  DS    0H                                                               
         OPEN  (RCVOUT,OUTPUT)     Open output                                  
                                                                                
* Build table of companies to be processed (AGYLIST)                            
                                                                                
         MVI   KEYSAVE,C' '                                                     
         MVC   KEYSAVE+1(L'KEYSAVE-1),KEYSAVE                                   
                                                                                
         XC    AGYLIST,AGYLIST                                                  
         LA    R2,AGYLIST                                                       
         LHI   R4,AGYLMXQ                                                       
                                                                                
DIR      USING CPYRECD,KEY                                                      
OPENS06  LLC   R1,KEYSAVE                                                       
         AHI   R1,1                                                             
         STC   R1,KEYSAVE                                                       
         CHI   R1,X'FE'                                                         
         JE    OPENS20                                                          
                                                                                
         MVC   DIR.CPYKEY,KEYSAVE                                               
         GOTOR VDATAMGR,DMCB,DMREAD,ACCDIR,DIR.CPYKEY,DIR.CPYKEY                
         JNE   OPENS06                                                          
         GOTOR VDATAMGR,DMCB,GETREC,ACCMST,DIR.CPYKDA,AIO2,DMWORK               
         JNE   *+2                                                              
                                                                                
         L     RF,AIO2                                                          
         LA    R1,CPYRFST-CPYRECD(RF)                                           
         USING CPYELD,R1                                                        
                                                                                
OPENS08  CLI   CPYEL,CPYELQ                                                     
         JE    OPENS10                                                          
         CLI   CPYEL,0                                                          
         JE    *+2                                                              
         LLC   R0,CPYLN                                                         
         AR    R1,R0                                                            
         J     OPENS08                                                          
                                                                                
OPENS10  OC    FLTCPY,FLTCPY       Card filter?                                 
         JZ    OPENS18                                                          
         CLC   FLTCPY,CPYALPHA                                                  
         JE    OPENS14                                                          
         OC    FLTOCPY,FLTOCPY                                                  
         JZ    OPENS06                                                          
         LA    RE,FLTOCPY                                                       
         LHI   RF,FLTOMAX                                                       
                                                                                
OPENS12  CLC   0(2,RE),SPACES                                                   
         JNH   OPENS06                                                          
         CLC   CPYALPHA,0(RE)                                                   
         JE    OPENS14                                                          
         AHI   RE,2                                                             
         JCT   RF,OPENS12                                                       
         J     OPENS06                                                          
                                                                                
OPENS14  MVC   0(1,R2),DIR.CPYKCPY                                              
         AHI   R2,AGYLNQ                                                        
         J     OPENS06                                                          
                                                                                
OPENS18  CLI   CPYLN,CPYSTATD-CPYELD                                            
         JL    OPENS06                                                          
         TM    CPYSTATD,CPYSGPDX                                                
         JZ    OPENS06                                                          
         MVC   0(1,R2),DIR.CPYKCPY                                              
         XC    1(AGYLNQ-1,R2),1(R2)                                             
         AHI   R2,AGYLNQ                                                        
         JCT   R4,OPENS06                                                       
                                                                                
         MVC   P(28),=CL28'Increase of AGYMXQ required'                         
         LA    R5,522                                                           
         GOTOR VPRINTER                                                         
         ABEND (R5),DUMP                                                        
         DROP  DIR,R1                                                           
                                                                                
OPENS20  DS    0H                                                               
         OC    AGYLIST,AGYLIST                                                  
         JZ    CARDER7                                                          
                                                                                
         LA    R2,AGYLIST          Set SJ level length for companies            
                                                                                
DIR      USING LDGRECD,KEY                                                      
OPENS22  CLI   0(R2),0                                                          
         JE    OPENSYSX                                                         
         MVC   DIR.LDGKEY,SPACES                                                
         MVC   DIR.LDGKCPY,0(R2)                                                
         MVI   DIR.LDGKUNT,C'S'                                                 
         MVI   DIR.LDGKLDG,C'J'                                                 
         GOTOR VDATAMGR,DMCB,DMREAD,ACCDIR,DIR.LDGKEY,DIR.LDGKEY                
         JNE   *+2                 (Company has no SJ ledger)                   
         GOTOR VDATAMGR,DMCB,GETREC,ACCMST,DIR.LDGKDA,AIO2,DMWORK               
         JNE   *+2                                                              
                                                                                
         USING ACLELD,R1                                                        
         L     RF,AIO2                                                          
         LA    R1,LDGRFST-LDGRECD(RF)                                           
                                                                                
OPENS24  CLI   ACLEL,0                                                          
         JE    *+2                 (SJ ledger has no levels)                    
         CLI   ACLEL,ACLELQ                                                     
         JE    OPENS26                                                          
         LLC   RE,ACLLN                                                         
         AR    R1,RE                                                            
         J     OPENS24                                                          
                                                                                
OPENS26  MVC   1(1,R2),ACLELLVA                                                 
         LLC   RE,ACLELLVB                                                      
         LLC   RF,ACLELLVA                                                      
         SR    RE,RF                                                            
         STC   RE,2(R2)                                                         
         LLC   RE,ACLELLVC                                                      
         LLC   RF,ACLELLVB                                                      
         SR    RE,RF                                                            
         CHI   RE,L'ESTKJOB                                                     
         JNH   OPENS28                                                          
         LHI   RE,L'ESTKJOB                                                     
                                                                                
OPENS28  STC   RE,3(R2)                                                         
                                                                                
         AHI   R2,AGYLNQ                                                        
         J     OPENS22                                                          
         DROP  DIR,R1                                                           
                                                                                
OPENSYSX DS    0H                                                               
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Read recovery file records from file or tape.                       *         
* NTRY - R1=A(I/O Area)                                               *         
* EXIT - CC EQ: Record in I/O area                                    *         
*        CC NE: End of file                                           *         
***********************************************************************         
                                                                                
         USING RRECD,R3            R3 used for recovery base                    
READRCV  NTR1  ,                                                                
         L     R3,0(R1)            A(OUTPUT)                                    
         LA    R0,RREC             Clear the                                    
         LHI   R1,RRECLEN            recovery                                   
         XR    RE,RE                   buffer                                   
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         CLI   INPUT,C'T'          Tape ?                                       
         JE    READR06             Yes                                          
                                                                                
* Read Recovery file                                                            
READR02  GOTOR VDATAMGR,DMCB,(X'3C',DMRSEQ),ACCRCV,DA,RREC,ATRKBUF              
         TM    8(R1),X'FF'-X'80'                                                
         JNZ   READR04                                                          
         TM    8(R1),X'80'         TEST E-O-F                                   
         JNZ   READR10                                                          
         XR    RE,RE                                                            
         ICM   RE,3,18(R1)                                                      
         JZ    READR02                                                          
         LA    RE,4(RE)            Set record length                            
         SLL   RE,16                                                            
         STCM  RE,15,RRECLN                                                     
         J     READR08                                                          
                                                                                
* Handle recovery file disk errors.                                             
READR04  MVC   P(28),=C'Disk error on recovery file='                           
         MVC   P+28(8),ACCRCV                                                   
         GOTOR VLOGIO,WORK,1,(60,P)                                             
         GOTOR VPRINTER                                                         
         MVC   P(17),=C'DA=XXXXXXXX,DMCB='                                      
         GOTOR VHEXOUT,WORK,DA,P+3,4,=C'TOG'                                    
         GOTOR (RF),(R1),DMCB,P+17,20,=C'TOG'                                   
         GOTOR VLOGIO,WORK,1,(60,P)                                             
         GOTOR VPRINTER                                                         
         LH    R3,DA               Bump to next track                           
         LA    R3,1(R3)                                                         
         STH   R3,DA                                                            
         MVC   DA+2(2),=X'0101'    Blocked                                      
         J     READR02             Try next                                     
                                                                                
* Read recovery tape                                                            
READR06  GET   RCVIN,(R3)                                                       
                                                                                
* Exit                                                                          
READR08  AP    COUNTALL,PONE                                                    
                                                                                
         CR    RB,RB               Set r/c EQ                                   
         J     EXIT                                                             
                                                                                
READR10  LTR   RB,RB               Set r/c NE                                   
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* SNIPTAB FREEMAIN etc.                                               *         
***********************************************************************         
                                                                                
SNIPEND  NTR1  ,                                                                
                                                                                
         SAM31 ,                                                                
         LHI   R0,SNIPTLNQ*SNIPTMXQ                                             
         L     R1,ASNIPTAB                                                      
         FREEMAIN R,A=(1),LV=(0)                                                
         SAM24 ,                                                                
                                                                                
         J     EXIT                                                             
                                                                                
***********************************************************************         
* SNIPTAB set up from SNIPFILE                                        *         
***********************************************************************         
                                                                                
SNIPSUP  NTR1  ,                                                                
                                                                                
         L     R3,AIO3             A(OUTPUT)                                    
                                                                                
         SAM31 ,                                                                
         LHI   R0,SNIPTLNQ*SNIPTMXQ                                             
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ASNIPTAB                                                      
         MVI   0(R1),0                                                          
         LR    R4,R1                                                            
         SAM24 ,                                                                
                                                                                
         LHI   R5,SNIPTMXQ                                                      
         OPEN  (SNIPFILE,INPUT)    Open file for input                          
                                                                                
SNIPS02  GET   SNIPFILE,(R3)                                                    
                                                                                
         MVC   0(SNIPTLNQ,R4),0(R3)                                             
                                                                                
         AHI   R4,SNIPTLNQ                                                      
         MVI   0(R4),0                                                          
         JCT   R5,SNIPS02                                                       
         J     *+2                 (increase SNIPTMXQ)                          
                                                                                
SNIPSUPX DS    0H                  Close file                                   
         CLOSE (SNIPFILE)                                                       
         J     EXIT                                                             
                                                                                
***********************************************************************         
* Write record                                                        *         
* This will put out the record and count it.                          *         
* NTRY - R1=A(I/O Area)                                               *         
* EXIT - N/A                                                          *         
***********************************************************************         
                                                                                
         USING RRECD,R3            R3 used for recovery base                    
PUTREC   NTR1  ,                                                                
         L     R3,0(R1)            A(Record)                                    
                                                                                
         CLI   RUNMODE,RUNM_FLT    Filter mode = Step 1?                        
         JNE   PUTR02                                                           
         L     R2,RCV_SEQ                                                       
         LHI   R0,3                (1 + 2 spare for ESTLOG purposes)            
         AR    R2,R0                                                            
         STCM  R2,B'1111',RVCHR    Set sequence for next steps                  
         ST    R2,RCV_SEQ                                                       
         MVC   RAG,SV_SEQ          Set O&E record sequence                      
         MVC   RPERSON,SV_ENT1     Set O&E record pseudo key                    
         MVC   RUSER,SV_ENT2                                                    
                                                                                
PUTR02   PUT   RCVOUT,0(R3)        Move it out                                  
                                                                                
         AP    COUNTOUT,PONE                                                    
                                                                                
* may print details here                                                        
*        MVC   P(80),...                                                        
*        GOTOR VPRINTER                                                         
                                                                                
         DS    0H                                                               
         J     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* Close files                                                         *         
***********************************************************************         
CLOSE    NTR1  ,                                                                
         CLOSE (RCVOUT)            Close o/p file                               
         CLI   INPUT,C'T'          Was input tape ?                             
         JNE   CLO02               No                                           
         CLOSE (RCVIN)                                                          
                                                                                
CLO02    DS    0H                                                               
         GOTOR VDATAMGR,DMCB,DMCLSE,ACCSYS,OPENLST,AIO3                         
                                                                                
         MVC   P,SPACES                                                         
         GOTOR VPRINTER            Print out total                              
         MVC   P,SPACES                                                         
         MVC   P+1(33),=CL33'Total number of records read in'                   
         EDITR (P8,COUNTALL),(12,P+35),0,ZERO=NOBLANK                           
         GOTOR VPRINTER            Print out total                              
         MVC   P,SPACES                                                         
         MVC   P+1(33),=CL33'Total number of records to output'                 
         EDITR (P8,COUNTOUT),(12,P+35),0,ZERO=NOBLANK                           
         GOTOR VPRINTER            Print out output                             
         MVC   P,SPACES                                                         
         MVC   P+1(33),=CL33'Total number of SIN table entries'                 
         EDITR (P8,COUNTSIN),(12,P+35),0,ZERO=NOBLANK                           
         GOTOR VPRINTER            Print out output                             
                                                                                
         SAM31 ,                                                                
         LHI   R0,BUFFLNQ                                                       
         L     R1,ABUFFR                                                        
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         LHI   R0,BUFTLNQ                                                       
         L     R1,ABUFTO                                                        
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         LHI   R0,BUSFLNQ                                                       
         L     R1,ABUSFR                                                        
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         LHI   R0,BUSTLNQ                                                       
         L     R1,ABUSTO                                                        
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         L     R0,ASINTLN                                                       
         L     R1,ASINTAB                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         L     R0,AOERTLN                                                       
         L     R1,AOERTAB                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
                                                                                
         L     R0,ATIMTLN                                                       
         L     R1,ATIMTAB                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
         SAM24 ,                                                                
                                                                                
         J     EXIT                                                             
                                                                                
*********************************************************************           
* General exits                                                     *           
*********************************************************************           
                                                                                
EXITL    DS    0H                  Low                                          
EXITN    LHI   R0,0                Not Equal                                    
         J     EXITCC                                                           
EXITY    LHI   R0,1                Equal                                        
         J     EXITCC                                                           
EXITH    LHI   R0,2                High                                         
                                                                                
EXITCC   CHI   R0,1                Set condition code                           
                                                                                
EXIT     XIT1  ,                                                                
                                                                                
*********************************************************************           
* Initialisation routine.                                           *           
* 1. Set up working storage.                                        *           
* 2. Process SYSIN cards.                                           *           
*********************************************************************           
                                                                                
INITIAL  NTR1  ,                                                                
                                                                                
* Set up External routines                                                      
         MVC   VCARDS,=V(CARDS)                                                 
         MVC   VDATCON,=V(DATCON)                                               
         MVC   VDATVAL,=V(DATVAL)                                               
         MVC   VDATAMGR,=V(DATAMGR)                                             
         MVC   VHELLO,=V(HELLO)                                                 
         MVC   VRECTYP,=V(ACRECTYP)                                             
         MVC   VADDAY,=V(ADDAY)                                                 
         MVC   VHEXIN,=V(HEXIN)                                                 
         MVC   VHEXOUT,=V(HEXOUT)                                               
         MVC   VLOGIO,=V(LOGIO)                                                 
         MVC   VPRINTER,=V(PRINTER)                                             
         MVC   VPRNTBL,=V(PRNTBL)                                               
         MVC   VNUMVAL,=V(NUMVAL)                                               
         MVC   VLOADER,=V(LOADER)                                               
         MVC   VUTL,=V(UTL)                                                     
         MVC   ASSB,=A(SSB)                                                     
                                                                                
* Set up working storage addresses                                              
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
         L     R1,=A(IOAREA2-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO2                                                          
         L     R1,=A(IOAREA3-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO3                                                          
         MVC   ATRKBUF,=A(TRKBUF)                                               
         MVC   ATRLOWER,=A(TRLOWER)                                             
                                                                                
         XC    SPACES,SPACES       Preset SPACES to space                       
         TR    SPACES,=C' '                                                     
         ZAP   COUNTOUT,PZERO                                                   
         ZAP   COUNTALL,PZERO                                                   
         ZAP   COUNTSIN,PZERO                                                   
         XC    RCV_SEQ,RCV_SEQ                                                  
         XC    RCV_UID,RCV_UID                                                  
                                                                                
         SAM31 ,                                                                
         LHI   R0,BUFFLNQ                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ABUFFR                                                        
                                                                                
         LHI   R0,BUFTLNQ                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ABUFTO                                                        
                                                                                
         LHI   R0,BUSFLNQ                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ABUSFR                                                        
                                                                                
         LHI   R0,BUSTLNQ                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ABUSTO                                                        
                                                                                
         LHI   R0,SINTLNQ                                                       
         MHI   R0,SINTMAX                                                       
         ST    R0,ASINTLN                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ASINTAB                                                       
         MVI   SINTIND-SINTABD(R1),FFQ                                          
                                                                                
         LHI   R0,OERTLNQ                                                       
         MHI   R0,OERTMAX                                                       
         ST    R0,AOERTLN                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,AOERTAB                                                       
         MVI   OERTKEY-OERTABD(R1),FFQ                                          
                                                                                
         L     R0,=A(TIMTLEN)                                                   
         ST    R0,ATIMTLN                                                       
         GETMAIN R,LV=(R0)                                                      
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,ATIMTAB                                                       
         XC    TIMTSIN-TIMTABD(L'TIMTSIN,R1),TIMTSIN-TIMTABD(R1)                
         SAM24 ,                                                                
                                                                                
***********************************************************************         
* Read and validate control cards                                     *         
***********************************************************************         
                                                                                
         L     R7,AIO1             A(work area for control cards)               
         XC    TRTAB,TRTAB                                                      
         MVI   TRTAB+C'>',X'24'                                                 
         MVI   TRTAB+C'<',X'44'                                                 
         MVI   TRTAB+C'^',X'74'                                                 
         MVI   TRTAB+C'=',X'84'                                                 
                                                                                
* Read in cards                                                                 
VALC002  DS    0H                                                               
         GOTOR VCARDS,DMCB,(R7),=C'RE00'                                        
         CLC   =C'/*',0(R7)                                                     
         JE    VALC020                                                          
         LR    R3,R7               A(start of card)                             
         CLI   0(R3),C'*'          * in col 1 is a comment                      
         JE    VALC014             ignore comment cards                         
         LA    R1,79(R1)                                                        
         MVC   P(80),0(R7)                                                      
         GOTOR VPRINTER            Print out card                               
                                                                                
* Scan CARDTAB for card                                                         
         USING CARDTABD,R4                                                      
         LA    R4,CARDTAB                                                       
                                                                                
VALC004  DS    0H                                                               
         CLI   0(R4),EOTQ                                                       
         JE    CARDER2             Error if card not found                      
         LLC   R1,CARDKXLN                                                      
         CLC   0(0,R3),CARDKEY                                                  
         EXRL  R1,*-6                                                           
         JE    VALC006                                                          
         AHI   R4,CARDTBLQ         Try next entry in CARDTAB                    
         J     VALC004                                                          
                                                                                
* Register usage:                                                               
*   R2 - branch condition (set by TRT)                                          
*   R3 - points to the delimeter                                                
*   R7 - points to start of card                                                
VALC006  DS    0H                                                               
         LA    R3,1(R1,R3)         Point to delimiter                           
         XR    RF,RF                                                            
         TM    CARDIND,CARDNUMQ    Numbers before delimeter ?                   
         JNO   VALC008             No                                           
         LA    RF,79(R7)           A(end of card)                               
         SR    RF,R3               L' to check                                  
                                                                                
VALC008  DS    0H                                                               
         TRT   0(0,R3),TRTAB       Is this a valid delimeter ?                  
         EXRL  RF,*-6                                                           
         JZ    CARDER2             No, error                                    
         ICM   RF,15,CARDVDSP                                                   
         TM    CARDIND,CARDRTNQ    Validation routine ?                         
         JNO   VALC010             No                                           
         BASR  RE,RF               Yes, call validation routine                 
         JNE   CARDER2                                                          
         J     VALC012                                                          
                                                                                
* Move value into local storage                                                 
VALC010  DS    0H                                                               
         IC    R1,CARDVXLN         Get len for move                             
         LA    RF,WORKD(RF)                                                     
         MVC   0(0,RF),1(R3)       Move to output area                          
         EXRL  R1,*-6                                                           
                                                                                
VALC012  DS    0H                                                               
         MVI   ANYCARDS,YESQ                                                    
                                                                                
VALC014  DS    0H                                                               
         J     VALC002                                                          
                                                                                
* All cards have been processed. Now validate cards in context.                 
                                                                                
VALC020  DS    0H                                                               
         CLI   ANYCARDS,YESQ                                                    
         JNE   CARDER1                                                          
                                                                                
         CLI   RUNMODE,SPACEQ                                                   
         JNH   CARDER8                                                          
                                                                                
         CLI   RUNMODE,RUNM_MRG                                                 
         JNE   VALC022                                                          
         CLI   INPUT,C'T'          Ensure Input is Tape                         
         JNE   CARDER9                                                          
                                                                                
* Validate DSPACE                                                               
VALC022  DS    0H                                                               
         CLI   DSPACE,0                                                         
         JE    CARDER5             Must provide DSPACE=                         
         CLI   DSPACE,C'T'                                                      
         JE    VALC024                                                          
         CLI   DSPACE,C'A'                                                      
         JE    VALC024                                                          
         CLI   DSPACE,C'C'                                                      
         JE    VALC024                                                          
         CLI   DSPACE,C'Q'                                                      
         JE    VALC024                                                          
         CLI   DSPACE,C'B'                                                      
         JNE   CARDER6                                                          
                                                                                
         USING SSOOFF,RF                                                        
VALC024  L     RF,ASSB                                                          
         MVC   SSODSPAC,DSPACE     Set dataspace ID                             
         MVI   SSOSTAT2,SSOSGALO+SSOSNRCV                                       
                                                                                
         L     RF,VUTL             SET SE NO. FOR CHOSEN SYS                    
         MVI   4(RF),CONSYSQ                                                    
         L     R7,AIO2             OPEN CT FILE FIRST                           
         LARL  RF,CFILLIST                                                      
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONSYS,(RF),(R7)                            
                                                                                
         USING CTWREC,R2                                                        
         L     R2,AIO1             READ SYSTEM LIST ACC RECORD                  
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
         MVI   CTWKSYSN,ACCQ                                                    
         GOTOR VDATAMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                        
         JNE   CARDER3             NOT FOUND - INVALID                          
         USING CTLSTD,R1                                                        
         LA    R1,CTWDATA                                                       
                                                                                
VALC026  CLI   CTLSTD,0            TEST END OF RECORD - INVALID                 
         JE    CARDER3                                                          
         CLI   CTLSTEL,CTLSTELQ    TEST LIST ELEMENT                            
         JNE   VALC028                                                          
         CLI   CTLSTSYS,ACCSYSQ    TEST ACCPAK SE LIST ENTRY                    
         JNE   VALC028                                                          
         LARL  RF,SYSACC                                                        
         CLC   CTLSTNAM(3),0(RF)                                                
         JNE   VALC028                                                          
         CLC   CTLSTNAM+3(2),THISSEAL                                           
         JNE   VALC028                                                          
         MVC   THISSENO,CTLSTSE                                                 
         J     VALC030                                                          
                                                                                
VALC028  LLC   R0,CTLSTLEN         BUMP TO NEXT ELEMENT ON LIST RE              
         AR    R1,R0                                                            
         J     VALC026                                                          
         DROP  R1,R2                                                            
                                                                                
VALC030  L     R7,AIO2             OPEN CT FILE FIRST                           
         LARL  RF,CFILLIST                                                      
         GOTO1 VDATAMGR,DMCB,DMCLSE,CONSYS,(RF),(R7)                            
                                                                                
INITIALX DS    0H                                                               
         XIT1  ,                                                                
                                                                                
         DS    0H                                                               
SYSACC   DC    CL3'ACC'                                                         
         DS    0H                                                               
CFILLIST DS    0XL9                                                             
         DC    C'NCTFILE '         CONTROL FILE (FOR USERIDS)                   
         DC    C'X'                                                             
         DS    0H                                                               
                                                                                
* Invalid control card. Print it out and abend.                                 
CARDER1  MVC   P(20),=CL20'No control cards'                                    
         LA    R2,501                                                           
         J     VALCDIE                                                          
                                                                                
CARDER2  MVC   P(20),=CL20'Invalid control card'                                
         LA    R2,502                                                           
         J     VALCDIE                                                          
                                                                                
CARDER3  MVC   P(20),=CL20'Invalid acc system'                                  
         LA    R2,503                                                           
         J     VALCDIE                                                          
                                                                                
CARDER4  MVC   P(20),=CL20'INPUT must be T or R'                                
         LA    R2,504                                                           
         J     VALCDIE                                                          
                                                                                
CARDER5  MVC   P(18),=CL20'DSPACE= is missing'                                  
         LA    R2,505                                                           
         J     VALCDIE                                                          
                                                                                
CARDER6  MVC   P(20),=CL20'DSPACE= is invalid'                                  
         LA    R2,506                                                           
         J     VALCDIE                                                          
                                                                                
CARDER7  MVC   P(28),=CL28'No applicable company found'                         
         LA    R2,507                                                           
         J     VALCDIE                                                          
                                                                                
CARDER8  MVC   P(28),=CL28'No MODE= card found'                                 
         LA    R2,508                                                           
         J     VALCDIE                                                          
                                                                                
CARDER9  MVC   P(28),=CL28'MODE=MERGE requires INPUT=T'                         
         LA    R2,509                                                           
         J     VALCDIE                                                          
                                                                                
CARDER10 LHI   R2,510                                                           
         J     VALCDIE2                                                         
                                                                                
VALCDIE  DS    0H                                                               
         GOTOR VPRINTER            Print out card                               
                                                                                
VALCDIE2 ABEND (R2),DUMP                                                        
                                                                                
*********************************************************************           
* MODE=anananan                                                     *           
* Validate RUNMODE                                                  *           
*********************************************************************           
                                                                                
MODVAL   NTR1  ,                                                                
         LA    RF,5(R7)                                                         
         CLC   T_MERGE,0(RF)                                                    
         JE    MODVAL02                                                         
         CLC   T_FILTER,0(RF)                                                   
         JE    MODVAL04                                                         
         J     CARDERR                                                          
                                                                                
MODVAL02 MVI   RUNMODE,RUNM_MRG                                                 
         J     CARDOK                                                           
                                                                                
MODVAL04 MVI   RUNMODE,RUNM_FLT                                                 
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* WHICHSYS=AA                                                       *           
* Validate basic format. MEDSID is called at end of validation      *           
*********************************************************************           
                                                                                
SYSVAL   NTR1  ,                                                                
         LA    R2,79(R7)           Find length of RHS                           
         LHI   RE,-1                                                            
         LA    RF,8(R7)                                                         
         CLI   0(R2),C' '                                                       
         JNE   SYSVAL02                                                         
         BRXH  R2,RE,*-8                                                        
         J     CARDERR                                                          
                                                                                
SYSVAL02 SR    R2,RF                                                            
         CHI   R2,1                                                             
         JL    SYSVAL04                                                         
         CHI   R2,2                                                             
         JH    CARDERR                                                          
                                                                                
SYSVAL04 MVC   THISSEAL,9(R7)                                                   
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* FLASH=.                                                           *           
* Validate and set FLASH= parms.                                    *           
*********************************************************************           
                                                                                
FLASH    NTR1  ,                                                                
         L     RF,ASSB                                                          
         CLI   SSOXTND-SSOOFF(RF),FFQ                                           
         JNE   *+2                                                              
         CLI   6(R7),C'A'          Test if valid flash character                
         JL    CARDERR                                                          
         CLI   6(R7),C'N'          FLASH=N means no flash copy                  
         JE    CARDOK                                                           
         OI    SSOFLAG2-SSOOFF(RF),SSO2FLSH                                     
         MVC   SSOFLSHI-SSOOFF(1,RF),6(R7)                                      
         CLI   6(R7),C'Y'          FLASH=Y is default so set to FLASH=S         
         JNE   CARDOK              to cause DSN=FLS.XXX...                      
         MVI   SSOFLSHI-SSOOFF(RF),C'S'                                         
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* DDSIO=  validation                                                *           
*********************************************************************           
                                                                                
DSIOVAL  NTR1  ,                                                                
         LA    R2,6(R7)                                                         
         L     RF,=V(DDSIO)        Set up DDSIO override                        
         MVC   0(8,RF),0(R2)                                                    
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* AGENCY=AA                                                         *           
* Validate and convert two character alpha agency.                  *           
*********************************************************************           
                                                                                
AGYVAL   NTR1  ,                                                                
         XC    FLTCPY,FLTCPY                                                    
         XC    FLTOCPY,FLTOCPY                                                  
         LA    R2,79(R7)           Find length of RHS                           
         LHI   RE,-1                                                            
         LA    RF,6(R7)            A(end of field)                              
         CLI   0(R2),C' '                                                       
         JNE   AGYVAL2                                                          
         BRXH  R2,RE,*-8                                                        
         J     CARDERR                                                          
                                                                                
AGYVAL2  SR    R2,RF               Length                                       
         AHI   RF,1                                                             
         CHI   R2,L'FLTCPY                                                      
         JL    CARDERR                                                          
         MVC   FLTCPY,0(RF)                                                     
         CHI   R2,L'FLTCPY                                                      
         JE    CARDOK                                                           
         SHI   R2,2                                                             
         AHI   RF,2                                                             
                                                                                
         LA    R1,FLTOCPY                                                       
         LHI   RE,FLTOMAX                                                       
                                                                                
AGYVAL4  CHI   R2,0                                                             
         JE    CARDOK                                                           
         JL    CARDERR                                                          
         CLI   0(RF),C','                                                       
         JNE   CARDERR                                                          
         MVC   0(2,R1),1(RF)                                                    
         AHI   RF,3                                                             
         SHI   R2,3                                                             
         AHI   R1,2                                                             
         JCT   RE,AGYVAL4                                                       
         J     CARDERR                                                          
                                                                                
*********************************************************************           
* STTIME=HH:MM:SS                                                   *           
*********************************************************************           
                                                                                
STTMVAL  NTR1  ,                                                                
         LA    R2,79(R7)           Find length of RHS                           
         LHI   RE,-1                                                            
         LA    RF,6(R7)                                                         
         CLI   0(R2),C' '                                                       
         JNE   *+12                                                             
         BRXH  R2,RE,*-8                                                        
         J     CARDERR                                                          
         SR    R2,RF                                                            
         CHI   R2,8                Must be 8 chars                              
         JNE   CARDERR                                                          
         XC    DUB,DUB                                                          
         MVC   DUB(2),1(RF)        HH                                           
         MVC   DUB+2(2),4(RF)      MM                                           
         MVC   DUB+4(2),7(RF)      SS                                           
         GOTOR VNUMVAL,DMCB,DUB,(2,0)                                           
         CLI   0(R1),0                                                          
         JNE   CARDERR                                                          
         PACK  FLTSTTM,DUB(6)                                                   
         OI    FLTSTTM+3,X'0F'     Force sign                                   
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* ENDTIME=HH:MM:SS                                                  *           
*********************************************************************           
                                                                                
EDTMVAL  NTR1  ,                                                                
         LA    R2,79(R7)           FIND LENGTH OF RHS                           
         LHI   RE,-1                                                            
         LA    RF,7(R7)                                                         
         CLI   0(R2),C' '                                                       
         JNE   *+12                                                             
         BRXH  R2,RE,*-8                                                        
         J     CARDERR                                                          
         SR    R2,RF                                                            
         CHI   R2,8                Must be 8 chars                              
         JNE   CARDERR                                                          
         XC    DUB,DUB                                                          
         MVC   DUB(2),1(RF)        HH                                           
         MVC   DUB+2(2),4(RF)      MM                                           
         MVC   DUB+4(2),7(RF)      SS                                           
         GOTOR VNUMVAL,DMCB,DUB,(2,0)                                           
         CLI   0(R1),0                                                          
         JNE   CARDERR                                                          
         PACK  FLTEDTM,DUB(6)                                                   
         OI    FLTEDTM+3,X'0F'     Force sign                                   
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* SIN=XXXX                                                          *           
* Specifiy 3 byte system input number.                              *           
* Error if:                                                         *           
*   1. Key not 6 chars.                                             *           
*   2. Invalid characters in field. They must be hex.               *           
*********************************************************************           
                                                                                
SINVAL   NTR1  ,                                                                
         LA    R2,79(R7)           Find length of RHS                           
         LHI   RE,-1                                                            
         LA    RF,3(R7)            A(delimeter)                                 
         CLI   0(R2),C' '                                                       
         JNE   SINVAL02                                                         
         BRXH  R2,RE,*-8                                                        
         J     CARDERR                                                          
                                                                                
SINVAL02 DS    0H                                                               
         SR    R2,RF                                                            
         CHI   R2,6                Must be 6 chars                              
         LA    RF,1(,RF)           Bump past delimeter                          
         JNE   CARDERR                                                          
         GOTOR VHEXIN,DMCB,(RF),FLTSIN,(R2)                                     
         CLI   15(R1),0            Invalid ?                                    
         JE    CARDERR             Yes                                          
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* GIN=XXXXXXXX                                                      *           
* Specifiy 8 byte general input number.                             *           
* Error if:                                                         *           
*   1. Key not 16 chars.                                            *           
*   2. Invalid characters in field. They must be hex.               *           
*********************************************************************           
                                                                                
GINVAL   NTR1  ,                                                                
         LA    R2,79(R7)           Find length of RHS                           
         LHI   RE,-1                                                            
         LA    RF,3(R7)            A(delimeter)                                 
         CLI   0(R2),C' '                                                       
         JNE   GINVAL02                                                         
         BRXH  R2,RE,*-8                                                        
         J     CARDERR                                                          
                                                                                
GINVAL02 DS    0H                                                               
         SR    R2,RF                                                            
         CHI   R2,16               Must be 16 chars                             
         LA    RF,1(,RF)           Bump past delimeter                          
         JNE   CARDERR                                                          
         GOTOR VHEXIN,DMCB,(RF),FLTGIN,(R2)                                     
         CLI   15(R1),0            Invalid ?                                    
         JE    CARDERR             Yes                                          
         J     CARDOK                                                           
                                                                                
*********************************************************************           
* KEY=XXXXXX....                                                    *           
* Specifiy up to 40 characters. This will be converted into         *           
* up to a 20 byte key to filter records on.                         *           
* Error if:                                                         *           
*   1. Key greater than 40 chars.                                   *           
*   2. Length of key not even.                                      *           
*   3. Invalid characters in field. They must be hex.               *           
*********************************************************************           
                                                                                
KEYVAL   NTR1  ,                                                                
         CLC   0(3,R7),=C'KY2'                                                  
         JNE   KEYVAL02                                                         
         CLI   FLTKEYL,0           KY2?                                         
         JE    *+2                 KEY missing                                  
         J     KEYVAL04                                                         
                                                                                
KEYVAL02 STC   R2,FLTKEYC          Branch condition                             
                                                                                
KEYVAL04 LA    R2,79(R7)           Find length of RHS                           
         LHI   RE,-1                                                            
         LA    RF,3(R7)            A(delimeter)                                 
         CLI   0(R2),C' '                                                       
         JNE   KEYVAL06                                                         
         BRXH  R2,RE,*-8                                                        
         J     CARDERR                                                          
                                                                                
KEYVAL06 DS    0H                                                               
         SR    R2,RF               Found end of key                             
         CHI   R2,42               Greater than 42 char ?                       
         JH    CARDERR             Yes, error                                   
         LR    R1,R2               Check if                                     
         XR    R0,R0                 even number                                
         D     R0,=F'2'                of chars                                 
         LTR   R0,R0                                                            
         JNZ   CARDERR                                                          
         LA    RF,1(,RF)           Bump over delimeter                          
         LA    R4,FLTKEY                                                        
         CLC   0(3,R7),=C'KY2'                                                  
         JNE   KEYVAL08                                                         
         LA    R4,FLTKEY+21                                                     
                                                                                
KEYVAL08 GOTOR VHEXIN,DMCB,(RF),0(R4),(R2)                                      
         CLI   15(R1),0            Invalid ?                                    
         JE    CARDERR             Yes                                          
         CLC   0(3,R7),=C'KY2'                                                  
         JE    KEYVAL10                                                         
         MVC   FLTKEYL,15(R1)      Save length                                  
         J     CARDOK                                                           
                                                                                
KEYVAL10 LLC   RE,FLTKEYL                                                       
         LLC   RF,15(R1)                                                        
         AR    RE,RF                                                            
         STC   RE,FLTKEYL                                                       
         J     CARDOK                                                           
                                                                                
* Exit from card validation routines                                            
                                                                                
CARDOK   CR    RB,RB               Set r/c EQ                                   
         J     *+6                                                              
                                                                                
CARDERR  LTR   RB,RB               Set r/c NE                                   
         J     VALCARDX                                                         
                                                                                
VALCARDX XIT1  ,                                                                
                                                                                
*********************************************************************           
* Global literals and constants (addressed by RB)                   *           
*********************************************************************           
                                                                                
$$DATA   LOCTR ,                                                                
GLOBALS  DS    0D                                                               
         LTORG ,                                                                
                                                                                
                                                                                
T_MERGE  DC    CL8'MERGE'                                                       
T_FILTER DC    CL8'FILTER'                                                      
EFFS     DC    X'FFFFFFFF'                                                      
NULLS    DC    X'0000000000000000'                                              
DMOPEN   DC    C'OPEN   '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
GETREC   DC    C'GETREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
PUTRECLT DC    C'PUTREC '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMADD    DC    C'DMADD  '                                                       
REQUST   DC    C'REQUEST'                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    CL8'CTFILE'                                                      
CONSYS   DC    CL4'CON'                                                         
CONQ     EQU   X'0A'                                                            
CONSYSQ  EQU   CONQ                                                             
ACCQ     EQU   X'06'                                                            
ACCSYSQ  EQU   ACCQ                                                             
ACCOUNT  DC    C'ACCOUNT'                                                       
ACCSYS   DC    CL4'ACC'                                                         
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    CL8'ACCMST '                                                     
ACCARC   DC    CL8'ACCARC '                                                     
ACCRCV   DC    C'ACCRCV '                                                       
ACCREQ   DC    C'ACCREQ '                                                       
                                                                                
OPENLST  DS    0CL9                                                             
         DC    C'NACCDIR '         ACCOUNTING FILES TO OPEN                     
         DC    C'NACCMST '                                                      
         DC    C'NACCARC '                                                      
         DC    C'NACCRCV '                                                      
         DC    C'NCTFILE '                                                      
         DC    C'X'                                                             
                                                                                
YESLIT   DC    CL8'Y'                                                           
PONE     DC    PL1'1'                                                           
PTEN     DC    PL2'10'                                                          
PHNDRD   DC    PL2'100'                                                         
PZERO    DC    PL1'0'                                                           
ORDCNT   DC    CL6'000000'                                                      
SJUL     DC    CL2'SJ'                                                          
EXPLDGS  DS    0X                  TYPE=Expense ledgers                         
SEUL     DC    CL2'SE'                                                          
SAUL     DC    CL2'SA'                                                          
*&&UK                                                                           
SQUL     DC    CL2'SQ'                                                          
*&&                                                                             
*&&US                                                                           
SBUL     DC    CL2'SB'                                                          
*&&                                                                             
EXPLDGLQ EQU   *-EXPLDGS                                                        
         DC    X'FF'                                                            
                                                                                
*********************************************************************           
* Datasets                                                          *           
* MEACTV generates DDFA recovery tapes with a BLKSIZE of 8200       *           
* which is why I have defined them as that here.                    *           
*********************************************************************           
                                                                                
SININ    DCB   DDNAME=SINFIL,                                          *        
               DSORG=PS,                                               *        
               MACRF=(GM),                                             *        
               RECFM=FB,                                               *        
               EODAD=GETS8                                                      
                                                                                
SINOUT   DCB   DDNAME=SINFIL,                                          *        
               DSORG=PS,                                               *        
               MACRF=(PM),                                             *        
               RECFM=FB,                                               *        
               BLKSIZE=0,LRECL=256                                              
                                                                                
RCVIN    DCB   DDNAME=RCVTAPE,                                         *        
               DSORG=PS,                                               *        
               MACRF=(GM),                                             *        
               RECFM=VB,                                               *        
               EODAD=READR10                                                    
                                                                                
RCVOUT   DCB   DDNAME=RCVOUT,                                          *        
               DSORG=PS,                                               *        
               MACRF=(PM),                                             *        
               RECFM=VB,                                               *        
               BLKSIZE=0,LRECL=8200                                             
                                                                                
SNIPFILE DCB   DDNAME=SNIPFILE,                                        *        
               DSORG=PS,                                               *        
               MACRF=(GM),                                             *        
               RECFM=FB,                                               *        
               EODAD=SNIPSUPX                                                   
                                                                                
CARDTAB  DS    0H                                                               
         DC    C'DDSIO     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(DSIOVAL)           
         DC    C'DSPACE    ',AL1(5),AL1(0),AL1(0),AL4(DSPACE-WORKD)             
         DC    C'FLASH     ',AL1(4),AL1(0),AL1(CARDRTNQ),AL4(FLASH)             
         DC    C'WHICHSYS  ',AL1(7),AL1(0),AL1(CARDRTNQ),AL4(SYSVAL)            
         DC    C'INPUT     ',AL1(4),AL1(0),AL1(0),AL4(INPUT-WORKD)              
         DC    C'MODE      ',AL1(3),AL1(0),AL1(CARDRTNQ),AL4(MODVAL)            
                                                                                
* Recovery header key fields                                                    
         DC    C'GIN       ',AL1(2),AL1(0),AL1(CARDRTNQ),AL4(GINVAL)            
         DC    C'SIN       ',AL1(2),AL1(0),AL1(CARDRTNQ),AL4(SINVAL)            
         DC    C'STTIME    ',AL1(5),AL1(0),AL1(CARDRTNQ),AL4(STTMVAL)           
         DC    C'ENDTIME   ',AL1(6),AL1(0),AL1(CARDRTNQ),AL4(EDTMVAL)           
         DC    C'AGENCY    ',AL1(5),AL1(0),AL1(CARDRTNQ),AL4(AGYVAL)            
         DC    C'SNIP      ',AL1(3),AL1(0),AL1(0),AL4(SNIP-WORKD)               
                                                                                
* Record fields                                                                 
         DC    C'KEY       ',AL1(2),AL1(0),AL1(CARDRTNQ),AL4(KEYVAL)            
         DC    C'KY2       ',AL1(2),AL1(0),AL1(CARDRTNQ),AL4(KEYVAL)            
         DC    AL1(EOTQ)                                                        
                                                                                
* Convert File number/TRNS code into Datamanger command                         
ACTTAB   DC    XL1'41',AL1(1),AL2(DMREAD-GLOBALS)                               
ACTTABL  EQU   *-ACTTAB                                                         
         DC    XL1'41',AL1(2),AL2(DMWRT-GLOBALS)                                
         DC    XL1'41',AL1(3),AL2(DMADD-GLOBALS)                                
         DC    XL1'42',AL1(1),AL2(GETREC-GLOBALS)                               
         DC    XL1'42',AL1(2),AL2(PUTRECLT-GLOBALS)                             
         DC    XL1'42',AL1(3),AL2(ADDREC-GLOBALS)                               
         DC    XL1'43',AL1(1),AL2(REQUST-GLOBALS)                               
         DC    XL1'43',AL1(2),AL2(REQUST-GLOBALS)                               
         DC    XL1'43',AL1(3),AL2(REQUST-GLOBALS)                               
         DC    AL1(EOTQ)                                                        
                                                                                
TRLOWER  DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  00-0F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  10-1F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  20-2F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  30-3F                    
         DC    XL16'406F6F6F6F6F6F6F6F6F4A4B4C4D4E4F'  40-4F                    
         DC    XL16'506F6F6F6F6F6F6F6F6F5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60616F6F6F6F6F6F6F6F6A6B6C6D6E6F'  60-6F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F797A7B7C7D7E7F'  70-7F                    
         DC    XL16'6F8182838485868788896F6F6F6F6F6F'  80-8F                    
         DC    XL16'6F9192939495969798996F6F6F6F6F6F'  90-9F                    
         DC    XL16'6FA1A2A3A4A5A6A7A8A96F6F6F6F6F6F'  A0-AF                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C96F6F6F6F6F6F'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D96F6F6F6F6F6F'  D0-D1                    
         DC    XL16'E06FE2E3E4E5E6E7E8E96F6F6F6F6F6F'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F96F6F6F6F6F6F'  F0-FF                    
                                                                                
$$CODE   LOCTR ,                                                                
                                                                                
*********************************************************************           
* Equates.                                                          *           
*********************************************************************           
EOTQ    EQU    255                                                              
EORQ    EQU    0                                                                
YESQ    EQU    C'Y'                                                             
NOQ     EQU    C'N'                                                             
SPACEQ  EQU    C' '                                                             
SLASHQ  EQU    C'/'                                                             
                                                                                
ACCDQ   EQU    X'69'                                                            
ACCMQ   EQU    X'6A'                                                            
ACCAQ   EQU    X'6B'                                                            
ACCRQ   EQU    X'63'                                                            
*RCVFAACC EQU   X'61'               ACCFIL                                      
*RCVFAREQ EQU   X'63'               ACCREQ                                      
*RCVFADAY EQU   X'66'               ACCDAY                                      
*RCVFADIR EQU   X'69'               ACCDIR                                      
*RCVFAMST EQU   X'6A'               ACCMST                                      
*RCVFAARC EQU   X'6B'               ACCARC                                      
                                                                                
RC4Q    EQU    4                                                                
RC8Q    EQU    8                                                                
RC12Q   EQU    12                                                               
                                                                                
CARDTABD DSECT ,                                                                
CARDKEY  DS    CL10                                                             
CARDKXLN DS    XL1             Length of key                                    
CARDVXLN DS    XL1             Length of field to move directly                 
CARDIND  DS    XL1                                                              
CARDRTNQ EQU   X'80'           Call routine to validate field                   
CARDNUMQ EQU   X'40'           Number comes before the delimeter                
CARDVDSP DS    XL4             Routine to validate field                        
CARDTBLQ EQU   *-CARDTABD                                                       
                                                                                
RRECD    DSECT ,               ** RECOVERY RECORD **                            
RRECLN   DS    H               Recovery record length                           
         DS    H                                                                
RREC     DS    0X                                                               
       ++INCLUDE DMRCVRHDR                                                      
RRECORD  DS    2136X                                                            
RRECLEN  EQU   *-RRECD         Maximum recovery record length                   
                                                                                
RECVEXTD DSECT ,                                                                
       ++INCLUDE DMRCVREXT                                                      
                                                                                
* Request record mapping                                                        
REQD     DSECT ,                                                                
REQHDR   DS    XL80                                                             
REQUEST  DS    0CL80                                                            
REQCODE  DS    CL2                                                              
         DS    CL77                                                             
REQCONT  DS    CL1                                                              
REQCONTQ EQU   X'FF'                                                            
                                                                                
* SIN table mapping                                                             
SINTABD  DSECT ,                                                                
SINTIND  DS    XL1                 Record type or FF for EoT                    
SINTSOM  DS    CL1                                                              
SINTSTQ  EQU   C'S'                                                             
SINTMAQ  EQU   C'M'                                                             
SINTSIN  DS    XL4                                                              
SINTGIN  DS    XL8                                                              
SINTSLQ  EQU   *-SINTABD                                                        
SINTKEY  DS    XL42                (sequence # always zero)                     
SINTLNQ  EQU   *-SINTABD                                                        
SINTMAX  EQU   10000                                                            
SINTLEN  EQU   SINTLNQ*(SINTMAX+1)                                              
                                                                                
* Order/Estimate Sin/Gin  or unique key recovery table mapping                  
OERTABD  DSECT ,                                                                
OERTKEY  DS    0XL42               AUDKEY key or FF for EoT                     
OERTKOE  DS    CL1                                                              
OERTKUN  DS    XL13                                                             
OERTSPA  DS    XL28                                                             
OERTUID  DS    0XL4                                                             
OERTSIN  DS    XL4                                                              
OERTGIN  DS    XL8                                                              
OERTTIM  DS    XL4                                                              
OERTDAT  DS    XL6                                                              
OERTADD  DS    CL1                                                              
OERTLNQ  EQU   *-OERTABD                                                        
OERTMAX  EQU   20000                                                            
OERTLEN  EQU   OERTLNQ*(OERTMAX+1)                                              
                                                                                
* Time table to set RTIME from per SIN/GIN table mapping                        
TIMTABD  DSECT ,                                                                
TIMTSIN  DS    XL4                 RSIN  - SYSTEM INPUT NUMBER                  
TIMTGIN  DS    XL8                 RGIN  - GLOBAL INPUT TRANSACION ID           
TIMTDAT  DS    XL3                 RDATE - YMD BINARY                           
TIMTTIM  DS    XL4                 RTIME - TIME 0HHMMSSC                        
TIMTLNQ  EQU   *-TIMTABD                                                        
TIMTMAX  EQU   100000                                                           
TIMTLEN  EQU   TIMTLNQ*(TIMTMAX+4)                                              
                                                                                
WORKD    DSECT                 ** GLOBAL WORKING STORAGE **                     
                                                                                
DUB      DS    D                                                                
DUB2     DS    D                                                                
DUB3     DS    D                                                                
DUBT     DS    D                                                                
FULL     DS    F                                                                
SAVERE   DS    A                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
DMCB     DS    6F                                                               
DMWORK   DS    9F                                                               
WORK     DS    XL80                                                             
TEMP     DS    XL256                                                            
ELEM     DS    XL256                                                            
TRTAB    DS    XL256                                                            
VDATAMGR DS    V                                                                
VDATCON  DS    V                                                                
VDATVAL  DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VHELLO   DS    V                                                                
VCARDS   DS    V                                                                
VLOGIO   DS    V                                                                
VPRINTER DS    V                                                                
VPRNTBL  DS    V                                                                
VUTL     DS    V                                                                
VDDSIO   DS    V                                                                
VNUMVAL  DS    V                                                                
VLOADER  DS    V                                                                
VRECTYP  DS    V                                                                
VADDAY   DS    V                                                                
ATRKBUF  DS    A                                                                
ATRLOWER DS    A                                                                
ASSB     DS    A                                                                
                                                                                
DA       DS    F               Disk address                                     
RCV_SEQ  DS    F                                                                
RCV_UID  DS    F                                                                
                                                                                
TODL1Y   DS    XL2                                                              
                                                                                
RECIND   DS    XL1                                                              
RECIEND  EQU   X'08'                                                            
RECIBUF  EQU   X'04'                                                            
RECIBUT  EQU   X'02'                                                            
RECIBSF  EQU   X'01'                                                            
RECIBST  EQU   X'80'                                                            
RECIADD  EQU   X'40'                                                            
RECISTA  EQU   X'20'                                                            
RECIRES  EQU   X'10'                                                            
                                                                                
PROCIND  DS    XL1                                                              
PROCIEST EQU   X'80'                                                            
PROCIORD EQU   X'40'                                                            
PROCIMAI EQU   X'20'                                                            
                                                                                
OUTIND   DS    XL1                                                              
OUTIF_D  EQU   X'80'           From: deleted                                    
OUTIT_D  EQU   X'40'           To: deleted                                      
OUTIF_X  EQU   X'20'           From: exists                                     
OUTIT_X  EQU   X'10'           To: exists                                       
OUTIF_R  EQU   X'08'           From: restored                                   
OUTIT_R  EQU   X'04'           To: restored                                     
OUTIF_A  EQU   X'02'           From: add                                        
OUTIT_A  EQU   X'01'           To: add                                          
*                                                                               
OUTIND2  DS    XL1                                                              
OUTIF_E  EQU   X'80'           From: empty                                      
OUTIT_E  EQU   X'40'           To: empty                                        
                                                                                
RECTYP   DS    XL1                                                              
RECCPY   DS    XL1                                                              
RETCODE  DS    XL1                                                              
ANYCARDS DS    CL1                                                              
TRACE    DS    C               TRACE=Y(ES)/N(O)                                 
DSPACE   DS    XL1                                                              
RUNMODE  DS    CL1                                                              
RUNM_FLT EQU   C'F'                                                             
RUNM_MRG EQU   C'M'                                                             
INPUT    DS    CL1             INPUT=R(ECOVER)/T(APE)                           
SNIP     DS    CL1             SNIP using SNIPFILE                              
THISSEAL DS    CL2             Acc alpha code                                   
THISSENO DS    XL1             SE number                                        
                                                                                
* Filter fields                                                                 
FLTKEYL  DS    XL1             l' filter key                                    
FLTKEY   DS    XL42            Filter key                                       
FLTKEYC  DS    XL1             Filter key branch condition                      
FLTCPY   DS    CL2             Agency to filter on                              
FLTOCPY  DS    CL(FLTOMAX*2)   Other agencies                                   
FLTOMAX  EQU   10                                                               
FLTSTTM  DS    XL4             Start time                                       
FLTEDTM  DS    XL4             End time                                         
FLTSIN   DS    XL3             System input number                              
FLTGIN   DS    XL8             General input number                             
FLTRTYP  DS    XL1             Record type                                      
FLTRTYPC DS    XL1             Record type brand cond                           
                                                                                
CURSEQ   DS    XL1                                                              
CURSIN   DS    XL(L'RSIN)                                                       
CURGIN   DS    XL(L'RGIN)                                                       
CURSTE   DS    XL(SINTLNQ)                                                      
CURMKY   DS    XL(L'ACCKEY)                                                     
                                                                                
ESTIND   DS    XL1                 Estimate indicator                           
ESTIPRDM EQU   X'80'               - estimate main rec changed in =Prod         
ESTICHG  EQU   X'40'               - estimate change record                     
                                                                                
SAVDATA  DS    0XL100              O/E saved data                               
SAVIND   DS    XL1                 Indicator                                    
SAVIFSTM EQU   X'80'               - first time maintenance done                
SAVIFSTS EQU   X'40'               - first time status done                     
SAVIMAIQ EQU   X'20'               - maintenance scenario and/or                
SAVISTAQ EQU   X'10'               - status scenario                            
SAVISFPQ EQU   X'08'               - Seq 'from' processed                       
***SAVIADD  EQU   X'08'            - O/E is add                                 
SAVSEQ   DS    XL1                 Sequence number                              
SAVFMSIN DS    XL(SINTSLQ)         From SINTAB for Maintenance                  
SAVTMSIN DS    XL(SINTSLQ)         To SINTAB for Maintenance                    
SAVFSSIN DS    XL(SINTSLQ)         From SINTAB for Status                       
SAVTSSIN DS    XL(SINTSLQ)         To SINTAB for Status                         
SAVSTAT  DS    CL1                 Status Skip or Add                           
SAVSKIPQ EQU   C'X'                                                             
SAVADDQ  EQU   C'A'                                                             
SAVDETQ  EQU   L'RTIME+L'RPRG+L'RDATE                                           
SAVFMDET DS    XL(SAVDETQ)         From/Maintenance details                     
SAVTMDET DS    XL(SAVDETQ)         To/Maintenance details                       
SAVFSDET DS    XL(SAVDETQ)         From/Status details                          
SAVTSDET DS    XL(SAVDETQ)         To/Status details                            
         ORG   SAVDATA+L'SAVDATA                                                
                                                                                
COUNTOUT DS    PL8                                                              
COUNTALL DS    PL8                                                              
COUNTSIN DS    PL8                                                              
                                                                                
SV_SEQ   DS    XL1                                                              
SV_ENT   DS    0XL4                                                             
SV_ENT1  DS    XL2                                                              
SV_ENT2  DS    XL2                                                              
                                                                                
AGYLIST  DS    XL(AGYLMXQ*AGYLNQ+1)                                             
AGYLMXQ  EQU   20                                                               
AGYLNQ   EQU   4                                                                
PRTLINE  DS    CL80                                                             
                                                                                
KEY      DS    XL(L'ACTKEY)        KEY AREA                                     
         DS    XL(64-L'ACTKEY)                                                  
KEYSAVE  DS    XL64                KEY SAVE AREA                                
                                                                                
LASTRTY  DS    XL1                                                              
LASTKEY  DS    XL42                                                             
LASTMKY  DS    XL42                                                             
                                                                                
ASNIPTAB DS    A                                                                
SNIPTLNQ EQU   RHLENQ+ACCRLNK-ACCRECD                                           
SNIPTMXQ EQU   100                                                              
ABUFFR   DS    A                                                                
BUFFLNQ  EQU   IOAREALN            must match L'IOAREAn                         
ABUFTO   DS    A                                                                
BUFTLNQ  EQU   IOAREALN            must match L'IOAREAn                         
ABUSFR   DS    A                                                                
BUSFLNQ  EQU   IOAREALN            must match L'IOAREAn                         
ABUSTO   DS    A                                                                
BUSTLNQ  EQU   IOAREALN            must match L'IOAREAn                         
                                                                                
ASINTAB  DS    A                                                                
ASINTLN  DS    A                                                                
AOERTAB  DS    A                                                                
AOERTLN  DS    A                                                                
ATIMTAB  DS    A                                                                
ATIMTLN  DS    A                                                                
                                                                                
AIO1     DS    A                   A(I/O AREA1)                                 
IOWORK1  DS    XL96                                                             
IODA1    DS    XL4                                                              
AIO2     DS    A                   A(I/O AREA2)                                 
IOWORK2  DS    XL96                                                             
IODA2    DS    XL4                                                              
AIO3     DS    A                   A(I/O AREA3)                                 
IOWORK3  DS    XL96                                                             
IODA3    DS    F                                                                
                                                                                
IOAREALN EQU   RRECLEN+RXLENQ+8                                                 
IOAREA1  DS    (IOAREALN)X                                                      
IOAREA2  DS    (IOAREALN)X                                                      
IOAREA3  DS    (IOAREALN)X                                                      
                                                                                
WORKX    DS    0D                                                               
                                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
                                                                                
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
                                                                                
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
                                                                                
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
                                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
                                                                                
* ACRCVRECD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
         PRINT ON                                                               
                                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
                                                                                
* AGXTYPTABD                                                                    
         PRINT OFF                                                              
       ++INCLUDE AGXTYPTABD                                                     
         PRINT ON                                                               
                                                                                
CTLSTD   DSECT                                                                  
         ORG   CTLSTDTA                                                         
CTLSTNAM DS    CL7                 SE NAME                                      
CTLSTSYS DS    XL1                 CALLOV SYSTEM NUMBER                         
CTLSTSE  DS    XL1                 SE NUMBER                                    
         ORG                                                                    
                                                                                
CORE     CSECT                     ** OTHER LARGE AREAS **                      
TRKBUF   DS    (64*1024)X                                                       
                                                                                
MASTC    CSECT                                                                  
         DC    10000AL1(0)                                                      
                                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL **                   
         DS    (64*1024)X                                                       
                                                                                
SSB      CSECT                                                                  
         DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSOOFF)                                             
         DC    X'FF'               OFFLINE EXTENSION IN USE                     
         ORG   SSB+(SSOSTAT2-SSOOFF)                                            
         DC    AL1(0)                                                           
         ORG                                                                    
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017AGX5STUP  04/09/20'                                      
         END                                                                    
