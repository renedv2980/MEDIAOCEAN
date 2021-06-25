*          DATA SET ACFIL04    AT LEVEL 016 AS OF 08/28/20                      
*&&      SET   NOP=N                                                            
*PHASE T62304C                                                                  
         SPACE 1                                                                
ACFIL04  TITLE 'FILE ROUTINE PROGRAM LEVEL OBJECTS'                             
         SPACE 1                                                                
*---------------------------------------------------------------------*         
* USR  LVL TICKET     DESCRIPTION                                     *         
* ---  --- ------     -----------                                     *         
* RKEJ 016 SPEC-43417 Ability to use AEC codes on the 1R ledger       *         
*---------------------------------------------------------------------*         
ACFIL04  CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
ROUT     NMOD1 RTWORKL,**FL04**,R6,RR=R3,CLEAR=YES                              
         USING RTWORKD,RC                                                       
         ST    R1,RTPARMA                                                       
         MVC   RTPARMS,0(R1)                                                    
         ST    R3,RTRELO                                                        
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         LR    R2,RF                                                            
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CDLFLD-COMFACSD(RF)                                           
         ST    RF,VDLFLD                                                        
*                                                                               
         GOTO1 VDICTAT,RTPARM,C'LU  ',DCLIST,RTLISTU                            
*                                                                               
         USING FLTCDD,R2                                                        
         L     R2,=A(FLTCD)                                                     
         A     R2,RTRELO                                                        
         CLI   FLTNAME,X'40'       ALREADY DONE                                 
         BH    SETUP20                                                          
SETUP10  GOTO1 VDICTAT,BOPARM,C'SL  ',FLTNAME,0                                 
         LA    R2,FLTCLNQ(R2)                                                   
         CLI   0(R2),X'FF'                                                      
         BNE   SETUP10                                                          
*                                                                               
SETUP20  B     OBJECT                                                           
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT                                                           
         DC    12XL4'00'                                                        
*                                                                               
EXITL    MVI   BCDUB,0             SET CC LOW                                   
         B     EXITCC                                                           
EXITH    MVI   BCDUB,2             SET CC HIGH                                  
         B     EXITCC                                                           
EXITOK   MVI   BCDUB,1             SET CC EQUAL                                 
EXITCC   CLI   BCDUB,1                                                          
*                                                                               
EXIT     L     R1,RTPARMA          RETURN PARAMS TO CALLER                      
         MVC   0(L'RTPARMS,R1),RTPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* TABLE  ITERATION ROUTINE - EXPECTS RE TO HOLD EQUATED VERB          *         
*                          - EXPECTS RF TO HOLD A(TABLE)              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK              NOT KNOWN, BUT OVERRIDE ANYWAY               
         CLM   RE,1,OBJVERB        RE HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE TABLE                                
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   RE,1,OBJVERB        RE HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,RTRELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* OBJECT - PROVIDES INTERFACE TO OBJECTS                              *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   LA    R1,RTPARMS                                                       
         TM    0(R1),GCBOVER       CAN WE OVERRIDE THIS CALL?                   
         BZ    OO10                                                             
         OC    AOLY,AOLY           IF PFVAL FROM ACFIL00-AOLY WILL NOT          
         BZ    OO10                BE SET SO SKIP THIS                          
         ZIC   RE,RTPARMS+3                                                     
         CHI   RE,OPFK             IF PFKEY OBJECT                              
         BNE   OO05                AND PFVAL VERB THAN DON'T CALL               
         ZIC   RE,RTPARMS2+3       OVERLAY B/C PGM NEEDS AN EXITH               
         CHI   RE,PFVAL            TO CONTINUE.                                 
         BE    OO10                                                             
*                                                                               
OO05     L     RF,AOLY                                                          
         TM    0(R1),GCBPS         FILTERING?                                   
         BZ    *+8                                                              
         L     RF,APSOLY           PREVIOUS SESSION OVERLAY REQUIRED            
         BASR  RE,RF                                                            
         BNH   EXIT                OVERRIDDEN AT A LOWER LEVEL                  
*                                                                               
OO10     LA    R3,TABLEOO          OBJECTS KNOWN AT THIS LEVEL                  
         USING OBJTABD,R3                                                       
         L     RE,0(R1)                                                         
*                                                                               
OO15     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   RE,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    OO20                MATCHED                                      
         LA    R3,OBJTABL(R3)                                                   
         B     OO15                ITERATE KNOWN OBJECTS                        
*                                                                               
OO20     ICM   RF,15,OBJADR                                                     
         A     RF,RTRELO                                                        
         BR    RF                  INVOKE OBJECT                                
         DROP  R3                                                               
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(EXITOK)                                 
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITOK)                                
         DC    AL1(OACTH),AL1(0,0,0),AL4(ACT)                                   
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OOPT),AL1(0,0,0),AL4(OPT)                                    
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* ACTION HANDLER OBJECT                                               *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
ACT      L     RE,RTPARMS2                                                      
         LA    RF,ACTABL                                                        
         B     ITERH                                                            
*                                                                               
ACTABL   DC    AL1(ACTREP),AL1(0,0,0),AL4(ACTPRC)    REPORT                     
         DC    AL1(ACTDWN),AL1(0,0,0),AL4(ACTPRC)    DOWNLOAD                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* PROCESS ACTION                                                                
* THIS IS FOR THE REPORT/DOWNLOAD:                                              
* WHEN PFKEY IS PRESSED A NEW SESSION IS ENTERED.  THE FIRST PART OF            
* THIS CODE IS READING THE PROPER TEMPSTR PAGE TO RESTORE THE FILTERS           
* SINCE THEY WERE WIPED OUT FROM THE NEW SESSION.  THEN PROCESS THE             
* REPORT AND EXIT BACK TO PREVIOUS LEVEL TO KEEP THE LIST INTACT.               
***********************************************************************         
         SPACE 1                                                                
ACTPRC   DS    0H                                                               
         CLI   CSREC,R#APPRO      If not approver record then do                
         BE    EXITH              something else                                
         CLI   CSREC,R#LIML       If not limlist record then do                 
         BE    EXITH              something else                                
         CLI   CSREC,R#GRPL       If not limlist record then do                 
         BE    EXITH              something else                                
         CLI   CSREC,R#ETYPE      If not etype record then do                   
         BE    EXITH              something else                                
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,TWASESNL         GET CURRENT SESSION NESTING LEVEL            
         SH    RE,=H'1'            DECREMENT TO PREVIOUS LEVEL                  
         BP    *+6                                                              
         DC    H'0'                MINIMUM NEST LEVEL EXCEEDED                  
*                                                                               
         LA    RE,1(RE)                                                         
         SRDL  RE,1                /2 FOR ABSOLUTE TEMPSTR PAGE NUMBER          
         STC   RE,BCBYTE1          SAVE TEMPSTR PAGE NUMBER                     
         L     R2,ATIA                                                          
         LTR   RF,RF               USING SECOND HALF OF PAGE?                   
         BZ    *+8                 NO                                           
         AH    R2,=Y(TWAMAX/2)                                                  
         USING FESD,R2             R2=A(SESSION SAVE AREA)                      
*                                                                               
         ICM   RF,12,=C'L='        READ SAVED TEMPSTR PAGE                      
         ICM   RF,3,=Y(TWAMAX)                                                  
         GOTOX VDMGR,BCPARM,=CL8'DMREAD',=CL8'TEMPSTR',(BCBYTE1,0),    X        
               ATIA,,(RF)                                                       
         BE    *+6                                                              
         DC    H'0'                TEMPSTR READ UNHAPPY...                      
*                                                                               
         LA    R0,TWAD             RESTORE OVERLAY SAVE AREA                    
         AH    R0,=Y(TWSAVE-TWAD)                                               
         LA    R1,FESSAVEL                                                      
         LA    RE,FESSAVE                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,TWAD             RESTORE FILTER VALUES                        
         AH    R0,=Y(FLTELSV-TWAD)                                              
         LA    R1,FESFLSVL                                                      
         LA    RE,FESD                                                          
         AH    RE,=Y(FESFLSV-FESD)                                              
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   BCPFKEY,4           PFKEY 4 IS FOR REPORT                        
         BE    *+12                                                             
         CLI   CSACT,A#REPT        OR ACTION IS REPORT                          
         BNE   *+12                                                             
         OI    GENINDS,GENIREP                                                  
         B     *+8                                                              
         OI    GENINDS,GENIDLD     IF NOT EITHER MUST BE A DOWNLOAD             
         BAS   RE,DOREP                                                         
         NI    GENINDS,X'FF'-(GENIREP+GENIDLD)                                  
         XC    IODAOVER,IODAOVER   *THIS IS SET FOR SOME LEDGERS                
*                                                                               
         GOTOX AGEN,RTPARM,OSES,SXIT                                            
         DC    H'0'                SHOULDN'T GET BACK HERE?                     
         EJECT                                                                  
***********************************************************************         
* REPORT CODE                                                                   
***********************************************************************         
         SPACE 1                                                                
RROUTS   NTR1                                                                   
         LA    RF,REPROUTS                                                      
RROUT02  CLI   0(RF),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLM   R1,1,0(RF)                                                       
         BE    *+12                                                             
         LA    RF,L'REPROUTS(RF)                                                
         B     RROUT02                                                          
*                                                                               
         ICM   R7,15,1(RF)                                                      
         A     R7,RTRELO                                                        
         BR    R7                                                               
*                                                                               
REPROUTS DS    0XL5                                                             
         DC    AL1(RPQINIT),AL4(INITPQ)                                         
         DC    AL1(RPQOPEN),AL4(OPENPQ)                                         
         DC    AL1(DINTDLCB),AL4(INITDLCB)                                      
         DC    AL1(DCLSDLCB),AL4(CLSEDLCB)                                      
         DC    AL1(DBLDREP),AL4(BLDREP)                                         
         DC    AL1(DPQCLSE),AL4(PQCLSE)                                         
         DC    AL1(DGETFRST),AL4(GETFRST)                                       
         DC    AL1(DGETNEXT),AL4(GETNEXT)                                       
         DC    AL1(DDISLINE),AL4(RDISLINE)                                      
         DC    AL1(DSETCOLS),AL4(RCOLUMNS)                                      
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* CREATE REPORT                                                       *         
***********************************************************************         
         SPACE 1                                                                
DOREP    NTR1                                                                   
         GOTOX RROUTS,RPQINIT      INITIALIZE THE PQ                            
         GOTOX RROUTS,RPQOPEN      OPEN THE PRINT QUEUE                         
         TM    GENINDS,GENIDLD     DOING A DOWNLOAD?                            
         BZ    DOREP10                                                          
         GOTOX RROUTS,DINTDLCB     INITIALIZE THE DLCB                          
DOREP10  GOTOX RROUTS,DBLDREP      BUILD THE REPORT                             
         BL    DOREPXX             ERROR BUILDING REPORT/DOWNLOAD               
*                                                                               
         TM    GENINDS,GENIDLD     DOING A DOWNLOAD?                            
         BZ    DOREP20                                                          
         GOTOX RROUTS,DCLSDLCB     CLOSE THE DLCB                               
DOREP20  GOTOX RROUTS,DPQCLSE      CLOSE THE PRINT QUEUE                        
         CR    RB,RB               ENSURE CC EQUAL                              
*                                                                               
DOREPXX  B     EXIT                DONE.                                        
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINT QUEUE FOR REPORTING                                *         
*                                                                     *         
* IF PRINTING IS TO BE SOON OR OVERNIGHT, THE APPLICATION MUST SET    *         
* UP THE FOLLOWING FIELDS WITH THE CORRECT VALUES:                    *         
* INSYSID, INPRGID, INJCLID, INPRTY1, INPRTY2                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
INITPQ   L     R4,AREP                                                          
         USING REPD,R4                                                          
*                                                                               
         MVC   INDEST,CUUSER       SET DEFAULT DESTINATION                      
         MVI   INWIDTH,REPWREGQ    SET REGULAR REPORT WIDTH AS DEFAULT          
*                                                                               
         OI    REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPACTN,REPAINI     INITIALIZE REPBLK                            
         LA    R0,REPHS            SET A(REPORT BUFFER)                         
         ST    R0,REPABUF                                                       
         MVC   REPAPQB,ATIA        SET A(TIA)                                   
         MVC   REPACOM,ACOM        SET A(COMFACS)                               
         MVC   REPDATE,ASBDAT      SET TODAY'S DATE                             
         MVC   REPCTRY,CUCTRY      SET CONNECTED COUNTRY                        
         MVC   REPLANG,CULANG      SET CONNECTED LANGUAGE                       
         MVC   REPSYSID,INSYSID    SET SYSTEM ID                                
         MVC   REPPRGID,INPRGID    SET PROGRAM ID                               
         BRAS  RE,COLSEC           CHECK FOR SECURITY SENSITIVE COLUMNS         
*                                                                               
         TM    GENINDS,GENIREP     DOING A REPORT?                              
         BZ    INPQ02              NO DON'T NEED SPECS OR REP HOOK              
         L     R1,=A(SPECS)        MY SPECS                                     
         A     R1,RTRELO                                                        
         ST    R1,REPAPHS                                                       
         L     R1,=A(REPHOOK)      MY HOOK ROUTINE                              
         A     R1,RTRELO                                                        
         ST    R1,REPAUSR                                                       
         MVI   REPHEADH,1                                                       
         MVI   REPMIDSH,2                                                       
*                                                                               
INPQ02   MVC   REPWIDTH,INWIDTH    SET REGULAR REPORT WIDTH AS DEFAULT          
         MVI   REPHEADN,REPHN      REGULAR HEADLINES                            
         MVI   REPMIDSN,REPMN      REGULAR MIDLINES                             
         MVI   REPPRNTN,REPPN      REGULAR PRINTLINES                           
         MVI   REPFOOTN,REPFN      REGULAR FOOTLINES                            
*                                                                               
         TM    GENINDS,GENIREP           REPORT?                                
         BZ    INPQ10                                                           
         GOTOX AOLY,RTPARM,OREP,RPQINIT  ALLOW USER TO SET OVERRIDES            
         B     INPQ20                                                           
INPQ10   GOTOX AOLY,RTPARM,ODLOAD,DPQINIT  OVERRIDES FOR DOWNLOAD               
*                                                                               
INPQ20   CLI   REPWIDTH,REPWIDEQ   TEST WIDE LINE OVERRIDE                      
         BNE   *+20                                                             
         MVI   REPHEADN,REPHWN     WIDE HEADLINES                               
         MVI   REPMIDSN,REPMWN     WIDE MIDLINES                                
         MVI   REPPRNTN,REPPWN     WIDE PRINTLINES                              
         MVI   REPFOOTN,REPFWN     WIDE FOOTLINES                               
*                                                                               
* NOT SUPPORTING NARROW REPORTS RIGHT NOW                                       
*        CLI   REPWIDTH,REPWNARQ   TEST NARROW LINE OVERRIDE                    
*        BNE   *+20                                                             
*        MVI   REPHEADN,REPHNN     NARROW HEADLINES                             
*        MVI   REPMIDSN,REPMNN     NARROW MIDLINES                              
*        MVI   REPPRNTN,REPPNN     NARROW PRINTLINES                            
*        MVI   REPFOOTN,REPFNN     NARROW FOOTLINES                             
*                                                                               
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OPEN PRINT QUEUE FOR REPORTING                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
OPENPQ   L     R4,AREP                                                          
         USING REPD,R4                                                          
         MVI   REPACTN,REPAINI     INITIALIZE REPBLK                            
         GOTOX VREPORT,REPBLK      SHOULD INITIALISE REPORT                     
*                                                                               
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         GOTOX VREPORT,REPBLK                                                   
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
OPN06    B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE DLFLD FOR DOWNLOADING                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
INITDLCB LA    R1,DLCB             DOWNLOAD CONTROL BLOCK IN P3                 
         USING DLCBD,R1                                                         
         L     R4,AREP             BUILD DOWNLOAD CONTROL BLOCK                 
         USING REPD,R4                                                          
*                                                                               
* IF IT TURNS OUT YOU NEED TO SET THE WIDTH FOR THE DLOAD YOU MUST              
* PUT THIS IN THE OVERLAY INSTEAD OF HERE                                       
*                                                                               
*        MVI   REPWIDTH,REPWIDEQ   SET WIDE PRINTING                            
*        MVI   REPHEADN,REPHWN     WIDE HEADLINES                               
*        MVI   REPMIDSN,REPMWN     WIDE MIDLINES                                
*        MVI   REPPRNTN,REPPWN     WIDE PRINTLINES                              
*        MVI   REPFOOTN,REPFWN     WIDE FOOTLINES                               
*                                                                               
         XC    DLCBD(DLCBL),DLCBD                                               
         L     RF,=A(DLLINE)                                                    
         A     RF,RTRELO                                                        
         ST    RF,DLCBAPR          USER SUPPLIED PRINT ROUTINE                  
         LA    RF,REPP1                                                         
         ST    RF,DLCBAPL          USER SUPPLIED PRINT LINE                     
         MVI   DLCBACT,DLCBSOR     START OF REPORT                              
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN  EXTENDED TEXT FIELD USED             
         MVC   DLCXMAXL,=Y(L'REPPW1)                                            
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTOX VDLFLD              FIRST FOR REPORT                             
         B     EXITOK                                                           
         DROP  R1,R4                                                            
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* BUILD THE REPORT                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDREP   GOTOX RROUTS,DSETCOLS     SET UP REPORT HEADLINES                      
         BNE   EXITL                                                            
         CLI   CSREC,X'19'         ACCT RECORD?                                 
         BNE   *+12                                                             
         BRAS  RE,DISFVW           BUILD THE FILTER VIEW LINE                   
         BRAS  RE,GETACCF          GET THE ACCT CODE FILTER                     
         TM    GENINDS,GENIDLD     DOING A DOWNLOAD?                            
         BZ    *+8                                                              
         BRAS  RE,DLDFLT           DOWNLOAD THE FILTERS                         
         XC    LSLASKEY,LSLASKEY   CLEAR LAST AND INITIAL KEYS                  
         MVC   KEY,LSINIKEY                                                     
         GOTOX AGENLST,RTPARM,OLIST,LLSTFRST,KEY                                
         BL    EXITL                                                            
         MVC   LSLASKEY,KEY                                                     
*                                                                               
         GOTOX RROUTS,DGETFRST                                                  
         BNE   BLDR30                                                           
         B     BLDR20                                                           
*                                                                               
BLDR10   GOTOX RROUTS,DGETNEXT                                                  
         BNE   BLDR30                                                           
*                                                                               
BLDR20   GOTO1 RROUTS,DDISLINE     CALL DISPLAY VERBS TO EXTRACT DATA           
         B     BLDR10                                                           
*                                                                               
BLDR30   GOTOX AGENLST,RTPARM,OLIST,LLSTLAST                                    
         BL    EXITL                                                            
         OI    LSLTIND1,LSLTIEOL   END-OF-LIST FOUND                            
         B     EXITOK                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD REPORT HEADLINES                                                        
* READ FIELD RECORDS TO PRINT REPORT MIDLINES (COLUMN HEADINGS)                 
* ALSO BUILD A TABLE OF FIELD EQU'S + WIDTH/DISPLACEMENT INFO TO USE            
* LATER WHEN MOVING THE DATA INFO TO THE PRINT LINE.                            
***********************************************************************         
         SPACE 1                                                                
         USING DCTABD,R2                                                        
         USING *,R7                                                             
         USING REPD,R5                                                          
RCOLUMNS DS    0H                                                               
         TM    GENINDS,GENIDLD     DOING A DOWNLOAD?                            
         BZ    RCOL10                                                           
         BRAS  RE,DCOLUMNS         DOWNLOAD GETS A DIFFERENT ROUTINE            
         B     RCOLX                                                            
*                                                                               
RCOL10   L     R5,AREP                                                          
         XR    R0,R0                                                            
         ICM   R0,3,LSFIXNUM       NUMBER OF FIXED COLUMNS                      
         BZ    RCOL20              NO FIXED COLS(IS THIS POSSIBLE?)             
         STCM  R0,3,SVNUMCOL       SAVE # OF FIXED COLUMNS                      
         OI    BYTE,FIXCLM         PROCESSING FIXED COLUMNS                     
         LA    R2,LSFIXCLM                                                      
         B     RCOL30                                                           
RCOL20   ICM   R0,3,LSVARNUM       NUMBER OF VARIABLE COLUMNS                   
         BNZ   *+6                                                              
         DC    H'0'                NEED SOMETHING                               
         STCM  R0,3,SVNUMCOL       SAVE # OF VARIABLE COLUMNS                   
         LA    R2,LSVARCLM                                                      
*                                                                               
RCOL30   XC    HD1DISP,HD1DISP     DISPLACEMENT INTO 1ST HEADLINE               
         XC    HD2DISP,HD2DISP     DISPLACEMENT INTO 2ND HEADLINE               
         XC    TABDISP,TABDISP     DISPLACEMENT INTO FIELD TABLE                
         LA    R0,FLDTAB                                                        
         L     R1,=A(FLDEND-FLDTAB)                                             
         LR    RE,0                                                             
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
COL      USING FDRRECD,IOKEY                                                    
RCOL40   XC    COL.FDRKEY,COL.FDRKEY                                            
         MVI   COL.FDRKMIN,FDRKMINQ  BUILD FIELD RECORD KEY FOR COLUMN          
         MVI   COL.FDRKTYP,FDRKTYPQ                                             
         MVC   COL.FDRKSYS,GCOVSYS                                              
         MVC   COL.FDRKPRG,GCPRGNO                                              
         MVC   COL.FDRKREC,CSREC                                                
         MVC   COL.FDRKNUM,DCTFLD#   FIELD # FROM LAVARCLM/LSFIXCLM             
         MVC   COL.FDRKCTRY,CUCTRY                                              
         XI    COL.FDRKCTRY,FF                                                  
         MVI   COL.FDRKSUB,FF                                                   
         MVC   COL.FDRKTEST,ASTEST                                              
         GOTOX ('GETFLD',AGROUTS),BOPARM,GFREAD,AIO2                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO2                                                          
         LA    R3,FDRFIRST(R3)                                                  
         USING FDRELD,R3                                                        
         XR    RF,RF                                                            
RCOL50   CLI   FDREL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FDREL,FDRELQ                                                     
         BE    RCOL60                                                           
         IC    RF,FDRLN                                                         
         LA    R3,0(RF,R3)                                                      
         B     RCOL50                                                           
*                                                                               
RCOL60   BRAS  RE,ADDFLD           ADD FIELD INFO TO TABLE                      
         OC    FDRLHED1,FDRLHED1   FIRST HEADLINE?                              
         BZ    RCOL100                                                          
*                                                                               
*FDRLHED1/FDRLHED2 CONTAIN THE 4 BYTE DATA DICTIONARY ENTRY CONSISTING          
*OF THE FOLLOWING:                                                              
*1ST BYTE: ESCAPE SEQUENCE                                                      
*2ND & 3RD BYTES:  DDICT EQUATE NUMBER                                          
*4TH BYTE: LENGTH                                                               
*                                                                               
* PROCESS 1ST HEADLINE                                                          
*                                                                               
* LET OVERLAY OVERRIDE THE HEADLINES                                            
* RTWORK WILL CONTAIN THE 1ST HEADLINE OVERRIDE                                 
* BCWORK WILL CONTAIN THE 2ND HEADLINE OVERRIDE (AN UNDERLINE)                  
* BCBYTE1 WILL CONTAIN THE LENGTH OF 1ST HEADLINE                               
         MVC   RTWORK,BCSPACES                                                  
         MVC   BCWORK,BCSPACES                                                  
         GOTOX AOLY,RTPARM,ODLOAD,DSETCOLS,FVIFLD,FDRELD,RTWORK,BCWORK          
         CLC   RTWORK,BCSPACES     IS THERE AN COLUMN HEADING OVERRIDE          
         BE    RCOL70              FROM THE OVERLAY?                            
         ZIC   R1,BCBYTE1          YES - SAVE THE LENGTH OF THE                 
         STC   R1,SVHLEN           OVERRIDE AND MOVE THE OVERRIDE INTO          
         BCTR  R1,0                FVIFLD FOR PROCESSING                        
         EXMVC R1,FVIFLD,RTWORK                                                 
         B     RCOL80                                                           
*                                                                               
RCOL70   MVC   FVIFLD(L'FDRLHED1),FDRLHED1                                      
         MVI   FVIFLD,DD#ESCL      SET HEADLINE 1 LEFT ALIGNED                  
         MVC   SVHLEN,FVIFLD+3     SAVE THE LENGTH OF THE HEADLINE              
*                                                                               
         ICM   RF,15,=C'SL  '      RESOLVE FIRST HEADLINE                       
         ICM   RF,2,COL.FDRKSYS                                                 
         GOTOX VDICTAT,BOPARM,(RF),FVIFLD,0,0                                   
*                                                                               
RCOL80   LA    RF,REPM2            POINT TO MIDLINE 2                           
         CLI   REPWIDTH,REPWIDEQ   PRINTING WIDE?                               
         BNE   *+8                                                              
         LA    RF,REPMW1                                                        
         LR    R1,RF                                                            
         AH    RF,HD1DISP          ADD DISPLACEMENT.                            
         LR    R0,RF               BEFORE SETTING THE HEADLINE                  
         SR    R0,R1               MAKE SURE IT WILL FIT.  IF NOT               
         LA    RE,L'REPM2          TELL USER TO DOWNLOAD THE REPORT.            
         CLI   REPWIDTH,REPWIDEQ                                                
         BNE   *+8                                                              
         LA    RE,L'REPMW1                                                      
         CR    R0,RE                                                            
         BNL   RCOL90              HDLINE EXCEEDS REPORT WIDTH                  
         SR    RE,R0               MAKE SURE AT LEAST 20 BYTES IS LEFT          
         CHI   RE,20                                                            
         BNL   *+12                                                             
RCOL90   OI    GENINDS,GENWREP     SET BIT FOR OVERLAY TO PROCESS MSG           
         B     RCOLERR                                                          
*                                                                               
         ZIC   R1,SVHLEN           LENGTH OF HEADLINE                           
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),FVIFLD                                                  
         LA    RE,REPM2            BEGINNING OF MIDLINE 2                       
         CLI   REPWIDTH,REPWIDEQ   PRINTING WIDE?                               
         BNE   *+8                                                              
         LA    RE,REPMW1                                                        
         ZIC   R1,FDRLHLEN         COLUMN FIELD WIDTH                           
         AR    RF,R1               BUMP TO NEXT AVAILABLE SPOT                  
         AHI   RF,1                                                             
         SR    RF,RE                                                            
         STH   RF,HD1DISP          SAVE DISPLACEMENT TO NEXT SPOT               
*                                                                               
* PROCESS 2ND HEADLINE                                                          
RCOL100  CLC   BCWORK,BCSPACES     DID WE OVERRIDE THE 1ST COLUMN               
         BE    RCOL110             HEADING? NO                                  
         ZIC   R1,BCBYTE1          YES - MOVE IT INTO FVIFLD FOR                
         BCTR  R1,0                FOR PROCESSING                               
         EXMVC R1,FVIFLD,BCWORK                                                 
         B     RCOL130                                                          
*                                                                               
RCOL110  MVC   FVIFLD,BCSPACES                                                  
         OC    FDRLHED2,FDRLHED2   SECOND HEADLINE?                             
         BNZ   RCOL120                                                          
         LA    RF,REPM3            POINT TO MIDLINE 3                           
         CLI   REPWIDTH,REPWIDEQ   PRINTING WIDE?                               
         BNE   *+8                                                              
         LA    RF,REPMW2                                                        
         AH    RF,HD2DISP          ADD DISPLACEMENT                             
         B     RCOL140                                                          
*                                                                               
RCOL120  MVC   FVIFLD(L'FDRLHED2),FDRLHED2                                      
         MVI   FVIFLD,DD#ESCL      SET HEADLINE 2 LEFT ALIGNED                  
         CLI   FDRLHED2,DD#ESUL2   SECOND HEADLINE IS UNDERLINE?                
         BL    *+8                                                              
         MVI   FVIFLD,DD#ESUL      SET HDLINE 2 LEFT AND UNDERLINE              
         MVC   SVHLEN,FVIFLD+3     SAVE THE LENGTH OF THE HEADLINE              
*                                                                               
         ICM   RF,15,=C'SL  '      RESOLVE 2ND HEADLINE                         
         ICM   RF,2,COL.FDRKSYS                                                 
         GOTOX VDICTAT,BOPARM,(RF),FVIFLD,0,0                                   
*                                                                               
RCOL130  LA    RF,REPM3            BEGINNING OF MIDLINE 3                       
         CLI   REPWIDTH,REPWIDEQ   PRINTING WIDE?                               
         BNE   *+8                                                              
         LA    RF,REPMW2                                                        
         AH    RF,HD2DISP                                                       
         ZIC   R1,SVHLEN           LENGTH OF HEADLINE                           
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),FVIFLD                                                  
*                                                                               
RCOL140  LA    RE,REPM3            BEGINNING OF MIDLINE 3                       
         CLI   REPWIDTH,REPWIDEQ   PRINTING WIDE?                               
         BNE   *+8                                                              
         LA    RE,REPMW2                                                        
         ZIC   R1,FDRLHLEN         COLUMN FIELD WIDTH                           
         AR    RF,R1               HEADLINE FOR NEXT AVAILABLE ENTRY            
         AHI   RF,1                                                             
         SR    RF,RE                                                            
         STH   RF,HD2DISP          SAVE DISPLACEMENT TO NEXT SPOT               
*                                                                               
*                                                                               
         LA    R2,DCTABL(R2)       NEXT COLUMN FOR THIS LINE                    
         SR    R1,R1                                                            
         ICM   R1,3,SVNUMCOL                                                    
         BCTR  R1,0                                                             
         STCM  R1,3,SVNUMCOL                                                    
         LTR   R1,R1               # OF COLS LEFT TO PROCESS                    
         BNZ   RCOL40                                                           
         TM    BYTE,FIXCLM         DID WE JUST FINISH THE FIXED COLS?           
         BZ    RCOLX               NO SO DONE                                   
         NI    BYTE,X'FF'-FIXCLM                                                
         SR    R0,R0                                                            
         ICM   R0,3,LSVARNUM       NO VARIABLE COLUMNS (IS THIS                 
         BZ    RCOLX               POSSIBLE?)                                   
         STCM  R0,3,SVNUMCOL         SAVE # OF VARIABLE COLUMNS                 
         LA    R2,LSVARCLM                                                      
         B     RCOL40                                                           
*                                                                               
RCOLERR  B     EXITL                                                            
RCOLX    B     EXITOK                                                           
         DROP  R2,R3,COL                                                        
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SETUP TO GET FIRST RECORD                                                     
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
GETFRST  LA    R3,LGETFRST                                                      
         L     RF,=A(GET)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* SETUP TO GET NEXT RECORD                                                      
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
GETNEXT  GOTOX AOLY,RTPARM,OLIST,LGETFRST,KEY,LSLASKEY,XIO11                    
         LA    R3,LGETNEXT         RE-ESTABLISH SEQUENCE                        
         L     RF,=A(GET)                                                       
         A     RF,RTRELO                                                        
         BR    RF                                                               
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* CALL OVERLAY TO GET RECORD IN LIST ROUTINE                                    
* ON ENTRY - R3 POINTS TO THE ACTION LGETFRST/LGETNEXT                          
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
GET      LR    R7,RF                                                            
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         GOTOX AOLY,RTPARM,OLIST,(R3),KEY,LSLASKEY,XIO11                        
         BNE   EXITL                                                            
*                                                                               
GET02    DS    0H                                                               
         TM    LSSTAT1,LSSMAIN     MAINTENANCE TYPE LIST?                       
         BO    GET04               YES                                          
         CLI   CSREC,O#FLTR                                                     
         BE    GET04                                                            
         GOTOX AGEN,RTPARM,OFILT,FDOD,KEY                                       
         BE    GET04                                                            
         L     RF,=A(GETNEXT)      GOTO GETNEXT                                 
         A     RF,RTRELO                                                        
         LR    R7,RF                                                            
         BR    R7                                                               
*                                                                               
GET04    DS    0H                                                               
         LHI   R1,XOGET+XOACCMST+XIO11  GET THE RECORD                          
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
*                                                                               
GET06    TM    LSSTAT1,LSSMAIN     LISTING OTHER THAN RECORDS?                  
         BO    GETX                YES                                          
         GOTOX AGEN,RTPARM,OFILT,FDOR,AIOREC                                    
         BE    GETX                                                             
         L     RF,=A(GETNEXT)      GOTO GETNEXT                                 
         A     RF,RTRELO                                                        
         LR    R7,RF                                                            
         BR    R7                                                               
*                                                                               
GETX     DS    0H                                                               
         MVC   LSLASKEY,KEY        SAVE LAST KEY                                
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL DISPLAY VERBS (BASED ON LSFIXCLM/LSVARCLM TABLES) TO EXTRACT   *         
* RECORD INFORMATION INTO FVIFLD                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
         USING DCTABD,R2           GEFILDWN IN HERE                             
         USING REPD,R5                                                          
RDISLINE L     R5,AREP                                                          
         XC    BCBYTE2,BCBYTE2                                                  
         NI    BYTE,X'FF'-FIXCLM                                                
         XC    LINDISP,LINDISP     DISPLACEMENT INTO PRINT LINE                 
         MVI   SVNROWS,1           SET ROWS FOR THIS REC (ONE BASED)            
*                                                                               
         L     RF,AIOREC           A(RECORD)                                    
         TM    LSSTAT1,LSSTSAR     LINE IS ON TSAR RECORD ONLY?                 
         BZ    *+8                                                              
         L     RF,ATLST            A(TSAR RECORD)                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('ADRELQ',(RF)),0                  
         SR    R0,R0               R0 WILL HOLD COUNT OF ADDRESS LINES          
         CLI   12(R1),0                                                         
         BNE   RDLN02                                                           
         L     RE,12(,R1)                                                       
         USING ADRELD,RE                                                        
         ZIC   R0,ADRNUM          # OF ADDRESS LINES                            
         DROP  RE                                                               
*                                                                               
RDLN02   LTR   R0,R0               IF NO ADDRESS MUST SET AT 1 ROW              
         BNZ   *+8                                                              
         LHI   R0,1                                                             
         L     R1,ATLST                                                         
         USING TLSTD,R1                                                         
         STC   R0,TLROWS                                                        
         DROP  R1                                                               
*                                                                               
         GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DFIRST',DDIS),(RF),0           
*                                                                               
RDLN05   XR    R0,R0                                                            
         ICM   R0,3,LSFIXNUM       NUMBER OF FIXED COLUMNS                      
         BZ    RDLN10                                                           
         STCM  R0,3,SVNUMCOL       SAVE # OF FIXED COLS                         
         LA    R2,LSFIXCLM         POINT TO FIXED COLUMNS                       
         OI    BYTE,FIXCLM         PROCESSING FIXED COLUMNS                     
         B     RDLN20                                                           
*                                                                               
RDLN10   ICM   R0,3,LSVARNUM       NUMBER OF VARIABLE COLUMNS                   
         BNZ   *+6                                                              
         DC    H'0'                NEED SOMETHING                               
         STCM  R0,3,SVNUMCOL       SAVE # OF FIXED COLS                         
         LA    R2,LSVARCLM                                                      
*                                                                               
RDLN20   MVC   SVINDS1,DCTINDS1    SAVE COLUMN LIST INDICATOR BYTE              
         GOTOX AGEN,RTPARM,ODATA,DLDIS,DCTFLD#,AIOREC                           
         BL    EXITL                                                            
*                                                                               
         TM    GENINDS,GENIDLD     DOWNLOAD?                                    
         BZ    *+12                                                             
         BRAS  RE,DWLLIN           MOVE DATA INTO DOWNLOAD                      
         B     RDLN30                                                           
         BRAS  RE,RPLIN            MOVE DATA INTO PLINE FOR REPORT              
*                                                                               
RDLN30   LA    R2,DCTABL(R2)       NEXT COLUMN FOR THIS LINE                    
         SR    R1,R1                                                            
         ICM   R1,3,SVNUMCOL                                                    
         BCTR  R1,0                                                             
         STCM  R1,3,SVNUMCOL                                                    
         LTR   R1,R1               # OF COLS LEFT TO PROCESS                    
         BNZ   RDLN20                                                           
         TM    BYTE,FIXCLM         DID WE JUST FINISH THE FIXED COLS?           
         BZ    RDLN40              NO SO DONE                                   
         NI    BYTE,X'FF'-FIXCLM                                                
         SR    R0,R0                                                            
         ICM   R0,3,LSVARNUM       NO VARIABLE COLUMNS (IS THIS                 
         BZ    RDLN40              POSSIBLE?)                                   
         STCM  R0,3,SVNUMCOL         SAVE # OF VARIABLE COLUMNS                 
         LA    R2,LSVARCLM                                                      
         B     RDLN20                                                           
*                                                                               
RDLN40   TM    GENINDS,GENIREP                                                  
         BZ    RDLN45                                                           
         MVI   REPACTN,REPAPUT                                                  
         GOTOX VREPORT,REPBLK                                                   
         B     RDLN50                                                           
*                                                                               
         USING DLCBD,RF                                                         
RDLN45   LA    RF,DLCB                                                          
         MVI   DLCBACT,DLCBEOL     END OF HEADING TEXT LINE                     
         GOTOX VDLFLD,DLCBD                                                     
*                                                                               
         USING TLSTD,RF                                                         
RDLN50   L     RF,ATLST                                                         
         TM    LSSTAT3,LS3RVAR     VARIABLE ROWS ON LIST?                       
         BZ    RDLN60              NO THAN DONE                                 
         ZIC   R0,TLROWS          NUMBER OF ROWS FOR THIS RECORD                
         LTR   R0,R0                                                            
         BZ    RDLN60             ZERO MEANS ONLY ONE ROW SO FINISHED           
         ZIC   R1,SVNROWS         SVNROWS = CURRENT ROW PROCESSING              
         CR    R0,R1             COMPARE TO SEE IF ANY MORE ROWS NEEDED         
         BNH   RDLN60                                                           
         AHI   R1,1                                                             
         STC   R1,SVNROWS         BUMP UP ROW # AND GO AROUND AGAIN             
         XC    LINDISP,LINDISP                                                  
         B     RDLN05                                                           
*                                                                               
RDLN60   L     RF,AIOREC                                                        
         TM    LSSTAT1,LSSTSAR                                                  
         BZ    *+8                                                              
         L     RF,ATLST                                                         
         GOTOX AGEN,RTPARM,('GCBOVER',ODATA),0,('DLAST',DDIS),(RF),0            
*                                                                               
         B     EXITOK                                                           
         DROP  R2,R5,RF                                                         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CLOSE DLFLD FOR DOWNLOADING                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CLSEDLCB LA    R1,DLCB             DOWNLOAD CONTROL BLOCK IN P3                 
         USING DLCBD,R1                                                         
         MVI   DLCBACT,DLCBEOR                                                  
         GOTOX VDLFLD               LAST FOR REPORT                             
         B     EXITOK                                                           
         DROP  R1                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* CLOSE OR PURGE PRINT QUEUE NORMALLY                                 *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
PQCLSE   L     R5,AREP                                                          
         USING REPD,R5             R5=A(REPORT BLOCK)                           
         TM    REPIND1,REPIPUT     TEST ANY LINES PUT                           
         BNZ   PQCL02                                                           
         MVI   REPACTN,REPACLO     NO - CLOSE REPORT                            
         GOTOX VREPORT,REPBLK                                                   
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$NOREP)                                           
         B     EXITL                                                            
*                                                                               
PQCL02   MVI   REPACTN,REPACLO     CLOSE THE REPORT                             
         GOTOX VREPORT,REPBLK                                                   
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                CLOSE ERROR                                  
         B     EXITOK                                                           
         DROP  R5,R7                                                            
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR ZERO                            *         
* P3 BYTE  0   HOLDS GLOBAL ACTION IF P2 IS ZERO                      *         
* P3 BYTES 1-3 HOLD EQUATED VERB                                      *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   RE,15,4(R1)         RE HOLDS DATA IDENTIFIER                     
         BZ    EXITH                                                            
         LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA02   CLC   KNOWID,=AL2(EOT)    E.O.T                                        
         BE    EXITH               NOT KNOWN AT THIS LEVEL                      
         CLM   RE,3,KNOWID         MATCH ON DATA TYPE                           
         BE    DATA04                                                           
         LA    RF,KNOWLQ(RF)       ITERATE THE TABLE                            
         B     DATA02                                                           
*                                                                               
DATA04   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,RTRELO           RELOCATE IT                                  
         BASR  RE,RF                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OPTION OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OPT      L     RE,RTPARMS2                                                      
         LA    RF,OPTTABL                                                       
         B     ITER                OVERRIDE ANY UNKNOWN, ELSE DIES              
*                                                                               
OPTTABL  DC    AL1(OHLP),AL1(0,0,0),AL4(OPTHLP)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* TEST OPTIONS USED, SET HELP                                         *         
*                                                                     *         
* P3 HOLDS DEFAULT HELP NUMBER IN 1ST BYTE. CHANGE IF REQUIRED        *         
*                                                                     *         
* *TEMP* VERSION DOES WHAT OVERLAYS SHOULD DO. EACH OVERLAY SHOULD BE *         
*        CHANGED TO TAKE OVER THESE FUNCTIONS                         *         
***********************************************************************         
         SPACE 1                                                                
OPTHLP   LA    RF,=X'2100'                                                      
OPTHLP02 CLC   CSREC,0(RF)         RECORD USES OPTION FIELD IF IN TABLE         
         BE    EXITOK              USE RECORD HELP NUMBER                       
         CLI   0(RF),0                                                          
         BE    OPTHLPX             SKIP IF CSREC NOT IN TABLE                   
         LA    RF,1(,RF)                                                        
         B     OPTHLP02                                                         
*                                                                               
OPTHLPX  B     EXITL               USE DEFAULT HELP NUMBER                      
         EJECT ,                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(GEN#CMPY),AL4(CPYDTA)      COMPANY CODE                      
         DC    AL2(GEN#CMPYN),AL4(CPYNDTA)    COMPANY CODE NAME                 
         DC    AL2(GEN#NME),AL4(NAMDTA)       NAME                              
         DC    AL2(GEN#ADR1),AL4(ADRL1)       ADDRESS LINE 1                    
         DC    AL2(GEN#ADR2),AL4(ADRL2)       ADDRESS LINE 2                    
         DC    AL2(GEN#ADR3),AL4(ADRL3)       ADDRESS LINE 3                    
         DC    AL2(GEN#ADR4),AL4(ADRL4)       ADDRESS LINE 4                    
         DC    AL2(GEN#XADR1),AL4(XDRL1)      EXTRA ADDRESS LINE 1              
         DC    AL2(GEN#XADR2),AL4(XDRL2)      EXTRA ADDRESS LINE 2              
         DC    AL2(GEN#XADR3),AL4(XDRL3)      EXTRA ADDRESS LINE 3              
         DC    AL2(GEN#XADR4),AL4(XDRL4)      EXTRA ADDRESS LINE 4              
         DC    AL2(GEN#BADL1),AL4(BADL1)      BUSINESS ADDR LINE 1              
         DC    AL2(GEN#BADL2),AL4(BADL2)      BUSINESS ADDR LINE 2              
         DC    AL2(GEN#BCITY),AL4(BADCTY)     BUSINESS ADDR CITY                
         DC    AL2(GEN#BSTT),AL4(BADST)       BUSINESS ADDR STATE               
         DC    AL2(GEN#BZIP),AL4(BADZIP)      BUSINESS ADDR ZIP                 
         DC    AL2(GEN#BZIPR),AL4(BADZIPR)    BUSINESS ADDR ZIP ROUTING         
         DC    AL2(GEN#TELNO),AL4(TELNO)      TELEPHONE NUMBER                  
         DC    AL2(GEN#CMT1),AL4(COMML1)      COMMENT LINE 1                    
         DC    AL2(GEN#CMT2),AL4(COMML2)      COMMENT LINE 2                    
         DC    AL2(GEN#CMT3),AL4(COMML3)      COMMENT LINE 3                    
         DC    AL2(GEN#SECLV),AL4(SECLV)      SECURITY LEVEL                    
         DC    AL2(GEN#FLT1),AL4(RSTF1)       FILTER 1                          
         DC    AL2(GEN#FLT2),AL4(RSTF2)       FILTER 2                          
         DC    AL2(GEN#FLT3),AL4(RSTF3)       FILTER 3                          
         DC    AL2(GEN#FLT4),AL4(RSTF4)       FILTER 4                          
         DC    AL2(GEN#FLT5),AL4(RSTF5)       FILTER 5                          
         DC    AL2(GEN#FLTN1),AL4(RSTN1)      RSTFILT1 NAME                     
         DC    AL2(GEN#FLTN2),AL4(RSTN2)      RSTFILT2 NAME                     
         DC    AL2(GEN#FLTN3),AL4(RSTN3)      RSTFILT3 NAME                     
         DC    AL2(GEN#FLTN4),AL4(RSTN4)      RSTFILT4 NAME                     
         DC    AL2(GEN#FLTN5),AL4(RSTN5)      RSTFILT5 NAME                     
         DC    AL2(GEN#SHTNM),AL4(SHTNM)      SHORT NAME                        
         DC    AL2(GEN#FACC),AL4(FACCA)       FACTORING ACCOUNT                 
         DC    AL2(GEN#ONLM),AL4(XMEMO)       ONLINE MEMO ELEMENT               
         DC    AL2(GEN#UL),AL4(ULCDTA)        UNIT/LEDG CODE                    
         DC    AL2(GEN#ACC),AL4(ACCDTA)       ACCOUNT CODE                      
         DC    AL2(GEN#ACCLN),AL4(ACCLEN)     ACCOUNT LENGTHS                   
         DC    AL2(GEN#LDGNM),AL4(LGNDTA)     LEDGER NAME                       
         DC    AL2(GEN#DTADD),AL4(DTADD)      DATE ADDED (FOR LIST SCR)         
         DC    AL2(GEN#DTCHA),AL4(DTCHA)      DATE CHANGED-FOR LIST SCR         
         DC    AL2(ACC#FVIEW),AL4(FILTVW)     FLTR VIEW FLD ON LIST SCR         
*                                             OF ACCOUNT RECORD                 
         DC    AL2(F#GEN#WHO),AL4(WHODTA)     REQUESTOR                         
         DC    AL2(F#GEN#WHR),AL4(WHRDTA)     DESTINATION                       
         DC    AL2(F#GEN#WHN),AL4(WHNDTA)     OUTPUT TYPE(EG SOON)              
         DC    AL2(F#GEN#FEDU1),AL4(FEDU)     FEDERATED URL 1                   
         DC    AL2(F#GEN#FEDU2),AL4(FEDU)     FEDERATED URL 2                   
         DC    AL2(F#GEN#FEDU3),AL4(FEDU)     FEDERATED URL 3                   
         DC    AL2(F#GEN#FEDU4),AL4(FEDU)     FEDERATED URL 4                   
         SPACE 2                                                                
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
ACFIL04  CSECT                                                                  
         LTORG                                                                  
DCLIST   DS    0D                                                               
         DCDDL AC#DEL,L'AC@DEL,L     DELETE                                     
         DCDDL AC#ACCL,L'AC@ACCL,C   ACCOUNT LISTING                            
         DCDDL AC#ACCL,L'AC@ACCLU,CU ACCOUNT LISTING UNDERLINE                  
         DCDDL AC#FLT,L'AC@FLT,L     FILTER                                     
         DCDDL AC#CUID,L'AC@CUID,L   COMPANY USER ID                            
         DCDDL AC#NF19,L'AC@NF19,L                                              
         DCDDL AC#NFA94,L'AC@NFA94,L                                            
         DCDDL AC#NONE,L'AC@NONE,L                                              
         DCDDL AC#ACC,L'AC@ACC,L     ACCOUNT CODE                               
         DCDDL AC#NFA42,L'AC@NFA42   (None)                                     
         DCDDL AC#SOON,L'RU@SOON,L   SOON                                       
         DCDDL GE#OV,L'RU@OVNT,L     OVER NIGHT                                 
         DCDDL GE#NOW,L'RU@NOW,L     NOW                                        
                                                                                
DCLISTX  DC    X'00'                                                            
         DROP  RB,R6                                                            
         EJECT                                                                  
***********************************************************************         
* MACRO BRANCH TO DATA OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
         MACRO                                                                  
&NTRDO   NTRDO  &VBTAB,&ID,&LB                                                  
         AIF   (T'&ID NE 'O').L1                                                
         DC    CL8' '                                                           
         AGO  .L2                                                               
.L1      ANOP                                                                   
         DC    CL8'&ID'                                                         
.L2      ANOP                                                                   
         DS    0H                                                               
         USING *,RB                                                             
&NTRDO   NTR1  BASE=(RF)                                                        
         LM    R1,R3,8(R1)         R1 HOLDS VERB                                
         USING ACTRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         LA    R4,&VBTAB           TABLE OF KNOWN VERBS                         
         USING OBJTABD,R4                                                       
*                                                                               
&LB.03   CLI   OBJVERB,EOT         E.O.T.                                       
         BE    &LB.H                                                            
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    *+12                MATCHED                                      
         LA    R4,OBJTABL(R4)                                                   
         B     &LB.03              BUMP & LOOP                                  
*                                                                               
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         A     RF,RTRELO                                                        
         BR    RF                                                               
*                                                                               
&LB.FXL  MVI   RTPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     &LB.E                                                            
&LB.FXE  MVI   RTPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     &LB.E                                                            
&LB.FXH  MVI   RTPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     &LB.E                                                            
&LB.FXX  MVI   RTPARMS,DFLTX       EXIT DEFINATELY NOT VALID                    
         B     &LB.E                                                            
*                                                                               
&LB.L    MVI   RTBYTE1,0           SET CC LOW                                   
         B     &LB.CC                                                           
&LB.H    MVI   RTBYTE1,2           SET CC HIGH                                  
         B     *+8                                                              
&LB.E    MVI   RTBYTE1,1           SET CC EQUAL                                 
&LB.CC   CLI   RTBYTE1,1                                                        
*                                                                               
*                                                                               
&LB.X    XIT1                                                                   
         MEND                                                                   
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTKCPY                                             *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'ACTKCPY'                                                     
CPYDTA   NTRDO CPYTABL,CPYDTA,CPY                                               
*                                                                               
CPYTABL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPY)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISFCPY)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFCPY)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFCPY)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCPY)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFLTCPY)                               
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DFLFCPY)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY AN ACTKCPY FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISCPY   CLI   ACTKCPY,C' '                                                     
         BNH   CPYE                                                             
         GOTO1 VHEXOUT,RTPARM,ACTKCPY,FVIFLD,L'ACTKCPY,0                        
         B     CPYE                                                             
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE AN ACTKCPY FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALCPY   CLI   FVILEN,0                                                         
         BE    CPYE                                                             
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VHEXIN,RTPARM,FVIFLD,ACTKCPY,(RF)                                
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVCP) INVALID CPY CODE/ INVALID HEX             
         B     CPYL                                                             
*                                                                               
X        USING ACTRECD,IOKEY                                                    
         MVC   X.ACTKEY,BCSPACES                                                
         MVC   X.ACTKCPY,ACTKCPY                                                
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    CPYE                                                             
         MVC   FVMSGNO,=AL2(AE$INVCP) INVALID CPY CODE                          
         B     CPYL                                                             
***********************************************************************         
* DISPLAY AN ACTKCPY FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISFCPY  CLI   FLTIFLD,C' '                                                     
         BNH   CPYE                                                             
         GOTO1 VHEXOUT,RTPARM,FLTIFLD,FVIFLD,L'ACTKCPY,0                        
         B     CPYE                                                             
         SPACE 1                                                                
***********************************************************************         
* VALIDATE AN ACTKCPY FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VALFCPY  XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VHEXIN,RTPARM,FVIFLD,FLTIFLD,(RF)                                
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)  INVALID HEX                               
         B     CPYL                                                             
*                                                                               
         MVC   ACTKCPY,FLTIFLD                                                  
X        USING ACTRECD,IOKEY                                                    
         MVC   X.ACTKEY,BCSPACES                                                
         MVC   X.ACTKCPY,ACTKCPY                                                
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    CPYE                                                             
         MVC   FVMSGNO,=AL2(AE$INVCP) INVALID COMPANY CODE                      
         B     CPYL                                                             
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON COMPANY CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFCPY   CLC   ACTKCPY,FLTIFLD                                                  
         BL    CPYFXL                                                           
         BE    CPYFXE                                                           
         BH    CPYFXH                                                           
***********************************************************************         
* SET DEFAULT FOR COMPANY                                             *         
***********************************************************************         
         SPACE 1                                                                
DFLTCPY  GOTO1 VHEXOUT,BOPARM,CUABIN,FVIFLD,L'CUABIN,0                          
         B     CPYE                                                             
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR COMPANY FILTER                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLFCPY  GOTO1 VHEXOUT,RTPARM,CUABIN,FVIFLD,L'CUABIN,0                          
         B     CPYE                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPANY NAME                                        *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'CPY.NAM'                                                     
CPYNDTA  NTRDO CPYNTBL,CPYNDTA,CPYN                                             
*                                                                               
CPYNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPYN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A COMPANY NAME FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISCPYN  MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,ACTKCPY                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   CPYNE                                                            
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM                                                          
         B     CPYNE                                                            
         DROP  T                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR NAMELD                                              *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'NAMEREC'                                                     
NAMDTA   NTRDO NAMTABL,NAMDTA,NAM                                               
*                                                                               
NAMTABL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(DVALNAM)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISFNAM)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFNAM)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFNAM)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A NAME                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISNAM   LR    R1,R2               A(ACTRECD)                                   
         GOTOX AGETNAM                                                          
         B     NAME                                                             
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A NAME                                                     *         
***********************************************************************         
         SPACE 1                                                                
DVALNAM  GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('NAMELQ',ACTRECD),0               
         CLI   FVILEN,0            WAS A NAME INPUT?                            
         BE    NAME                NO                                           
         CLI   FVIFLD,C' '         1ST CHAR CAN'T BE A SPACE                    
         BE    NAML                                                             
*                                                                               
* USERS WOULD DELETE A RECORD IN =FILE BY RENAMING THE RECORD TO                
* 'DELETE'.  IN =NFILE THERE IS A DELETE ACTION SO WE DO NOT WANT               
* USERS TO RENAME A TON OF RECORDS TO DELETE ONLY TO REALIZE THAT               
* THEY ARE STILL ON THE FILE.                                                   
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,AC@DEL                                                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVNM)    INVALID NAME                           
         B     NAML                                                             
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         LA    R3,BOWORK2          BUILD A NAMEL                                
         USING NAMELD,R3                                                        
         MVI   NAMEL,NAMELQ                                                     
         IC    RE,FVILEN                                                        
         LA    RE,NAMLN1Q(RE)                                                   
         STC   RE,NAMLN                                                         
         MVC   NAMEREC,FVIFLD                                                   
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),(R2),NAMELD                        
         CLI   12(R1),0                                                         
         BE    NAME                                                             
         DC    H'0'                                                             
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPALY A NAME FILTER                                               *         
* THE LENGTH (TRUE LENGTH NOT EXECUTED LENGTH)IS CONTAINED IN THE 1ST *         
* BYTE OF FLTIFLD.                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISFNAM  MVC   FVIFLD(L'FVIFLD-1),FLTIFLD+1                                     
         B     NAME                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE NAME FILTER                                            *         
* THE LENGTH (TRUE LENGTH NOT EXECUTED LENGTH)IS CONTAINED IN THE 1ST *         
* BYTE OF FLTIFLD.                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALFNAM  ZIC   R1,FVILEN                                                        
         STC   R1,FLTIFLD         STORE LENGTH                                  
         EXMVC R1,FLTIFLD+1,FVIFLD                                              
         B     NAME                                                             
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON ACCOUNT NAME                                        *         
* THE LENGTH (TRUE LENGTH NOT EXECUTED LENGTH)IS CONTAINED IN THE 1ST *         
* BYTE OF FLTIFLD.                                                    *         
***********************************************************************         
         SPACE 1                                                                
DOFNAM   GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('NAMELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   NAME                                                             
*                                                                               
         L     R1,12(R1)                                                        
         USING NAMELD,R1                                                        
         LA    RE,NAMEREC        RE POINTS TO NAME STRING IN ELEMENT            
         ZIC   RF,NAMLN          RF CONTAINS LENGTH OF ELEM STRING FLD          
         SH    RF,=Y(NAMLN1Q)                                                   
         DROP  R1                                                               
         LA    R2,FLTIFLD+1      R2 POINTS TO FILTER STRING                     
         LA    R3,L'FLTIFLD-1    R3 POINTS TO LEN OF FILTER STRING FLD          
         ZIC   R0,FLTIFLD        R0 CONTAINS LENGTH OF FILTER STRING            
         LA    R1,X'40'          SET PADDING CHARACTER                          
         CUSE  RE,R2                                                            
         BE    NAMFXE                                                           
         B     NAMFXL                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE 1                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'ADREL,L1'                                                    
ADRL1    NTRDO ADR1TABL,ADRL1,ADR1                                              
*                                                                               
ADR1TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISADR1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADR1)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY ADDRESS LINE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
DISADR1  GOTO1 AGETADR,RTPARM,(1,ACTRECD)                                       
         BRAS  RE,CKLOPRO                                                       
         B     ADR1E                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADDRESS LINE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
VALADR1  LA    R4,1                                                             
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         LA    R3,BOWORK1                                                       
         USING ADRELD,R3                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('ADRELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   VAD102                                                           
         L     RF,12(R1)                                                        
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ADRELD(0),0(RF)                                                  
         GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('ADRELQ',ACTRECD),0               
*                                                                               
VAD102   MVI   ADREL,ADRELQ                                                     
         MVI   ADRLN,ADRLN4Q                                                    
         MVI   ADRNUM,4                                                         
*                                                                               
         BCTR  R4,0                                                             
         MH    R4,=Y(L'ADRADD1)                                                 
         LA    R4,ADRADD1(R4)                                                   
         MVC   0(L'ADRADD1,R4),FVIFLD                                           
*                                                                               
         LA    R0,4                FIND HOW MANY LINES IN ADDRESS               
         LA    R4,ADRELD+ADRLN4Q                                                
         USING ADRADD1,R4                                                       
         LA    RE,L'ADRADD1                                                     
VAD104   SR    R4,RE                                                            
         OC    ADRADD1,ADRADD1                                                  
         BNZ   *+10                                                             
         MVC   ADRADD1,BCSPACES                                                 
         CLC   ADRADD1,BCSPACES                                                 
         BNE   VAD106                                                           
         BCT   R0,VAD104                                                        
         B     ADR1E               NO ADDRESS                                   
         DROP  R4                                                               
*                                                                               
VAD106   STC   R0,ADRNUM                                                        
         AR    R4,RE                                                            
         SR    R4,R3                                                            
         STC   R4,ADRLN                                                         
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,ADRELD                     
         CLI   12(R1),0                                                         
         B     ADR1X                                                            
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE 2                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'ADREL,L2'                                                    
ADRL2    NTRDO ADR2TABL,ADRL2,ADR2                                              
*                                                                               
ADR2TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISADR2)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADR2)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY ADDRESS LINE 2                                              *         
***********************************************************************         
         SPACE 1                                                                
DISADR2  GOTO1 AGETADR,RTPARM,(2,ACTRECD)                                       
         BRAS  RE,CKLOPRO                                                       
         B     ADR2E                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADDRESS LINE 2                                             *         
***********************************************************************         
         SPACE 1                                                                
VALADR2  LA    R4,2                                                             
         LA    R3,BOWORK1                                                       
         USING ADRELD,R3                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('ADRELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   VAD202                                                           
         L     RF,12(R1)                                                        
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ADRELD(0),0(RF)                                                  
         GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('ADRELQ',ACTRECD),0               
*                                                                               
VAD202   MVI   ADREL,ADRELQ                                                     
         MVI   ADRLN,ADRLN4Q                                                    
         MVI   ADRNUM,4                                                         
*                                                                               
         BCTR  R4,0                                                             
         MH    R4,=Y(L'ADRADD1)                                                 
         LA    R4,ADRADD1(R4)                                                   
         MVC   0(L'ADRADD1,R4),FVIFLD                                           
*                                                                               
         LA    R0,4                FIND HOW MANY LINES IN ADDRESS               
         LA    R4,ADRELD+ADRLN4Q                                                
         USING ADRADD1,R4                                                       
         LA    RE,L'ADRADD1                                                     
VAD204   SR    R4,RE                                                            
         OC    ADRADD1,ADRADD1                                                  
         BNZ   *+10                                                             
         MVC   ADRADD1,BCSPACES                                                 
         CLC   ADRADD1,BCSPACES                                                 
         BNE   VAD206                                                           
         BCT   R0,VAD204                                                        
         B     ADR2E               NO ADDRESS                                   
         DROP  R4                                                               
*                                                                               
VAD206   STC   R0,ADRNUM                                                        
         AR    R4,RE                                                            
         SR    R4,R3                                                            
         STC   R4,ADRLN                                                         
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,ADRELD                     
         CLI   12(R1),0                                                         
         B     ADR2X                                                            
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE 3                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'ADREL,L3'                                                    
ADRL3    NTRDO ADR3TABL,ADRL3,ADR3                                              
*                                                                               
ADR3TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISADR3)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADR3)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY ADDRESS LINE 3                                              *         
***********************************************************************         
         SPACE 1                                                                
DISADR3  GOTO1 AGETADR,RTPARM,(3,ACTRECD)                                       
         BRAS  RE,CKLOPRO                                                       
         B     ADR3E                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADDRESS LINE 3                                             *         
***********************************************************************         
         SPACE 1                                                                
VALADR3  LA    R4,3                                                             
         LA    R3,BOWORK1                                                       
         USING ADRELD,R3                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('ADRELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   VAD302                                                           
         L     RF,12(R1)                                                        
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ADRELD(0),0(RF)                                                  
         GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('ADRELQ',ACTRECD),0               
*                                                                               
VAD302   MVI   ADREL,ADRELQ                                                     
         MVI   ADRLN,ADRLN4Q                                                    
         MVI   ADRNUM,4                                                         
*                                                                               
         BCTR  R4,0                                                             
         MH    R4,=Y(L'ADRADD1)                                                 
         LA    R4,ADRADD1(R4)                                                   
         MVC   0(L'ADRADD1,R4),FVIFLD                                           
*                                                                               
         LA    R0,4                FIND HOW MANY LINES IN ADDRESS               
         LA    R4,ADRELD+ADRLN4Q                                                
         USING ADRADD1,R4                                                       
         LA    RE,L'ADRADD1                                                     
VAD304   SR    R4,RE                                                            
         OC    ADRADD1,ADRADD1                                                  
         BNZ   *+10                                                             
         MVC   ADRADD1,BCSPACES                                                 
         CLC   ADRADD1,BCSPACES                                                 
         BNE   VAD306                                                           
         BCT   R0,VAD304                                                        
         B     ADR3E               NO ADDRESS                                   
         DROP  R4                                                               
*                                                                               
VAD306   STC   R0,ADRNUM                                                        
         AR    R4,RE                                                            
         SR    R4,R3                                                            
         STC   R4,ADRLN                                                         
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,ADRELD                     
         CLI   12(R1),0                                                         
         B     ADR3X                                                            
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE 4                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'ADREL,L4'                                                    
ADRL4    NTRDO ADR4TABL,ADRL4,ADR4                                              
*                                                                               
ADR4TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISADR4)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADR4)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY ADDRESS LINE 4                                              *         
***********************************************************************         
         SPACE 1                                                                
DISADR4  GOTO1 AGETADR,RTPARM,(4,ACTRECD)                                       
         BRAS  RE,CKLOPRO                                                       
         B     ADR4E                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADDRESS LINE 4                                             *         
***********************************************************************         
         SPACE 1                                                                
VALADR4  LA    R4,4                                                             
         LA    R3,BOWORK1                                                       
         USING ADRELD,R3                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('ADRELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   VAD402                                                           
         L     RF,12(R1)                                                        
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ADRELD(0),0(RF)                                                  
         GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('ADRELQ',ACTRECD),0               
*                                                                               
VAD402   MVI   ADREL,ADRELQ                                                     
         MVI   ADRLN,ADRLN4Q                                                    
         MVI   ADRNUM,4                                                         
*                                                                               
         BCTR  R4,0                                                             
         MH    R4,=Y(L'ADRADD1)                                                 
         LA    R4,ADRADD1(R4)                                                   
         MVC   0(L'ADRADD1,R4),FVIFLD                                           
*                                                                               
         LA    R0,4                FIND HOW MANY LINES IN ADDRESS               
         LA    R4,ADRELD+ADRLN4Q                                                
         USING ADRADD1,R4                                                       
         LA    RE,L'ADRADD1                                                     
VAD404   SR    R4,RE                                                            
         OC    ADRADD1,ADRADD1                                                  
         BNZ   *+10                                                             
         MVC   ADRADD1,BCSPACES                                                 
         CLC   ADRADD1,BCSPACES                                                 
         BNE   VAD406                                                           
         BCT   R0,VAD404                                                        
         B     ADR4E               NO ADDRESS                                   
         DROP  R4                                                               
*                                                                               
VAD406   STC   R0,ADRNUM                                                        
         AR    R4,RE                                                            
         SR    R4,R3                                                            
         STC   R4,ADRLN                                                         
*                                                                               
         USING TLSTD,RF                                                         
         L     RF,ATLST            UPDATE TSAR RECORD WITH CORRECT              
         LTR   R0,R0               # OF ROWS (FOR LIST SCREEN)                  
         BNZ   *+8                                                              
         LHI   R0,1                                                             
         STC   R0,TLROWS                                                        
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,ADRELD                     
         CLI   12(R1),0                                                         
         B     ADR4X                                                            
*                                                                               
         DROP  R3,RF                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EXTRA ADDRESS LINE 1                                *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OADEL,L1'                                                    
XDRL1    NTRDO XDR1TABL,XDRL1,XDR1                                              
*                                                                               
XDR1TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISXDR1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXDR1)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY EXTRA ADDRESS LINE 1                                        *         
***********************************************************************         
         SPACE 1                                                                
DISXDR1  GOTO1 AGETADR,RTPARM,(X'81',ACTRECD)                                   
         B     XDR1X                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTRA ADDRESS LINE 1                                       *         
***********************************************************************         
         SPACE 1                                                                
VALXDR1  LA    R4,1                                                             
         LA    R3,BOWORK1                                                       
         USING ADRELD,R3                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OADELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   VXA102                                                           
         L     RF,12(R1)                                                        
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ADRELD(0),0(RF)                                                  
         GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('OADELQ',ACTRECD),0               
*                                                                               
VXA102   MVI   ADREL,OADELQ                                                     
         MVI   ADRLN,ADRLN4Q                                                    
         MVI   ADRNUM,4                                                         
*                                                                               
         BCTR  R4,0                                                             
         MH    R4,=Y(L'ADRADD1)                                                 
         LA    R4,ADRADD1(R4)                                                   
         MVC   0(L'ADRADD1,R4),FVIFLD                                           
*                                                                               
         LA    R0,4                FIND HOW MANY LINES IN ADDRESS               
         LA    R4,ADRELD+ADRLN4Q                                                
         USING ADRADD1,R4                                                       
         LA    RE,L'ADRADD1                                                     
VXA104   SR    R4,RE                                                            
         OC    ADRADD1,ADRADD1                                                  
         BNZ   *+10                                                             
         MVC   ADRADD1,BCSPACES                                                 
         CLC   ADRADD1,BCSPACES                                                 
         BNE   VXA106                                                           
         BCT   R0,VXA104                                                        
         B     XDR1E               NO ADDRESS                                   
         DROP  R4                                                               
*                                                                               
VXA106   STC   R0,ADRNUM                                                        
         AR    R4,RE                                                            
         SR    R4,R3                                                            
         STC   R4,ADRLN                                                         
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,ADRELD                     
         CLI   12(R1),0                                                         
         B     XDR1X                                                            
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EXTRA ADDRESS LINE 2                                *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OADEL,L2'                                                    
XDRL2    NTRDO XDR2TABL,XDRL2,XDR2                                              
*                                                                               
XDR2TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISXDR2)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXDR2)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY EXTRA ADDRESS LINE 2                                        *         
***********************************************************************         
         SPACE 1                                                                
DISXDR2  GOTO1 AGETADR,RTPARM,(X'82',ACTRECD)                                   
         B     XDR2X                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTRA ADDRESS LINE 2                                       *         
***********************************************************************         
         SPACE 1                                                                
VALXDR2  LA    R4,2                                                             
         LA    R3,BOWORK1                                                       
         USING ADRELD,R3                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OADELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   VXA202                                                           
         L     RF,12(R1)                                                        
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ADRELD(0),0(RF)                                                  
         GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('OADELQ',ACTRECD),0               
*                                                                               
VXA202   MVI   ADREL,OADELQ                                                     
         MVI   ADRLN,ADRLN4Q                                                    
         MVI   ADRNUM,4                                                         
*                                                                               
         BCTR  R4,0                                                             
         MH    R4,=Y(L'ADRADD1)                                                 
         LA    R4,ADRADD1(R4)                                                   
         MVC   0(L'ADRADD1,R4),FVIFLD                                           
*                                                                               
         LA    R0,4                FIND HOW MANY LINES IN ADDRESS               
         LA    R4,ADRELD+ADRLN4Q                                                
         USING ADRADD1,R4                                                       
         LA    RE,L'ADRADD1                                                     
VXA204   SR    R4,RE                                                            
         OC    ADRADD1,ADRADD1                                                  
         BNZ   *+10                                                             
         MVC   ADRADD1,BCSPACES                                                 
         CLC   ADRADD1,BCSPACES                                                 
         BNE   VXA206                                                           
         BCT   R0,VXA204                                                        
         B     XDR2E               NO ADDRESS                                   
         DROP  R4                                                               
*                                                                               
VXA206   STC   R0,ADRNUM                                                        
         AR    R4,RE                                                            
         SR    R4,R3                                                            
         STC   R4,ADRLN                                                         
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,ADRELD                     
         CLI   12(R1),0                                                         
         B     XDR2X                                                            
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EXTRA ADDRESS LINE 3                                *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OADEL,L3'                                                    
         USING *,RB                                                             
XDRL3    NTRDO XDR3TABL,XDRL3,XDR3                                              
*                                                                               
XDR3TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISXDR3)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXDR3)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY EXTRA ADDRESS LINE 3                                        *         
***********************************************************************         
         SPACE 1                                                                
DISXDR3  GOTO1 AGETADR,RTPARM,(X'83',ACTRECD)                                   
         B     XDR3X                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTRA ADDRESS LINE 3                                       *         
***********************************************************************         
         SPACE 1                                                                
VALXDR3  LA    R4,3                                                             
         LA    R3,BOWORK1                                                       
         USING ADRELD,R3                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OADELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   VXA302                                                           
         L     RF,12(R1)                                                        
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ADRELD(0),0(RF)                                                  
         GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('OADELQ',ACTRECD),0               
*                                                                               
VXA302   MVI   ADREL,OADELQ                                                     
         MVI   ADRLN,ADRLN4Q                                                    
         MVI   ADRNUM,4                                                         
*                                                                               
         BCTR  R4,0                                                             
         MH    R4,=Y(L'ADRADD1)                                                 
         LA    R4,ADRADD1(R4)                                                   
         MVC   0(L'ADRADD1,R4),FVIFLD                                           
*                                                                               
         LA    R0,4                FIND HOW MANY LINES IN ADDRESS               
         LA    R4,ADRELD+ADRLN4Q                                                
         USING ADRADD1,R4                                                       
         LA    RE,L'ADRADD1                                                     
VXA304   SR    R4,RE                                                            
         OC    ADRADD1,ADRADD1                                                  
         BNZ   *+10                                                             
         MVC   ADRADD1,BCSPACES                                                 
         CLC   ADRADD1,BCSPACES                                                 
         BNE   VXA306                                                           
         BCT   R0,VXA304                                                        
         B     XDR3E               NO ADDRESS                                   
         DROP  R4                                                               
*                                                                               
VXA306   STC   R0,ADRNUM                                                        
         AR    R4,RE                                                            
         SR    R4,R3                                                            
         STC   R4,ADRLN                                                         
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,ADRELD                     
         CLI   12(R1),0                                                         
         B     XDR3X                                                            
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EXTRA ADDRESS LINE 4                                *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OADEL,L4'                                                    
XDRL4    NTRDO XDR4TABL,XDRL4,XDR4                                              
*                                                                               
XDR4TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISXDR4)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXDR4)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY EXTRA ADDRESS LINE 4                                        *         
***********************************************************************         
         SPACE 1                                                                
DISXDR4  GOTO1 AGETADR,RTPARM,(X'84',ACTRECD)                                   
         B     XDR4X                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXTRA ADDRESS LINE 4                                       *         
***********************************************************************         
         SPACE 1                                                                
VALXDR4  LA    R4,4                                                             
         LA    R3,BOWORK1                                                       
         USING ADRELD,R3                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OADELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   VXA402                                                           
         L     RF,12(R1)                                                        
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ADRELD(0),0(RF)                                                  
         GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('OADELQ',ACTRECD),0               
*                                                                               
VXA402   MVI   ADREL,OADELQ                                                     
         MVI   ADRLN,ADRLN4Q                                                    
         MVI   ADRNUM,4                                                         
*                                                                               
         BCTR  R4,0                                                             
         MH    R4,=Y(L'ADRADD1)                                                 
         LA    R4,ADRADD1(R4)                                                   
         MVC   0(L'ADRADD1,R4),FVIFLD                                           
*                                                                               
         LA    R0,4                FIND HOW MANY LINES IN ADDRESS               
         LA    R4,ADRELD+ADRLN4Q                                                
         USING ADRADD1,R4                                                       
         LA    RE,L'ADRADD1                                                     
VXA404   SR    R4,RE                                                            
         OC    ADRADD1,ADRADD1                                                  
         BNZ   *+10                                                             
         MVC   ADRADD1,BCSPACES                                                 
         CLC   ADRADD1,BCSPACES                                                 
         BNE   VXA406                                                           
         BCT   R0,VXA404                                                        
         B     XDR4E               NO ADDRESS                                   
         DROP  R4                                                               
*                                                                               
VXA406   STC   R0,ADRNUM                                                        
         AR    R4,RE                                                            
         SR    R4,R3                                                            
         STC   R4,ADRLN                                                         
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,ADRELD                     
         CLI   12(R1),0                                                         
         B     XDR4X                                                            
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BUSINESS ADDRESS LINE 1                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OATEL,L1'                                                    
BADL1    NTRDO BAD1TABL,BADL1,BAD1                                              
*                                                                               
BAD1TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DBADL1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VBADL1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BUSINESS ADDR LINE 1                                        *         
***********************************************************************         
         SPACE 1                                                                
DBADL1   MVC   BOELEM,BCSPACES                                                  
         LA    R3,BOELEM                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OATELQ',ACTRECD),       C        
               (L'OATSUB,=AL1(OATSUB5Q))                                        
         CLI   12(R1),0                                                         
         BNE   BAD1E                                                            
         USING OATELD,RF                                                        
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'OATLINE1),OATLINE1                                      
         B     BAD1E                                                            
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BUSINESS ADDRESS LINE 1                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING OATELD,R3                                                        
VBADL1   MVC   BOELEM,BCSPACES                                                  
         LA    R3,BOELEM                                                        
         XC    OATSTAT,OATSTAT                                                  
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OATELQ',ACTRECD),       C        
               (L'OATSUB,=AL1(OATSUB5Q))                                        
         CLI   12(R1),0                                                         
         BNE   VBADL10            No element found                              
         L     RF,12(,R1)                                                       
         IC    RE,1(,RF)                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   OATELD(0),0(RF)                                                  
         XC    RTPARM,RTPARM                                                    
         GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('OATELQ',ACTRECD),       C        
               (L'OATSUB,=AL1(OATSUB5Q))                                        
*                                                                               
VBADL10  LA    R3,BOELEM                                                        
         MVI   OATEL,OATELQ        BUILD X'8C' ELEMENT IN BOELE                 
         MVI   OATLN,OATLNQ                                                     
         MVI   OATSUB,OATSUB5Q     CREDITOR ADDRESS                             
         OI    OATSTAT,OATCSZ                                                   
         MVC   OATLINE1,BCSPACES                                                
         MVC   OATLINE1,FVIFLD      MOVE ADDRESS LINE 1                         
         B     BAD1E                Don't write till we process zip             
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BUSINESS ADDRESS LINE 2                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OATEL,L2'                                                    
BADL2    NTRDO BAD2TBL,BADL2,BAD2                                               
*                                                                               
BAD2TBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DBADL2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VBADL2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BUSINESS ADDR LINE 2                                        *         
***********************************************************************         
         SPACE 1                                                                
DBADL2   MVC   BOELEM,BCSPACES                                                  
         LA    R3,BOELEM                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OATELQ',ACTRECD),       C        
               (L'OATSUB,=AL1(OATSUB5Q))                                        
         CLI   12(R1),0                                                         
         BNE   BAD2E                                                            
         USING OATELD,RF                                                        
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'OATLINE2),OATLINE2                                      
         B     BAD2E                                                            
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BUSINESS ADDRESS LINE 2                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING OATELD,R3                                                        
VBADL2   LA    R3,BOELEM                                                        
         MVI   OATEL,OATELQ        BUILD X'8C' ELEMENT IN BOELE                 
         MVI   OATLN,OATLNQ                                                     
         MVI   OATSUB,OATSUB5Q     CREDITOR ADDRESS                             
         OI    OATSTAT,OATCSZ                                                   
         MVC   OATLINE2,BCSPACES                                                
         MVC   OATLINE2,FVIFLD                                                  
         B     BAD2E               Don't write till we process zip              
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BUSINESS ADDRESS CITY                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OATCITY'                                                     
BADCTY   NTRDO BADCTTB,BADCTY,BCTY                                              
*                                                                               
BADCTTB  DC    AL1(DDIS),AL1(0,0,0),AL4(DBCTY)                                  
         DC    AL1(DVAL),AL1(0,0,0),AL4(VBCTY)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BUSINESS ADDR CITY                                          *         
***********************************************************************         
         SPACE 1                                                                
DBCTY    MVC   BOELEM,BCSPACES                                                  
         LA    R3,BOELEM                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OATELQ',ACTRECD),       C        
               (L'OATSUB,=AL1(OATSUB5Q))                                        
         CLI   12(R1),0                                                         
         BNE   BCTYE                                                            
         USING OATELD,RF                                                        
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'OATCITY),OATCITY                                        
         B     BCTYE                                                            
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BUSINESS ADDRESS CITY                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING OATELD,R3                                                        
VBCTY    LA    R3,BOELEM                                                        
         MVI   OATEL,OATELQ        BUILD X'8C' ELEMENT IN BOELE                 
         MVI   OATLN,OATLNQ                                                     
         MVI   OATSUB,OATSUB5Q     CREDITOR ADDRESS                             
         OI    OATSTAT,OATCSZ                                                   
         MVC   OATCITY,BCSPACES                                                 
         MVC   OATCITY,FVIFLD                                                   
         B     BCTYE               Don't write till we process zip              
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BUSINESS ADDRESS STATE                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OATSTATE'                                                    
BADST    NTRDO BADSTTB,BADST,BST                                                
*                                                                               
BADSTTB  DC    AL1(DDIS),AL1(0,0,0),AL4(DBSTT)                                  
         DC    AL1(DVAL),AL1(0,0,0),AL4(VBSTT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BUSINESS ADDR STATE                                         *         
***********************************************************************         
         SPACE 1                                                                
DBSTT    MVC   BOELEM,BCSPACES                                                  
         LA    R3,BOELEM                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OATELQ',ACTRECD),       C        
               (L'OATSUB,=AL1(OATSUB5Q))                                        
         CLI   12(R1),0                                                         
         BNE   BSTE                                                             
         USING OATELD,RF                                                        
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'OATSTATE),OATSTATE                                      
         B     BSTE                                                             
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BUSINESS ADDRESS STATE                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING OATELD,R3                                                        
VBSTT    LA    R3,BOELEM                                                        
         MVI   OATEL,OATELQ        BUILD X'8C' ELEMENT IN BOELE                 
         MVI   OATLN,OATLNQ                                                     
         MVI   OATSUB,OATSUB5Q     CREDITOR ADDRESS                             
         OI    OATSTAT,OATCSZ                                                   
         MVC   OATSTATE,BCSPACES                                                
         MVC   OATSTATE,FVIFLD                                                  
         B     BSTE                Don't write till we process zip              
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BUSINESS ADDRESS ZIP                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OATZIP'                                                      
BADZIP   NTRDO BADZPTB,BADZIP,BZP                                               
*                                                                               
BADZPTB  DC    AL1(DDIS),AL1(0,0,0),AL4(DBZIP)                                  
         DC    AL1(DVAL),AL1(0,0,0),AL4(VBZIP)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BUSINESS ADDR ZIP                                           *         
***********************************************************************         
         SPACE 1                                                                
DBZIP    MVC   BOELEM,BCSPACES                                                  
         LA    R3,BOELEM                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OATELQ',ACTRECD),       C        
               (L'OATSUB,=AL1(OATSUB5Q))                                        
         CLI   12(R1),0                                                         
         BNE   BZPE                                                             
         USING OATELD,RF                                                        
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'OATZIP),OATZIP                                          
         B     BZPE                                                             
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BUSINESS ADDRESS ZIP                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING OATELD,R3                                                        
VBZIP    LA    R3,BOELEM                                                        
         MVI   OATEL,OATELQ        BUILD X'8C' ELEMENT IN BOELE                 
         MVI   OATLN,OATLNQ                                                     
         MVI   OATSUB,OATSUB5Q     CREDITOR ADDRESS                             
         OI    OATSTAT,OATCSZ                                                   
         MVC   OATZIP,BCSPACES                                                  
         MVC   OATZIP,FVIFLD                                                    
         B     BZPE                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BUSINESS ADDRESS ZIP ROUTING                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OATZIPR'                                                     
BADZIPR  NTRDO BADZRTB,BADZIR,BZR                                               
*                                                                               
BADZRTB  DC    AL1(DDIS),AL1(0,0,0),AL4(DBZIPR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VBZIPR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BUSINESS ADDR ZIP ROUTING                                   *         
***********************************************************************         
         SPACE 1                                                                
DBZIPR   MVC   BOELEM,BCSPACES                                                  
         LA    R3,BOELEM                                                        
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OATELQ',ACTRECD),       C        
               (L'OATSUB,=AL1(OATSUB5Q))                                        
         CLI   12(R1),0                                                         
         BNE   BZRE                                                             
         USING OATELD,RF                                                        
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'OATZIPRN),OATZIPRN                                      
         B     BZRE                                                             
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BUSINESS ADDRESS ZIP ROUTING                               *         
***********************************************************************         
         SPACE 1                                                                
         USING OATELD,R3                                                        
VBZIPR   LA    R3,BOELEM                                                        
         MVI   OATEL,OATELQ        BUILD X'8C' ELEMENT IN BOELE                 
         MVI   OATLN,OATLNQ                                                     
         MVI   OATSUB,OATSUB5Q     CREDITOR ADDRESS                             
         OI    OATSTAT,OATCSZ                                                   
         MVC   OATZIPRN,BCSPACES                                                
         MVC   OATZIPRN,FVIFLD                                                  
*                                                                               
         LA    RF,OATLINE1                                                      
         CLC   0(L'OATCSZLN,RF),BCSPACES   Anything in address?                 
         BE    BZRE                                                             
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,OATELD                     
         B     BZRE                                                             
         DROP  R3                                                               
***********************************************************************         
* DATA OBJECT FOR TELEPHONE NUMBER                                    *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'FFTTPTEL'                                                    
         USING *,RB                                                             
TELNO    NTRDO TELNTABL,TELNO,TELN                                              
*                                                                               
TELNTABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISTELN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTELN)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY TELEPHONE NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
DISTELN  GOTO1 AGETFFT,RTPARM,ACTRECD,FFTTPTEL                                  
         B     TELNX                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE TELEPHONE NUMBER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALTELN  GOTO1 ABLDFFT,RTPARM,ACTRECD,FFTTPTEL                                  
         B     TELNX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMENT LINE 1                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'FTTPCOM,1'                                                   
COMML1   NTRDO COM1TABL,COMML1,COM1                                             
*                                                                               
COM1TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOM1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOM1)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMMENT LINE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
DISCOM1  GOTO1 AGETFFT,RTPARM,ACTRECD,(1,FFTTPCOM)                              
         B     COM1X                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMENT LINE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
VALCOM1  GOTO1 ABLDFFT,RTPARM,ACTRECD,(1,FFTTPCOM)                              
         B     COM1X                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMENT LINE 2                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'FTTPCOM,2'                                                   
COMML2   NTRDO COM2TABL,COMML2,COM2                                             
*                                                                               
COM2TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOM2)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOM2)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMMENT LINE 2                                              *         
***********************************************************************         
         SPACE 1                                                                
DISCOM2  GOTO1 AGETFFT,RTPARM,ACTRECD,(2,FFTTPCOM)                              
         B     COM2X                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMENT LINE 2                                             *         
***********************************************************************         
         SPACE 1                                                                
VALCOM2  GOTO1 ABLDFFT,RTPARM,ACTRECD,(2,FFTTPCOM)                              
         B     COM2X                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMENT LINE 3                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'FTTPCOM,3'                                                   
COMML3   NTRDO COM3TABL,COMML3,COM3                                             
*                                                                               
COM3TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOM3)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOM3)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMMENT LINE 3                                              *         
***********************************************************************         
         SPACE 1                                                                
DISCOM3  GOTO1 AGETFFT,RTPARM,ACTRECD,(3,FFTTPCOM)                              
         B     COM3X                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMMENT LINE 3                                             *         
***********************************************************************         
         SPACE 1                                                                
VALCOM3  GOTO1 ABLDFFT,RTPARM,ACTRECD,(3,FFTTPCOM)                              
         B     COM3X                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SECURITY LEVEL                                      *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTSECY'                                                     
SECLV    NTRDO SCLVTABL,SECLV,SCLV                                              
*                                                                               
SCLVTABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISSCLV)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSCLV)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFSCLV)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFSCLV)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DFDCLV)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SECURITY LEVEL                                              *         
***********************************************************************         
         SPACE 1                                                                
DISSCLV  GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   SCLVX                                                            
         L     RF,12(R1)                                                        
         XR    RE,RE                                                            
         ICM   RE,3,RSTSECY-RSTELD(RF)                                          
         CURED (RE),(5,FVIFLD),0,ALIGN=LEFT,DMCB=BCPARM                         
         B     SCLVX                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SECURITY LEVEL                                             *         
***********************************************************************         
         SPACE 1                                                                
VALSCLV  GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ELNOF)                                           
         B     SCLVL               CAN`T FIND RSTEL                             
*                                                                               
         USING RSTELD,RF                                                        
         L     RF,12(R1)                                                        
         CLI   FVILEN,0            NO INPUT - DEFAULT IS 0                      
         BNE   *+10                                                             
         XR    RE,RE                                                            
         B     VSEC02                                                           
         SPACE 1                                                                
         TM    FVIIND,FVINUM       FIELD MUST BE NUMERIC IF INPUT               
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     SCLVL                                                            
         SPACE 1                                                                
         L     RE,BCFULL                                                        
         CH    RE,=H'255'          NUMBER CONSTRAINED TO <255                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$N1255)                                           
         B     SCLVL                                                            
         SPACE 1                                                                
VSEC02   STCM  RE,3,RSTSECY        SAVE NEW SECURITY LEVEL                      
         B     SCLVE                                                            
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SECURITY LEVEL FILTER                                       *         
***********************************************************************         
         SPACE 1                                                                
DFSCLV   XR    RE,RE                                                            
         ICM   RE,3,FLTIFLD                                                     
         BZ    SCLVE                                                            
         CURED (RE),(5,FVIFLD),0,ALIGN=LEFT,DMCB=BCPARM                         
         B     SCLVE                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SECURITY LEVEL FILTER                                      *         
***********************************************************************         
         SPACE 1                                                                
VFSCLV   CLI   FVILEN,0            NO INPUT - DEFAULT IS 0                      
         BNE   *+10                                                             
         XR    RE,RE                                                            
         B     VFSC02                                                           
         SPACE 1                                                                
         TM    FVIIND,FVINUM       FIELD MUST BE NUMERIC IF INPUT               
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     SCLVL                                                            
         SPACE 1                                                                
         L     RE,BCFULL                                                        
         CH    RE,=H'255'          NUMBER CONSTRAINED TO <255                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     SCLVL                                                            
         SPACE 1                                                                
VFSC02   STCM  RE,3,FLTIFLD        SAVE NEW SECURITY LEVEL                      
         B     SCLVE                                                            
         SPACE 2                                                                
***********************************************************************         
* DO SECURITY LEVEL FILTER                                            *         
***********************************************************************         
         SPACE 1                                                                
DFDCLV   GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    *+12                                                             
         MVI   RTPARMS,DFLTX                                                    
         B     SCLVL               CAN`T FIND RSTEL SO WE DON`T WANT IT         
*                                                                               
         L     RF,12(R1)                                                        
         USING RSTELD,RF                                                        
         CLC   RSTSECY,FLTIFLD                                                  
         MVI   RTPARMS,DFLTL                                                    
         BL    SCLVE                                                            
         MVI   RTPARMS,DFLTE                                                    
         BE    SCLVE                                                            
         MVI   RTPARMS,DFLTH                                                    
         BH    SCLVE                                                            
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RSTFILT 1 VALUE                                     *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P3 HOLDS EQUATED VERB                                               *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTFILT1'                                                    
RSTF1    NTRDO RST1TABL,RSTF1,RST1                                              
*                                                                               
RST1TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRSF1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRSF1)                                
         DC    AL1(DHED),AL1(0,0,0),AL4(DISHD1)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DISMHD1)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RSTFILT1 VALUE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISRSF1  MVI   RTBYTE1,1                                                        
TEMP     USING ACTKSTA,GSRECSTA    GET RSTFILT1 CODE                            
         MVC   FVIFLD(L'ACTKSAF1),TEMP.ACTKSAF1                                 
         DROP  TEMP                                                             
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST1E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVC   RSFKFLT#,RTBYTE1                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST1E               NO FILTER NAME RECORD SET UP                 
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,FVIFLD)                                               
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RST1E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'RSFCODE),RSFCODE-RSFELD(RF)                             
         B     RST1E               TYPE EXISTS & IS VALID                       
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE RSTFILT1 VALUE                                             *         
***********************************************************************         
         SPACE 1                                                                
VALRSF1  MVC   RTBYTE1,FVIFLD      SAVE FIRST BYTE INPUT                        
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RSTF114             COMPANY IS ONLY ON CHARACTERS                
*                                                                               
         CLI   FVILEN,0            EMPTY FIELD?                                 
         BE    RSTF114                                                          
*                                                                               
*@@      CLI   FVIFLD,C'='         DO WE WANT TO SEARCH?                        
*@@      BNE   RSTF102             NO                                           
*@@      GOTO1 ANTRSES,=AL1(R#FVAL,A#SCH,RTNSCHQ,0,SH#FVAL,1)                   
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
RSTF102  MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ    TYPE                                         
         MVC   RSFKCPY,ACTKCPY     COMPANY                                      
         MVC   RSFKUNT,ACTKUNT     UNIT                                         
         MVC   RSFKLDG,ACTKLDG     LEDGER                                       
         MVI   RSFKFLT#,1          RSTFILT #1                                   
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    RSTF104             RSTFILT #1 SET UP AS A NAMED TYPE            
*                                                                               
         CLI   FVILEN,L'RSTFILT1   IF LENGTH IS SAME AS L'RSTFILT THEN          
         BE    RSTF114             USE THIS AS THE VALUE                        
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     RST1L               OTHERWISE FIELD IS INVALID                   
*                                                                               
RSTF104  LHI   R1,XOGET+XOACCMST+XIO1 USE LEDGER RECORD                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
         POP   USING                                                            
*                                                                               
         CLI   FVILEN,L'RSFTYPE    IF LENGTH IS SAME AS L'RSFTYPE               
         BNE   RSTF108             TRY TO FIND A MATCHING RSFTYPE               
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0                                                         
         BE    RSTF114             TYPE EXISTS & IS VALID                       
*                                                                               
         USING RSFELD,R3                                                        
RSTF108  LA    R3,RSFRFST-RSFRECD(R3) START OF THE DATA                         
         XR    RE,RE               SEE IF IT IS A PARTIAL CODE                  
*                                                                               
RSTF110  CLI   RSFEL,0             E.O.R.                                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     RST1L               FIELD IS INVALID IF REACH END                
*                                                                               
         CLI   RSFEL,RSFELQ        FILTER ELEMENT?                              
         BNE   RSTF112                                                          
*                                                                               
         IC    RE,FVXLEN           COMPARE INPUT AGAINST CODE                   
         EX    RE,*+8                                                           
         BNE   RSTF112             IT DOESN`T MATCH - TRY NEXT                  
*                                                                               
         CLC   FVIFLD(0),RSFCODE                                                
         MVC   RTBYTE1,RSFTYPE     HAVE A MATCH - SAVE THE CODE                 
         B     RSTF114                                                          
*                                                                               
RSTF112  IC    RE,RSFLN            BUMP TO NEXT AND TRY AGAIN                   
         LA    R3,0(RE,R3)                                                      
         B     RSTF110                                                          
*                                                                               
RSTF114  GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ELNOF)                                           
         B     RST1L               CAN`T FIND RSTEL                             
*                                                                               
         USING RSTELD,RF                                                        
TEMP     USING ACTKSTA,GSRECSTA                                                 
         L     RF,12(R1)                                                        
         MVC   RSTFILT1,RTBYTE1       PUT INTO RSTEL                            
         MVC   TEMP.ACTKSAF1,RTBYTE1  AND INTO DIRECTORY STATUS TOO             
         B     RST1E                                                            
         DROP  TEMP                                                             
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RSTFLT1 NAME INTO TAG FIELD ON MAINT SCREEN                 *         
* P6 = A(BUILD AREA)                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISMHD1  MVI   RTBYTE1,1                                                        
         USING FDRELD,R3                                                        
         CLI   FDRTDEFL,0          MUST HAVE A TAG                              
         BZ    RST1E                                                            
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST1E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST1E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',AIO1),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE SET UP?                          
         BNE   RST1E               NO                                           
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         L     R1,RTPARMS6         A(SECOND HEADLINE)                           
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BNH   *+14                NO - USE COMPULSORY CODE THEN                
         MVC   0(L'RSFSNAM,R1),RSFSNAM                                          
         B     RST1E                                                            
*                                                                               
         MVC   0(L'RSFCODE,R1),RSFCODE                                          
         B     RST1E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RSTFLT1 NAME FOR LIST HEADLINE                              *         
* P5 = LIST HEADLINE 1                                                *         
* P6 = LIST HEADLINE 2                                                *         
***********************************************************************         
         SPACE 1                                                                
DISHD1   B     RST1E                                                            
*ISHD1   MVI   RTBYTE1,1           DEFAULT DISPLAY IS #:                        
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST1E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
*        MVC   RSFKCPY,ACTKCPY                                                  
*        MVC   RSFKUNT,ACTKUNT                                                  
*        MVC   RSFKLDG,ACTKLDG                                                  
***                                                                             
         MVC   RSFKCPY,CUABIN                                                   
         MVI   RSFKUNT,C'S'                                                     
         MVI   RSFKLDG,C'V'                                                     
***                                                                             
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST1E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',AIO1),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE SET UP?                          
         BNE   RST1E               NO                                           
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         L     R1,RTPARMS6         A(SECOND HEADLINE)                           
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BNH   *+14                NO - USE COMPULSORY CODE THEN                
         MVC   0(L'RSFSNAM,R1),RSFSNAM                                          
         B     RST1E                                                            
*                                                                               
         MVC   0(L'RSFCODE,R1),RSFCODE                                          
         B     RST1E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RSTFILT 2 VALUE                                     *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTFILT2'                                                    
RSTF2    NTRDO RST2TABL,RSTF2,RST2                                              
*                                                                               
RST2TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRSF2)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRSF2)                                
         DC    AL1(DMHED),AL1(0,0,0),AL4(DISMHD2)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY RSTFILT2 VALUE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISRSF2  MVI   RTBYTE1,2                                                        
TEMP     USING ACTKSTA,GSRECSTA    GET RSTFILT2 CODE                            
         MVC   FVIFLD(L'ACTKSAF1),TEMP.ACTKSAF2                                 
         DROP  TEMP                                                             
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST2E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVC   RSFKFLT#,RTBYTE1                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST2E               NO FILTER NAME RECORD SET UP                 
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,FVIFLD)                                               
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RST2E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'RSFCODE),RSFCODE-RSFELD(RF)                             
         B     RST2E               TYPE EXISTS & IS VALID                       
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE RSTFILT 2 VALUE                                            *         
***********************************************************************         
         SPACE 1                                                                
VALRSF2  MVC   RTBYTE1,FVIFLD      SAVE FIRST BYTE INPUT                        
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RSTF214             COMPANY IS ONLY ON CHARACTERS                
*                                                                               
         CLI   FVILEN,0            EMPTY FIELD?                                 
         BE    RSTF214                                                          
*                                                                               
         CLI   FVIFLD,C'='         DO WE WANT TO SEARCH?                        
         BNE   RSTF202             NO                                           
*@@      GOTO1 ANTRSES,=AL1(R#FVAL,A#SCH,RTNSCHQ,0,SH#FVAL,2)                   
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
RSTF202  MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ    TYPE                                         
         MVC   RSFKCPY,ACTKCPY     COMPANY                                      
         MVC   RSFKUNT,ACTKUNT     UNIT                                         
         MVC   RSFKLDG,ACTKLDG     LEDGER                                       
         MVI   RSFKFLT#,2          RSTFILT #2                                   
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    RSTF204             RSTFILT #2 SET UP AS A NAMED TYPE            
*                                                                               
         CLI   FVILEN,L'RSTFILT2   IF LENGTH IS SAME AS L'RSTFILT THEN          
         BE    RSTF214             USE THIS AS THE VALUE                        
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     RST2L               OTHERWISE FIELD IS INVALID                   
*                                                                               
RSTF204  LHI   R1,XOGET+XOACCMST+XIO1 USE LEDGER RECORD                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
         POP   USING                                                            
*                                                                               
         CLI   FVILEN,L'RSFTYPE    IF LENGTH IS SAME AS L'RSFTYPE               
         BNE   RSTF208             TRY TO FIND A MATCHING RSFTYPE               
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0                                                         
         BE    RSTF214             TYPE EXISTS & IS VALID                       
*                                                                               
         USING RSFELD,R3                                                        
RSTF208  LA    R3,RSFRFST-RSFRECD(R3) START OF THE DATA                         
         XR    RE,RE               SEE IF IT IS A PARTIAL CODE                  
*                                                                               
RSTF210  CLI   RSFEL,0             E.O.R.                                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     RST2L               FIELD IS INVALID IF REACH END                
*                                                                               
         CLI   RSFEL,RSFELQ        FILTER ELEMENT?                              
         BNE   RSTF212                                                          
*                                                                               
         IC    RE,FVXLEN           COMPARE INPUT AGAINST CODE                   
         EX    RE,*+8                                                           
         BNE   RSTF212             IT DOESN`T MATCH - TRY NEXT                  
*                                                                               
         CLC   FVIFLD(0),RSFCODE                                                
         MVC   RTBYTE1,RSFTYPE     HAVE A MATCH - SAVE THE CODE                 
         B     RSTF214                                                          
*                                                                               
RSTF212  IC    RE,RSFLN            BUMP TO NEXT AND TRY AGAIN                   
         LA    R3,0(RE,R3)                                                      
         B     RSTF210                                                          
*                                                                               
RSTF214  GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ELNOF)                                           
         B     RST2L               CAN`T FIND RSTEL                             
*                                                                               
         USING RSTELD,RF                                                        
TEMP     USING ACTKSTA,GSRECSTA                                                 
         L     RF,12(R1)                                                        
         MVC   RSTFILT2,RTBYTE1       PUT INTO RSTEL                            
         MVC   TEMP.ACTKSAF2,RTBYTE1  AND INTO DIRECTORY STATUS TOO             
         B     RST2E                                                            
         DROP  TEMP,RF,R3                                                       
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RSTFLT2 NAME INTO TAG FIELD ON MAINT SCREEN                 *         
* P6 = A(BUILD AREA)                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISMHD2  MVI   RTBYTE1,2                                                        
         USING FDRELD,R3                                                        
         CLI   FDRTDEFL,0          MUST HAVE A TAG                              
         BZ    RST2E                                                            
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST2E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST2E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',AIO1),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE SET UP?                          
         BNE   RST2E               NO                                           
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         L     R1,RTPARMS6         A(SECOND HEADLINE)                           
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BNH   *+14                NO - USE COMPULSORY CODE THEN                
         MVC   0(L'RSFSNAM,R1),RSFSNAM                                          
         B     RST2E                                                            
*                                                                               
         MVC   0(L'RSFCODE,R1),RSFCODE                                          
         B     RST2E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         SPACE 2                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RSTFILT 3 VALUE                                     *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTFILT3'                                                    
RSTF3    NTRDO RST3TABL,RSTF3,RST3                                              
*                                                                               
RST3TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRSF3)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRSF3)                                
         DC    AL1(DMHED),AL1(0,0,0),AL4(DISMHD3)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY RSTFILT3 VALUE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISRSF3  MVI   RTBYTE1,3                                                        
TEMP     USING ACTKSTA,GSRECSTA    GET RSTFILT3 CODE                            
         MVC   FVIFLD(L'ACTKSAF3),TEMP.ACTKSAF3                                 
         DROP  TEMP                                                             
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST3E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVC   RSFKFLT#,RTBYTE1                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST3E               NO FILTER NAME RECORD SET UP                 
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,FVIFLD)                                               
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RST3E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'RSFCODE),RSFCODE-RSFELD(RF)                             
         B     RST3E               TYPE EXISTS & IS VALID                       
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE RSTFILT 3 VALUE                                            *         
***********************************************************************         
         SPACE 1                                                                
VALRSF3  MVC   RTBYTE1,FVIFLD      SAVE FIRST BYTE INPUT                        
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RSTF314             COMPANY IS ONLY ON CHARACTERS                
*                                                                               
         CLI   FVILEN,0            EMPTY FIELD?                                 
         BE    RSTF314                                                          
*                                                                               
         CLI   FVIFLD,C'='         DO WE WANT TO SEARCH?                        
         BNE   RSTF302             NO                                           
*@@      GOTO1 ANTRSES,=AL1(R#FVAL,A#SCH,RTNSCHQ,0,SH#FVAL,3)                   
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
RSTF302  MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ    TYPE                                         
         MVC   RSFKCPY,ACTKCPY     COMPANY                                      
         MVC   RSFKUNT,ACTKUNT     UNIT                                         
         MVC   RSFKLDG,ACTKLDG     LEDGER                                       
         MVI   RSFKFLT#,3          RSTFILT #3                                   
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    RSTF304             RSTFILT #3 SET UP AS A NAMED TYPE            
*                                                                               
         CLI   FVILEN,L'RSTFILT3   IF LENGTH IS SAME AS L'RSTFILT THEN          
         BE    RSTF314             USE THIS AS THE VALUE                        
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     RST3L               OTHERWISE FIELD IS INVALID                   
*                                                                               
RSTF304  LHI   R1,XOGET+XOACCMST+XIO1 USE LEDGER RECORD                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
         POP   USING                                                            
*                                                                               
         CLI   FVILEN,L'RSFTYPE    IF LENGTH IS SAME AS L'RSFTYPE               
         BNE   RSTF308             TRY TO FIND A MATCHING RSFTYPE               
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0                                                         
         BE    RSTF314             TYPE EXISTS & IS VALID                       
*                                                                               
         USING RSFELD,R3                                                        
RSTF308  LA    R3,RSFRFST-RSFRECD(R3) START OF THE DATA                         
         XR    RE,RE               SEE IF IT IS A PARTIAL CODE                  
*                                                                               
RSTF310  CLI   RSFEL,0             E.O.R.                                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     RST3L               FIELD IS INVALID IF REACH END                
*                                                                               
         CLI   RSFEL,RSFELQ        FILTER ELEMENT?                              
         BNE   RSTF312                                                          
*                                                                               
         IC    RE,FVXLEN           COMPARE INPUT AGAINST CODE                   
         EX    RE,*+8                                                           
         BNE   RSTF312             IT DOESN`T MATCH - TRY NEXT                  
*                                                                               
         CLC   FVIFLD(0),RSFCODE                                                
         MVC   RTBYTE1,RSFTYPE     HAVE A MATCH - SAVE THE CODE                 
         B     RSTF314                                                          
*                                                                               
RSTF312  IC    RE,RSFLN            BUMP TO NEXT AND TRY AGAIN                   
         LA    R3,0(RE,R3)                                                      
         B     RSTF310                                                          
*                                                                               
RSTF314  GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ELNOF)                                           
         B     RST3L               CAN`T FIND RSTEL                             
*                                                                               
         USING RSTELD,RF                                                        
TEMP     USING ACTKSTA,GSRECSTA                                                 
         L     RF,12(R1)                                                        
         MVC   RSTFILT3,RTBYTE1       PUT INTO RSTEL                            
         MVC   TEMP.ACTKSAF3,RTBYTE1  AND INTO DIRECTORY STATUS TOO             
         B     RST3E                                                            
         DROP  TEMP,RF,R3                                                       
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RSTFLT3 NAME INTO TAG FIELD ON MAINT SCREEN                 *         
* P6 = A(BUILD AREA)                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISMHD3  MVI   RTBYTE1,3                                                        
         USING FDRELD,R3                                                        
         CLI   FDRTDEFL,0          MUST HAVE A TAG                              
         BZ    RST3E                                                            
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST3E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST3E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',AIO1),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE SET UP?                          
         BNE   RST3E               NO                                           
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         L     R1,RTPARMS6         A(SECOND HEADLINE)                           
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BNH   *+14                NO - USE COMPULSORY CODE THEN                
         MVC   0(L'RSFSNAM,R1),RSFSNAM                                          
         B     RST3E                                                            
*                                                                               
         MVC   0(L'RSFCODE,R1),RSFCODE                                          
         B     RST3E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         SPACE 2                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RSTFILT 4 VALUE                                     *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTFILT4'                                                    
RSTF4    NTRDO RST4TABL,RSTF4,RST4                                              
*                                                                               
RST4TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRSF4)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRSF4)                                
         DC    AL1(DMHED),AL1(0,0,0),AL4(DISMHD4)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY RSTFILT4 VALUE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISRSF4  MVI   RTBYTE1,4                                                        
TEMP     USING ACTKSTA,GSRECSTA    GET RSTFILT4 CODE                            
         MVC   FVIFLD(L'ACTKSAF4),TEMP.ACTKSAF4                                 
         DROP  TEMP                                                             
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST4E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVC   RSFKFLT#,RTBYTE1                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST4E               NO FILTER NAME RECORD SET UP                 
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,FVIFLD)                                               
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RST4E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'RSFCODE),RSFCODE-RSFELD(RF)                             
         B     RST4E               TYPE EXISTS & IS VALID                       
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE RSTFILT 4 VALUE                                            *         
***********************************************************************         
         SPACE 1                                                                
VALRSF4  MVC   RTBYTE1,FVIFLD      SAVE FIRST BYTE INPUT                        
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RSTF414             COMPANY IS ONLY ON CHARACTERS                
*                                                                               
         CLI   FVILEN,0            EMPTY FIELD?                                 
         BE    RSTF414                                                          
*                                                                               
         CLI   FVIFLD,C'='         DO WE WANT TO SEARCH?                        
         BNE   RSTF402             NO                                           
*@@      GOTO1 ANTRSES,=AL1(R#FVAL,A#SCH,RTNSCHQ,0,SH#FVAL,4)                   
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
RSTF402  MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ    TYPE                                         
         MVC   RSFKCPY,ACTKCPY     COMPANY                                      
         MVC   RSFKUNT,ACTKUNT     UNIT                                         
         MVC   RSFKLDG,ACTKLDG     LEDGER                                       
         MVI   RSFKFLT#,4          RSTFILT #4                                   
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    RSTF404             RSTFILT #4 SET UP AS A NAMED TYPE            
*                                                                               
         CLI   FVILEN,L'RSTFILT3   IF LENGTH IS SAME AS L'RSTFILT THEN          
         BE    RSTF414             USE THIS AS THE VALUE                        
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     RST4L               OTHERWISE FIELD IS INVALID                   
*                                                                               
RSTF404  LHI   R1,XOGET+XOACCMST+XIO1 USE LEDGER RECORD                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
         POP   USING                                                            
*                                                                               
         CLI   FVILEN,L'RSFTYPE    IF LENGTH IS SAME AS L'RSFTYPE               
         BNE   RSTF408             TRY TO FIND A MATCHING RSFTYPE               
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0                                                         
         BE    RSTF414             TYPE EXISTS & IS VALID                       
*                                                                               
         USING RSFELD,R3                                                        
RSTF408  LA    R3,RSFRFST-RSFRECD(R3) START OF THE DATA                         
         XR    RE,RE               SEE IF IT IS A PARTIAL CODE                  
*                                                                               
RSTF410  CLI   RSFEL,0             E.O.R.                                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     RST4L               FIELD IS INVALID IF REACH END                
*                                                                               
         CLI   RSFEL,RSFELQ        FILTER ELEMENT?                              
         BNE   RSTF412                                                          
*                                                                               
         IC    RE,FVXLEN           COMPARE INPUT AGAINST CODE                   
         EX    RE,*+8                                                           
         BNE   RSTF412             IT DOESN`T MATCH - TRY NEXT                  
*                                                                               
         CLC   FVIFLD(0),RSFCODE                                                
         MVC   RTBYTE1,RSFTYPE     HAVE A MATCH - SAVE THE CODE                 
         B     RSTF414                                                          
*                                                                               
RSTF412  IC    RE,RSFLN            BUMP TO NEXT AND TRY AGAIN                   
         LA    R3,0(RE,R3)                                                      
         B     RSTF410                                                          
*                                                                               
RSTF414  GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ELNOF)                                           
         B     RST4L               CAN`T FIND RSTEL                             
*                                                                               
         USING RSTELD,RF                                                        
TEMP     USING ACTKSTA,GSRECSTA                                                 
         L     RF,12(R1)                                                        
         MVC   RSTFILT4,RTBYTE1       PUT INTO RSTEL                            
         MVC   TEMP.ACTKSAF4,RTBYTE1  AND INTO DIRECTORY STATUS TOO             
         B     RST4E                                                            
         DROP  TEMP,RF,R3                                                       
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RSTFLT4 NAME INTO TAG FIELD ON MAINT SCREEN                 *         
* P6 = A(BUILD AREA)                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISMHD4  MVI   RTBYTE1,4                                                        
         USING FDRELD,R3                                                        
         CLI   FDRTDEFL,0          MUST HAVE A TAG                              
         BZ    RST4E                                                            
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST4E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST4E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',AIO1),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE SET UP?                          
         BNE   RST4E               NO                                           
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         L     R1,RTPARMS6         A(SECOND HEADLINE)                           
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BNH   *+14                NO - USE COMPULSORY CODE THEN                
         MVC   0(L'RSFSNAM,R1),RSFSNAM                                          
         B     RST4E                                                            
*                                                                               
         MVC   0(L'RSFCODE,R1),RSFCODE                                          
         B     RST4E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         SPACE 2                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RSTFILT 5 VALUE                                     *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTFILT5'                                                    
RSTF5    NTRDO RST5TABL,RSTF5,RST5                                              
*                                                                               
RST5TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRSF5)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRSF5)                                
         DC    AL1(DMHED),AL1(0,0,0),AL4(DISMHD5)                               
         DC    AL1(DHED),AL1(0,0,0),AL4(HEDRSF5)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY RSTFILT5 VALUE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISRSF5  MVI   RTBYTE1,5                                                        
TEMP     USING ACTKSTA,GSRECSTA    GET RSTFILT5 CODE                            
         MVC   FVIFLD(L'ACTKSAF5),TEMP.ACTKSAF5                                 
         DROP  TEMP                                                             
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST5E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVC   RSFKFLT#,RTBYTE1                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST5E               NO FILTER NAME RECORD SET UP                 
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,FVIFLD)                                               
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RST5E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'RSFCODE),RSFCODE-RSFELD(RF)                             
         B     RST5E               TYPE EXISTS & IS VALID                       
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE RSTFILT 5 VALUE                                            *         
***********************************************************************         
         SPACE 1                                                                
VALRSF5  MVC   RTBYTE1,FVIFLD      SAVE FIRST BYTE INPUT                        
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RSTF514             COMPANY IS ONLY ON CHARACTERS                
*                                                                               
         CLI   FVILEN,0            EMPTY FIELD?                                 
         BE    RSTF514                                                          
*                                                                               
         CLI   FVIFLD,C'='         DO WE WANT TO SEARCH?                        
         BNE   RSTF502             NO                                           
*@@      GOTO1 ANTRSES,=AL1(R#FVAL,A#SCH,RTNSCHQ,0,SH#FVAL,5)                   
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
RSTF502  MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ    TYPE                                         
         MVC   RSFKCPY,ACTKCPY     COMPANY                                      
         MVC   RSFKUNT,ACTKUNT     UNIT                                         
         MVC   RSFKLDG,ACTKLDG     LEDGER                                       
         MVI   RSFKFLT#,5          RSTFILT #5                                   
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    RSTF504             RSTFILT #5 SET UP AS A NAMED TYPE            
*                                                                               
         CLI   FVILEN,L'RSTFILT3   IF LENGTH IS SAME AS L'RSTFILT THEN          
         BE    RSTF514             USE THIS AS THE VALUE                        
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     RST5L               OTHERWISE FIELD IS INVALID                   
*                                                                               
RSTF504  LHI   R1,XOGET+XOACCMST+XIO1 USE LEDGER RECORD                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
         POP   USING                                                            
*                                                                               
         CLI   FVILEN,L'RSFTYPE    IF LENGTH IS SAME AS L'RSFTYPE               
         BNE   RSTF508             TRY TO FIND A MATCHING RSFTYPE               
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0                                                         
         BE    RSTF514             TYPE EXISTS & IS VALID                       
*                                                                               
         USING RSFELD,R3                                                        
RSTF508  LA    R3,RSFRFST-RSFRECD(R3) START OF THE DATA                         
         XR    RE,RE               SEE IF IT IS A PARTIAL CODE                  
*                                                                               
RSTF510  CLI   RSFEL,0             E.O.R.                                       
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     RST5L               FIELD IS INVALID IF REACH END                
*                                                                               
         CLI   RSFEL,RSFELQ        FILTER ELEMENT?                              
         BNE   RSTF512                                                          
*                                                                               
         IC    RE,FVXLEN           COMPARE INPUT AGAINST CODE                   
         EX    RE,*+8                                                           
         BNE   RSTF512             IT DOESN`T MATCH - TRY NEXT                  
*                                                                               
         CLC   FVIFLD(0),RSFCODE                                                
         MVC   RTBYTE1,RSFTYPE     HAVE A MATCH - SAVE THE CODE                 
         B     RSTF514                                                          
*                                                                               
RSTF512  IC    RE,RSFLN            BUMP TO NEXT AND TRY AGAIN                   
         LA    R3,0(RE,R3)                                                      
         B     RSTF510                                                          
*                                                                               
RSTF514  GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ELNOF)                                           
         B     RST5L               CAN`T FIND RSTEL                             
*                                                                               
         USING RSTELD,RF                                                        
TEMP     USING ACTKSTA,GSRECSTA                                                 
         L     RF,12(R1)                                                        
         MVC   RSTFILT5,RTBYTE1       PUT INTO RSTEL                            
         MVC   TEMP.ACTKSAF5,RTBYTE1  AND INTO DIRECTORY STATUS TOO             
         B     RST5E                                                            
         DROP  RF,R3,TEMP                                                       
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RSTFLT5 NAME INTO TAG FIELD ON MAINT SCREEN                 *         
* P6 = A(BUILD AREA)                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISMHD5  MVI   RTBYTE1,5                                                        
         USING FDRELD,R3                                                        
         CLI   FDRTDEFL,0          MUST HAVE A TAG                              
         BZ    RST5E                                                            
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RST5E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST5E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',AIO1),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE SET UP?                          
         BNE   RST5E               NO                                           
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         L     R1,RTPARMS6         A(SECOND HEADLINE)                           
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BNH   *+14                NO - USE COMPULSORY CODE THEN                
         MVC   0(L'RSFSNAM,R1),RSFSNAM                                          
         B     RST5E                                                            
*                                                                               
         MVC   0(L'RSFCODE,R1),RSFCODE                                          
         B     RST5E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         SPACE 2                                                                
*                                                                               
***********************************************************************         
* DISPLAY RSTFILT 5 NAME FOR LIST                                     *         
*                                                                     *         
* P4 A(HEADLINE 1)                                                    *         
* P5 A(HEADLINE 2)                                                    *         
***********************************************************************         
         SPACE 1                                                                
HEDRSF5  MVI   RTBYTE1,5                                                        
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0                                                       
         POP   USING                                                            
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RST5E               NO LEDGER SET UP - DISPLAY DEFAULT           
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RST5E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     R4,RTPARMS5                                                      
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BE    SETRN04             NO - USE COMPULSORY CODE THEN                
         MVC   0(L'RSFSNAM,R4),RSFSNAM                                          
         B     RST5E                                                            
*                                                                               
SETRN04  MVC   0(L'RSFCODE,R4),RSFCODE                                          
         B     RST5E               CLEAR DEFAULT & MOVE IN CODE                 
         DROP  RF                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RSTFILT1 NAME                                       *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTF1.NM'                                                    
RSTN1    NTRDO RSN1TABL,RSTN1,RSN1                                              
*                                                                               
RSN1TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRN1)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY RSTFLT1 NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISRN1   MVI   RTBYTE1,1           DEFAULT DISPLAY IS #:                        
         MVC   FVIFLD(2),=CL2'1:'                                               
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RSN1E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RSN1E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RSN1E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BE    *+14                NO - USE COMPULSORY CODE THEN                
         MVC   FVIFLD(L'RSFSNAM),RSFSNAM                                        
         B     RSN1E                                                            
*                                                                               
         MVC   FVIFLD(L'RSFSNAM),BCSPACES                                       
         MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
         B     RSN1E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RSTFILT2 NAME                                       *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTF2.NM'                                                    
RSTN2    NTRDO RSN2TABL,RSTN2,RSN2                                              
*                                                                               
RSN2TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRN2)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY RSTFLT2 NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISRN2   MVI   RTBYTE1,2           DEFAULT DISPLAY IS #:                        
         MVC   FVIFLD(2),=CL2'2:'                                               
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RSN2E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RSN2E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RSN2E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BE    *+14                NO - USE COMPULSORY CODE THEN                
         MVC   FVIFLD(L'RSFSNAM),RSFSNAM                                        
         B     RSN2E                                                            
*                                                                               
         MVC   FVIFLD(L'RSFSNAM),BCSPACES                                       
         MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
         B     RSN2E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RSTFILT3 NAME                                       *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTF3.NM'                                                    
RSTN3    NTRDO RSN3TABL,RSTN3,RSN3                                              
*                                                                               
RSN3TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRN3)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY RSTFLT3 NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISRN3   MVI   RTBYTE1,3           DEFAULT DISPLAY IS #:                        
         MVC   FVIFLD(2),=CL2'3:'                                               
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RSN3E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RSN3E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RSN3E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BE    *+14                NO - USE COMPULSORY CODE THEN                
         MVC   FVIFLD(L'RSFSNAM),RSFSNAM                                        
         B     RSN3E                                                            
*                                                                               
         MVC   FVIFLD(L'RSFSNAM),BCSPACES                                       
         MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
         B     RSN3E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RSTFILT4 NAME                                       *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTF4.NM'                                                    
RSTN4    NTRDO RSN4TABL,RSTN4,RSN4                                              
*                                                                               
RSN4TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRN4)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY RSTFLT4 NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISRN4   MVI   RTBYTE1,4           DEFAULT DISPLAY IS #:                        
         MVC   FVIFLD(2),=CL2'4:'                                               
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RSN4E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RSN4E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RSN4E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BE    *+14                NO - USE COMPULSORY CODE THEN                
         MVC   FVIFLD(L'RSFSNAM),RSFSNAM                                        
         B     RSN4E                                                            
*                                                                               
         MVC   FVIFLD(L'RSFSNAM),BCSPACES                                       
         MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
         B     RSN4E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RSTFILT5 NAME                                       *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'RSTF5.NM'                                                    
RSTN5    NTRDO RSN5TABL,RSTN5,RSN5                                              
*                                                                               
RSN5TABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRN5)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY RSTFLT5 NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISRN5   MVI   RTBYTE1,5           DEFAULT DISPLAY IS #:                        
         MVC   FVIFLD(2),=CL2'5:'                                               
*                                                                               
         TM    BCCPYST8,CPYSFNAM   COMPANY USES NAMES?                          
         BZ    RSN5E               NO                                           
*                                                                               
         PUSH  USING                                                            
         USING RSFRECD,IOKEY                                                    
         MVC   RSFKEY,BCSPACES                                                  
         MVI   RSFKTYP,RSFKTYPQ                                                 
         MVC   RSFKCPY,ACTKCPY                                                  
         MVC   RSFKUNT,ACTKUNT                                                  
         MVC   RSFKLDG,ACTKLDG                                                  
         MVI   RSFKFLT#,0          FILTER NAME RECORD                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   RSN5E               NO LEDGER SET UP - DISPLAY DEFAULT           
                                                                                
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('RSFELQ',(R3)),          *        
               (L'RSFTYPE,RTBYTE1)                                              
         CLI   12(R1),0            IS THE CODE THERE ?                          
         BNE   RSN5E               NO - DISPLAY RSTFILT CHARACTER               
*                                                                               
         L     RF,12(R1)                                                        
         USING RSFELD,RF                                                        
         CLC   RSFSNAM,BCSPACES    SHORT NAME?                                  
         BE    *+14                NO - USE COMPULSORY CODE THEN                
         MVC   FVIFLD(L'RSFSNAM),RSFSNAM                                        
         B     RSN5E                                                            
*                                                                               
         MVC   FVIFLD(L'RSFSNAM),BCSPACES                                       
         MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
         B     RSN5E               CLEAR DEFAULT & MOVE IN CODE                 
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SHORT NAME                                          *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'SNMEL'                                                       
SHTNM    NTRDO SHNMTABL,SHTNM,SHNM                                              
*                                                                               
SHNMTABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISSNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSNM)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY SHORT NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISSNM   GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('SNMELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   SHNME                                                            
         L     RF,12(R1)                                                        
         MVC   FVIFLD(L'SNMNAME),SNMNAME-SNMELD(RF)                             
         B     SHNME                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SHORT NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALSNM   GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('SNMELQ',ACTRECD),0               
         CLI   FVILEN,0                                                         
         BE    SHNME                                                            
         LA    R3,BOWORK1                                                       
         USING SNMELD,R3                                                        
         MVI   SNMEL,SNMELQ                                                     
         MVI   SNMLN,SNMLNQ                                                     
         MVC   SNMNAME,FVIFLD                                                   
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),(R2),SNMELD                        
         CLI   12(R1),0                                                         
         BE    SHNME                                                            
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FACTORING COMPANY                                   *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'SPATFACC'                                                    
FACCA    NTRDO FACCTABL,FACCA,FACC                                              
*                                                                               
FACCTABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISFACC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFACC)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY FACTORING ACCOUNT                                           *         
***********************************************************************         
         SPACE 1                                                                
DISFACC  GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('SPAELQ',ACTRECD),       *        
               (L'SPATYPE,=AL1(SPATFACC))                                       
         CLI   12(R1),0                                                         
         BNE   FACCE                                                            
         L     RF,12(R1)                                                        
         USING SPAELD,RF                                                        
         MVC   FVIFLD(L'SPAAULA),SPAAULA                                        
         B     FACCE                                                            
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE FACTORING COMPANY                                          *         
***********************************************************************         
         SPACE 1                                                                
VALFACC  GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('SPAELQ',ACTRECD),       *        
               (L'SPATYPE,=AL1(SPATFACC))                                       
         CLI   FVILEN,0                                                         
         BE    FACCE                                                            
*                                                                               
         LA    R3,BOWORK1          BUILD SPAEL                                  
         USING SPAELD,R3                                                        
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATFACC                                                 
         MVC   SPAAULA,FVIFLD      FACTORING ACCOUNT                            
*                                                                               
TEMP     USING ACTRECD,IOKEY       NOW TRY TO FIND THE ACCOUNT                  
         MVC   TEMP.ACTKEY,BCSPACES                                             
         MVC   TEMP.ACTKCPY,ACTKCPY                                             
         MVC   TEMP.ACTKULA,SPAAULA                                             
         DROP  TEMP                                                             
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACTNF)  ACCOUNT DOES NOT EXIST                   
         B     FACCL                                                            
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                PROBLEM WITH ACCMST                          
*                                                                               
         L     RF,AIO1                                                          
         GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('ABLELQ',(RF)),0                  
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WLACT)  WRONG LEVEL - MUST HAVE ABLEL            
         B     FACCL                                                            
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,SPAELD                     
         CLI   12(R1),0                                                         
         BE    FACCE                                                            
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ONLINE MEMO ELEMENT                                 *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'OMEEL'                                                       
XMEMO    NTRDO OMEMTABL,XMEMO,OMEM                                              
*                                                                               
OMEMTABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISMEM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMEM)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY ONLINE MEMO ELEMENT                                         *         
***********************************************************************         
         SPACE 1                                                                
DISMEM   GOTO1 VHELLO,RTPARM,(C'G',GCFILNAM),('OMEELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   DISOMX                                                           
         L     R3,12(R1)                                                        
         USING OMEELD,R3                                                        
         XR    RF,RF                                                            
         IC    RF,OMELN                                                         
         SH    RF,=Y(OMELN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),OMEMO                                                  
DISOMX   BRAS  RE,CKLOPRO                                                       
         B     OMEME                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ONLINE MEMO ELEMENT                                        *         
***********************************************************************         
         SPACE 1                                                                
VALMEM   GOTO1 VHELLO,RTPARM,(C'D',GCFILNAM),('OMEELQ',ACTRECD),0               
         CLI   FVILEN,0                                                         
         BE    OMEME                                                            
*                                                                               
         PUSH  USING                                                            
         USING OMEELD,RTDATA                                                    
         XC    RTDATA(OMELN1Q+1),RTDATA                                         
         MVI   OMEEL,OMEELQ        BUILD DUMMY OMEEL                            
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   OMEMO(0),FVIFLD     MOVE IN THE DATA                             
         AH    RF,=Y(OMELN1Q+1)    MOVE IN THE LENGTH                           
         STC   RF,OMELN                                                         
*                                                                               
         GOTO1 VHELLO,RTPARM,(C'P',GCFILNAM),ACTRECD,OMEELD                     
         CLI   12(R1),0                                                         
         BE    OMEME               OMEEL ADDED OK                               
         DC    H'0'                                                             
         POP   USING                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR UNIT/LEDGER CODE ACTKUNT+ACTKLDG                    *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'UL/LDG'                                                      
ULCDTA   NTRDO ULCTBL,ACTKUNT,ULC                                               
*                                                                               
ULCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISULC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALULC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTULC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTULC)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTULC)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISULC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISULC   MVC   FVIFLD(L'ACTKUNT+L'ACTKLDG),ACTKUNT                              
         B     ULCE                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LEDGER FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
VALULC   MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),FVIFLD                              
*ALLDGC  MVC   ACTKLDG,FVIFLD                                                   
         MVC   T.ACTKEY,BCSPACES   READ THE LEDGER RECORD FOR T                 
         MVC   T.ACTKCPY(3),ACTKCPY     COMPANY/UNIT/LEDGER                     
         DROP  T                                                                
         GOTO1 AGETACT,0           READ LEDGER AND TEST SECURITY                
         BNE   ULCL                                                             
         B     ULCE                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTULC  MVC   FVIFLD(L'ACTKUNT+L'ACTKLDG),FLTIFLD                              
         B     ULCE                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LEDGER FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTULC  MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),FVIFLD                              
         MVC   FLTIFLD(L'ACTKUNT+L'ACTKLDG),FVIFLD                              
         B     ULCE                                                             
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LEDGER                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTULC  CLI   ACTKLDG,C' '        IS THERE A LEDGER TO COMPARE ON?             
         BNH   ULCFXX              NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),FLTIFLD                             
         BL    ULCFXL                                                           
         BE    ULCFXE                                                           
         BH    ULCFXH                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ACCDTA   NTRDO ACCTBL,ACTKACT,ACC                                               
*                                                                               
ACCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISACC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALACC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTACC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTACC)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTACC)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISACC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHACC)                               
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISACC)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY AN ACCOUNT FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISACC   MVC   FVIFLD(L'ACTKACT),ACTKACT                                        
         B     ACCE                                                             
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON AN ACCOUNT FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
SRCHACC  CLI   CSACT,A#LST         FOR ACTION LIST DO NOT SEARCH                
         BE    ACCE                IT MESSES UP THE FILTERING                   
         GOTO1 VACSRCHC,BOPARM,(3,RTPARMS5),ATWA,ACTKUNT,              *        
               ACOM,(X'14',0)                                                   
         B     ACCE                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ACCOUNT FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALACC   MVC   ACTKACT,FVIFLD                                                   
         B     ACCE                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN ACCOUNT FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTACC  MVC   FVIFLD(L'ACTKACT),FLTIFLD                                        
         B     ACCE                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ACCOUNT FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTACC  MVC   ACTKACT,FVIFLD                                                   
         MVC   FLTIFLD(L'ACTKACT),FVIFLD                                        
         B     ACCE                                                             
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON ACCOUNT FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTACC  CLC   ACTKACT,BCSPACES    IS THERE AN ACCOUNT TO COMPARE ON?           
         BNH   ACCFXX              NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   ACTKACT,FLTIFLD                                                  
         BL    ACCFXL                                                           
         BE    ACCFXE                                                           
         BH    ACCFXH                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT LENGTHS                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ACCLEN   NTRDO ACCLTBL,ACLVLEN,ACCL                                             
*                                                                               
ACCLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISACCL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL D ACCOUNT LENGTH                                      *         
***********************************************************************         
         SPACE 1                                                                
DISACCL  GOTO1 AGETEL,BOPARM,('ACLELQ',ACTRECD),0                               
         BNE   ACCLE                                                            
         PUSH  USING                                                            
         USING ACLELD,BOELEM                                                    
         XR    R4,R4                                                            
         MVI   BOBYTE1,0                                                        
         MVC   BOWORK1(80),BCSPACES                                             
         LA    R3,BOWORK1                                                       
         LA    R2,ACLVALS                                                       
*                                                                               
DISACC10 XR    R1,R1                                                            
         ICM   R1,1,ACLVLEN-ACLVALS(R2)                                         
         BZ    DISACC20                                                         
         LR    RF,R1                                                            
         XR    RE,RE                                                            
         IC    RE,BOBYTE1                                                       
         SR    R1,RE                                                            
         STC   RF,BOBYTE1                                                       
         CURED (R1),(2,(R3)),0,ALIGN=LEFT,ZERO=NOBLANK,DMCB=BOPARM              
         LA    R3,20(R3)                                                        
         LA    R4,1(R4)                                                         
         CLI   ACLVLEN-ACLVALS(R2),L'ACTKACT                                    
         BE    DISACC20                                                         
         LA    R2,L'ACLVALS(R2)                                                 
         B     DISACC10                                                         
*                                                                               
DISACC20 GOTO1 VUNSCAN,BOPARM,((R4),BOWORK1),(C'C',FVIFLD)                      
*                                                                               
         B     ACCLE                                                            
         POP   USING                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A LEDGER NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LGNDTA   NTRDO LGNTBL,LGNDTA,LDGN                                               
*                                                                               
LGNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLGN)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLGN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
TEMP     USING ACTRECD,IOKEY                                                    
DISLGN   MVC   IOKEY,BCSPACES       READ THE LEDGER RECORD FOR THIS ID          
         MVC   TEMP.ACTKCPY,ACTKCPY COMPANY                                     
         MVC   TEMP.ACTKUNT,ACTKUNT UNIT                                        
         MVC   TEMP.ACTKLDG,ACTKLDG LEDGER                                      
         DROP  TEMP                                                             
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
*                                                                               
         LH    R4,=Y(TWUSER-TWAD)                                               
         AR    R4,RA                                                            
         USING TWUSER,R4                                                        
         XC    LNLEVELS,LNLEVELS   CLEAR ACCOUNT LENGTHS WORK AREA              
         GOTO1 AGETEL,BOPARM,('ACLELQ',AIO1),0                                  
         JNE   LDGNE               NO ACCOUNT LENGTHS ELEMENT                   
         USING ACLELD,RF                                                        
         LA    RF,BOELEM                                                        
         MVC   LNLEV1,ACLVALS                                                   
         MVC   LNLEV2,ACLVALS+(L'ACLVALS*1)                                     
         MVC   LNLEV3,ACLVALS+(L'ACLVALS*2)                                     
         MVC   LNLEV4,ACLVALS+(L'ACLVALS*3)                                     
         DROP  R4                                                               
         B     LDGNE                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER NAME FIELD FROM THE FILTER                         *         
***********************************************************************         
         SPACE 1                                                                
TEMP     USING ACTRECD,IOKEY                                                    
DFLGN    CLI   FLTIFLD,C' '                                                     
         BNH   LDGNE                                                            
         MVC   IOKEY,BCSPACES       READ THE LEDGER RECORD FOR THIS ID          
         MVC   TEMP.ACTKCPY,ACTKCPY COMPANY ID IN HERE                          
         MVC   TEMP.ACTKUNT,ACTKUNT UNIT IN HERE                                
         MVC   TEMP.ACTKLDG,FLTIFLD LEDGER IN HERE                              
         DROP  TEMP                                                             
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   LDGNE                                                            
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     LDGNE                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADDED DATE (FOR LIST SCREEN ONLY)                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DTADD    NTRDO DTADDTB,DTADD,DTAD                                               
*                                                                               
DTADDTB  DC    AL1(DDIS),AL1(0,0,0),AL4(DSDTAD)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDTAD)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFDTAD)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFDTAD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ADDED DATE ON LIST SCREEN                                             
***********************************************************************         
         SPACE 1                                                                
DSDTAD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RACELQ',ACTRECD),       +        
               (L'RACTYPE,=AL1(RACTADD))                                        
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   FVIFLD(L'AC@NFA42),AC@NFA42  DISPLAY '(NONE)'                    
         B     DTADE                                                            
*                                                                               
         L     RF,12(R1)                                                        
         USING RACELD,RF                                                        
         GOTO1 VDATCON,BODMCB,(1,RACDATE),(5,FVIFLD)                            
         B     DTADE                                                            
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATE ADDED FILTER FOR LIST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
DFDTAD   MVC   FVIFLD,BCSPACES                                                  
         CLI   FLTIFLD,0                                                        
         BE    DTADE               NOTHING TO FILTER ON                         
*                                                                               
         GOTO1 VDATCON,BODMCB,(1,FLTIFLD),(5,FVIFLD)                            
         B     DTADE                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DATE ADDED FILTER                                          *         
* STORE FILTER AS A 3 BYTE PACKED DATE IN FLTIFLD                               
***********************************************************************         
         SPACE 1                                                                
VFDTAD   CLI   FVILEN,0            NO INPUT - DEFAULT IS NO FILTER              
         BE    DTADE                                                            
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         ZIC   RF,FVILEN                                                        
         GOTO1 VPERVAL,BOPARM,((RF),FVIFLD),BOWORK1                             
         CLI   BOPARM+4,X'01'                                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     DTADL                                                            
*                                                                               
         USING PERVALD,R1                                                       
         LA    R1,BOWORK1                                                       
         MVC   FLTIFLD(L'PVALPSTA),PVALPSTA                                     
         B     DTADE                                                            
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON DATE ADDED FILTER                                   *         
***********************************************************************         
         SPACE 1                                                                
DOFDTAD  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RACELQ',ACTRECD),       +        
               (L'RACTYPE,=AL1(RACTADD))                                        
         CLI   12(R1),0                                                         
         BNE   DTADFXL                                                          
*                                                                               
         L     RF,12(R1)                                                        
         USING RACELD,RF                                                        
         CLC   RACDATE,FLTIFLD                                                  
         BE    DTADFXE                                                          
         BH    DTADFXH                                                          
         B     DTADFXL                                                          
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR changed date (for list screen only)                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DTCHA    NTRDO DTCHATB,DTCHA,DTCH                                               
*                                                                               
DTCHATB  DC    AL1(DDIS),AL1(0,0,0),AL4(DSDTCH)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDTCH)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFDTCH)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFDTCH)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CHANGED DATE ON LIST SCREEN                                           
***********************************************************************         
         SPACE 1                                                                
DSDTCH   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RACELQ',ACTRECD),       +        
               (L'RACTYPE,=AL1(RACTCHA))                                        
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   FVIFLD(L'AC@NFA42),AC@NFA42  DISPLAY '(NONE)'                    
         B     DTCHE                                                            
*                                                                               
         L     RF,12(R1)                                                        
         USING RACELD,RF                                                        
         GOTO1 VDATCON,BODMCB,(1,RACDATE),(5,FVIFLD)                            
         B     DTCHE                                                            
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATE CHANGED FILTER FOR LIST SCREEN                         *         
***********************************************************************         
         SPACE 1                                                                
DFDTCH   MVC   FVIFLD,BCSPACES                                                  
         CLI   FLTIFLD,0                                                        
         BE    DTCHE               NOTHING TO FILTER ON                         
*                                                                               
         GOTO1 VDATCON,BODMCB,(1,FLTIFLD),(5,FVIFLD)                            
         B     DTCHE                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DATE CHANGED FILTER                                        *         
* STORE FILTER AS A 3 BYTE PACKED DATE IN FLTIFLD                               
***********************************************************************         
         SPACE 1                                                                
VFDTCH   CLI   FVILEN,0            NO INPUT - DEFAULT IS NO FILTER              
         BE    DTCHE                                                            
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         ZIC   RF,FVILEN                                                        
         GOTO1 VPERVAL,BOPARM,((RF),FVIFLD),BOWORK1                             
         CLI   BOPARM+4,X'01'                                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     DTCHL                                                            
*                                                                               
         USING PERVALD,R1                                                       
         LA    R1,BOWORK1                                                       
         MVC   FLTIFLD(L'PVALPSTA),PVALPSTA                                     
         B     DTCHE                                                            
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON DATE CHANGED FILTER                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFDTCH  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RACELQ',ACTRECD),       +        
               (L'RACTYPE,=AL1(RACTCHA))                                        
         CLI   12(R1),0                                                         
         BNE   DTCHFXL                                                          
*                                                                               
         L     RF,12(R1)                                                        
         USING RACELD,RF                                                        
         CLC   RACDATE,FLTIFLD                                                  
         BE    DTCHFXE                                                          
         BH    DTCHFXH                                                          
         B     DTCHFXL                                                          
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FILTER VIEW FIELD ON ACCT LIST SCREEN OR BLOCK TO   *         
* BE OUPUT ON REPORT                                                            
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DC    CL8'FILTVW'                                                      
FILTVW   NTRDO FLTVWTB,FILTVW,FLTVW                                             
*                                                                               
FLTVWTB  DC    AL1(DFDIS),AL1(0,0,0),AL4(DOFILT)  **NEED DFVAL TO               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(FLTVWE)  PREVENT DUMPS IN              
         DC    AL1(EOT)                           GEFIL01                       
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTERS ON PROTECTED FIELD OR BLOCK AREA FOR REPORTING      *         
***********************************************************************         
         SPACE 1                                                                
DOFILT   BRAS  RE,DISFVW                                                        
         B     FLTVWE                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR REQUESTOR                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'INUSER'                                                      
WHODTA   NTRDO WHOTBL,WHODTA,WHO                                                
*                                                                               
WHOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISWHO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWHO)                                 
         DC    AL1(DRDIS),AL1(0,0,0),AL4(DISWHO)                                
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALWHO)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY REQUESTOR                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISWHO   MVC   FVIFLD(L'INUSER),INUSER                                          
         CLC   INUSER,BCSPACES                                                  
         BH    WHOE                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     WHOL                NO INPUT                                     
         SPACE 2                                                                
***********************************************************************         
* VALIDATE REQUESTOR                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALWHO   MVC   INUSER,FVIFLD                                                    
         L     RF,AREP                                                          
         USING REPD,RF                                                          
         MVC   REPSUBID,INUSER                                                  
         B     WHOE                                                             
         DROP  RF                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR WHEN                                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'INWHEN'                                                      
WHNDTA   NTRDO WHNTBL,WHNDTA,WHN                                                
*                                                                               
WHNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISWHN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWHN)                                 
         DC    AL1(DRDIS),AL1(0,0,0),AL4(DISWHN)                                
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALWHN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY WHEN                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISWHN   CLI   INWHEN,INWNSOON                                                  
         BNE   *+14                                                             
         MVC   FVIFLD(L'RU@SOON),RU@SOON                                        
         B     WHNE                                                             
         CLI   INWHEN,INWNOVNT                                                  
         BNE   *+14                                                             
         MVC   FVIFLD(L'RU@OVNT),RU@OVNT                                        
         B     WHNE                                                             
         CLI   INWHEN,INWNNOW                                                   
         BNE   WHNE                                                             
         MVC   FVIFLD(L'RU@NOW),RU@NOW                                          
         B     WHNE                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE WHEN                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALWHN   GOTOX ('VALWHEN',AGROUTS)                                              
         BL    WHNL                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   INWHEN,INWNNOW                                                   
         BNE   *+12                                                             
         TM    WHENOK,WHENNOW                                                   
         BZ    WHNL                                                             
         CLI   INWHEN,INWNSOON                                                  
         BNE   *+12                                                             
         TM    WHENOK,WHENSOON                                                  
         BZ    WHNL                                                             
         CLI   INWHEN,INWNOVNT                                                  
         BNE   *+12                                                             
         TM    WHENOK,WHENOV                                                    
         BZ    WHNL                                                             
         OI    FVIIND,FVIVAL                                                    
         B     WHNE                                                             
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DESTINATION                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'INDEST'                                                      
WHRDTA   NTRDO WHRTBL,WHRDTA,WHR                                                
*                                                                               
WHRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(WHRE)                                   
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWHR)                                 
         DC    AL1(DRDIS),AL1(0,0,0),AL4(WHRE)                                  
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALWHR)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DESTINATION                                                *         
***********************************************************************         
         SPACE 1                                                                
VALWHR   GOTOX ('VALDEST',AGROUTS)                                              
         BL    WHRL                                                             
         OI    FVIIND,FVIVAL                                                    
         B     WHRE                                                             
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FEDARATED SECURITY URL LINES 1-4                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL8'FTTFURL'                                                     
FEDU     NTRDO FEDUTBL,FEDU,FED                                                 
*                                                                               
FEDUTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFEDU)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFEDU)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FEDERATED AUTHORISATION URL                                 *         
***********************************************************************         
         SPACE 1                                                                
DISFEDU  DS    0H                                                               
         ICM   RF,15,RTPARMS2      CURRENT FIELD NUMBER                         
         SR    RE,RE                                                            
         ICM   RE,3,=AL2(F#GEN#FEDU1)                                           
         AHI   RE,-1                                                            
         SR    RF,RE               RF=1-7                                       
         GOTO1 AGETFFT,RTPARM,ACTRECD,((RF),FFTTFURL)                           
         B     FEDE                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FEDERATED URL LINE N                                       *         
***********************************************************************         
         SPACE 1                                                                
VALFEDU  DS    0H                                                               
         ICM   RF,15,RTPARMS2      CURRENT FIELD NUMBER                         
         SR    RE,RE                                                            
         ICM   RE,3,=AL2(F#GEN#FEDU1)                                           
         AHI   RE,-1                                                            
         SR    RF,RE               RF=1-7                                       
         GOTO1 ABLDFFT,RTPARM,ACTRECD,((RF),FFTTFURL)                           
         B     FEDX                                                             
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* CALL THE FILTER VIEW ROUTINE LOOKING FOR THE ACCOUNT CODE FIELD     *         
* IN ORDER TO DISPLAY IN THE REPORT HEADLINES                                   
***********************************************************************         
         SPACE 1                                                                
GETACCF  NTR1  BASE=*,LABEL=*                                                   
         OI    BYTE,FLTHD                                                       
         MVC   SVFLTACC,BCSPACES                                                
         MVC   SVFLDNM,=AL2(ACC#ACCDE) ACCOUNT CODE FIELD #                     
         BRAS  RE,DISFVW                                                        
         NI    BYTE,X'FF'-FLTHD                                                 
         OC    SVFLTER2,SVFLTER2                                                
         BZ    GETACCFX                                                         
         MVC   SVFLTACC,SVFLTER2                                                
*                                                                               
GETACCFX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK LOW LEVEL ACCOUNT AND PROTECT FIELD FOR CHANGE ACTION                   
***********************************************************************         
         SPACE 1                                                                
         USING TWUSER,R4                                                        
CKLOPRO  NTR1  BASE=*,LABEL=*                                                   
         LH    R4,=Y(TWUSER-TWAD)                                               
         AR    R4,RA                                                            
         CLI   CSACT,A#DIS         IS IT A DISPLAY?                             
         JE    *+12                NO                                           
         CLI   CSACT,A#CHA         IS IT A CHANGE ?                             
         JNE   CKLOPX              NO - GET OUT                                 
         CLC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'1R' 1R RECORD ?                  
         JNE   CKLOPX              NO - GET OUT                                 
         MVC   TLEVS,LNLEVELS      TEMPORARY LEVEL LENGTHS                      
         BRAS  RE,CHKACLN          LOW LEVEL ACCOUNT ?                          
         JNE   CKLOPX              NO - GET OUT                                 
         OI    FVATRB,FVAPROT      PROTECT IF LOW LVL 1R ACCT                   
CKLOPX   XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TEST NEW OR EXISTING ACCOUNT IS LOW-LEVEL                           *         
* TLEVS CONTAINS THE LEVEL LENGTHS                                              
* XIT - EQUAL IF LOW LEVEL ACCOUNT                                              
*       LOW IF NOT LOW LEVEL ACCOUNT                                            
***********************************************************************         
         SPACE 1                                                                
         USING TWUSER,R4                                                        
CHKACLN  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,L'ACTKACT           12 MAX FOR THE ACCOUNT                    
         LA    RF,ACTKACT+L'ACTKACT-1 POINT TO LAST BYTE                        
         CLI   0(RF),C' '             FIND OUT HOW MANY CHARACTERS              
         BNE   *+10                   THE USER ENTERED                          
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         CLI   TLEV1,L'ACTKACT     ONE LEVEL LEDGER?                            
         BE    CHKACE                                                           
         ZIC   RF,TLEV3            IF 4 LEVEL LEDGER MUST CHECK                 
         CLI   TLEV4,0             3RD LEVEL                                    
         BNE   CHKAC10                                                          
         ZIC   RF,TLEV2            IF 3 LEVEL LEDGER MUST CHECK                 
         CLI   TLEV3,0             2ND LEVEL                                    
         BNE   CHKAC10                                                          
         ZIC   RF,TLEV1            CAN ASSUME A 2 LEVEL LEDGER SO               
CHKAC10  CR    R1,RF               R1 CONTAINS LENGTH USER ENTERED              
         BH    CHKACE                                                           
*                                                                               
CHKACL   CLI   *,FF                                                             
         B     CHKACX                                                           
CHKACE   CR    RB,RB                                                            
CHKACX   XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD THE SAVED FILTER VIEW LINE (SVFLTVW) TO THE DOWNLOAD            *         
***********************************************************************         
         SPACE 1                                                                
         USING DLCBD,R4                                                         
DLDFLT   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,DLCB                                                          
         MVC   FVIFLD(L'SVFLTVW),SVFLTVW                                        
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         LA    R1,L'FVIFLD         REPLACE ANY `"` WITH `'` IN                  
         CLI   0(RF),C'"'          ELSE DOWNLOAD NOT HAPPY...                   
         BNE   *+8                                                              
         MVI   0(RF),C''''                                                      
         BCTR  RF,0                                                             
         BCT   R1,*-14                                                          
*                                                                               
         MVI   DLCBFLX,C' '        FILL FIELD WITH SPACES                       
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
         MVC   DLCBFLX(L'FVIFLD),FVIFLD                                         
         MVI   DLCBACT,DLCBPUT     PUTTING TEXT (FOR NOW)                       
         MVI   DLCBTYP,DLCBTXT                                                  
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN                                       
         GOTOX VDLFLD,DLCBD                                                     
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF FILTER TEXT LINE                      
         GOTOX VDLFLD,DLCBD                                                     
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK TO SEE IF ANY OF THE COLUMNS ARE SECURITY SENSITIVE           *         
***********************************************************************         
         SPACE 1                                                                
         USING DCTABD,R2                                                        
         USING REPD,R4                                                          
COLSEC   NTR1  BASE=*,LABEL=*                                                   
         XR    R0,R0                                                            
         ICM   R0,3,LSFIXNUM       NUMBER OF FIXED COLUMNS                      
         BZ    COLS10                                                           
         STCM  R0,3,SVNUMCOL       SAVE # OF FIXED COLS                         
         LA    R2,LSFIXCLM         POINT TO FIXED COLUMNS                       
         OI    BYTE,FIXCLM         PROCESSING FIXED COLUMNS                     
         B     COLS20                                                           
*                                                                               
COLS10   ICM   R0,3,LSVARNUM       NUMBER OF VARIABLE COLUMNS                   
         BNZ   *+6                                                              
         DC    H'0'                NEED SOMETHING                               
         STCM  R0,3,SVNUMCOL       SAVE # OF FIXED COLS                         
         LA    R2,LSVARCLM                                                      
*                                                                               
COLS20   DS    0H                                                               
         CLC   DCTFLD#,=AL2(ACC#OTNUM) SS/TIN?                                  
         BE    COLS30                                                           
         CLC   DCTFLD#,=AL2(ACC#BPYACN) BANK ACCOUNT NUMBER PAYABLES            
         BE    COLS30                                                           
         CLC   DCTFLD#,=AL2(ACC#BSCACN) BANK ACCOUNT NUMBER SC                  
         BE    COLS30                                                           
         CLC   DCTFLD#,=AL2(ACC#ROUTN)  BANK ROUTING NUMBER                     
         BE    COLS30                                                           
*                                                                               
         LA    R2,DCTABL(R2)       NEXT COLUMN FOR THIS LINE                    
         SR    R1,R1                                                            
         ICM   R1,3,SVNUMCOL                                                    
         BCTR  R1,0                                                             
         STCM  R1,3,SVNUMCOL                                                    
         LTR   R1,R1               # OF COLS LEFT TO PROCESS                    
         BNZ   COLS20                                                           
         TM    BYTE,FIXCLM         DID WE JUST FINISH THE FIXED COLS?           
         BZ    COLSX               NO SO DONE                                   
         NI    BYTE,X'FF'-FIXCLM                                                
         SR    R0,R0                                                            
         ICM   R0,3,LSVARNUM       NO VARIABLE COLUMNS (IS THIS                 
         BZ    COLSX               POSSIBLE?)                                   
         STCM  R0,3,SVNUMCOL         SAVE # OF VARIABLE COLUMNS                 
         LA    R2,LSVARCLM                                                      
         B     COLS20                                                           
*                                                                               
COLS30   MVI   REPPSWD,X'FF'           SET SECURITY ON PQ SO ONLY USER          
         MVI   REPSECF1,REPSSSN        WHO REQUESTED A REPORT CAN SEE           
         MVI   REPSECF2,0                                                       
*                                                                               
COLSX    XIT1                                                                   
         DROP  R2,R4                                                            
***********************************************************************         
* DISPLAY THE FILTERS ON PROTECTED FIELD (FOR ACCOUNT LIST SCREEN) OR           
* MOVE INTO SVFLTVWH FIELD FOR REPORT                                           
* USE AIO5 TO BUILD BLOCK AND PASS TO UNSCAN                          *         
* USE 20 AS THE LENGTH FOR THE RHS OF THE FILTER (DEFAULT FOR UNSCAN  *         
* IS 10.                                                                        
***********************************************************************         
         SPACE 1                                                                
DISFVW   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,AIO5                                                          
         LR    RE,R3               CLEAR IO5                                    
         LA    RF,IOAREALN                                                      
         XCEF                                                                   
*                                                                               
         XC    DISPL,DISPL                                                      
         XC    BCBYTE1,BCBYTE1     KEEP TRACK OF UNSCAN ENTRIES                 
         LH    RE,=Y(FLTELSV-TWAD)                                              
         A     RE,ATWA             RE POINTS TO FILTER BLOCK LENGTH             
         LH    RF,0(RE)            RF CONTAINS FILTER BLOCK LENGTH+1            
         BCTR  RF,0                SUBTRACT FOR TRUE LENGTH                     
         AR    RF,RE               POINT TO END OF TABLE                        
         ST    RF,TABEND           SAVE END OF TABLE                            
*                                                                               
         USING FLTELD,R4                                                        
         USING FLTCDD,R1                                                        
         LH    R4,=Y(FLTELSDT-TWAD)                                             
         A     R4,ATWA             R4 POINTS TO FILTER BLOCK                    
         ST    R4,SVADDR                                                        
         NI    BYTE,X'FF'-COMMA                                                 
DFVW05   XC    SVFLTER2,SVFLTER2                                                
         XC    DISPL,DISPL                                                      
         NI    BYTE,X'FF'-(RANGE1+RANGE2+COMMA)                                 
         CLC   SVADDR,TABEND       IF CURRENT ADDR IS BEYOND FILTER             
         BNL   DFVW200             BLOCK END - DONE                             
         MVC   SVFLTCNT,FLTCNT     SAVE # OF FILTERS IN THE LIST                
         MVC   SVFLTMAX,FLT1LEN    MAX LENGTH OF THIS FILTER                    
         LA    R1,FLTINFO          INFO FOR EACH ENTRY                          
         ST    R1,SVINFADR                                                      
         TM    BYTE,FLTHD          BUILDING HEADLINE FROM FILTER?               
         BZ    *+18                NO                                           
         CLC   FLTNUM,SVFLDNM      YES THAN CLC ON SAVED FLD # INSTEAD          
         BE    DFVW20              OF FILTER TABLE                              
         B     DFVW195                                                          
*                                                                               
         LA    R1,FLTCD            R1 POINTS TO TABLE OF FILTER CODES           
DFVW10   CLI   0(R1),X'FF'                                                      
         BE    DFVW195                                                          
*                                                                               
         CLC   FLTNUM,FLTCID       COMPARE ON FIELD NUMBER                      
         BE    DFVW20                                                           
DFVW12   LA    R1,FLTCLNQ(R1)       BUMP TO NEXT ENTRY                          
         B     DFVW10                                                           
*                                                                               
DFVW20   DS    0H                                                               
         TM    BYTE,FLTHD          DOING REP HDLINES?                           
         BO    DFVW20B             YES                                          
         TM    GENINDS,GENIREP+GENIDLD   DOING A REPORT OR DOWNLOAD?            
         BZ    DFVW20A             NO                                           
         TM    GENINDS,GENIDLD     DOING A DOWNLOAD?                            
         BO    DFVW20B             YES SO ALWAYS ADD TO LINE                    
         TM    FLTBYTE,FLTHDL      IF THIS FLTR IS ONLY FOR REP HDLINES         
         BO    DFVW12              DON'T ADD IT TO THE FILTER VIEW LINE         
         B     DFVW20B                                                          
*                                                                               
DFVW20A  TM    FLTBYTE,FLTNOLST    FOR A LIST DON'T INCLUDE THE QUICK           
         BO    DFVW12              FILTERS (E.G. F1,F2, ETC.)                   
DFVW20B  MVC   SVFLTNM,FLTNUM      SAVE THIS FIELD NUMBER                       
         MVC   0(L'FLTNAME,R3),FLTNAME                                          
*                                                                               
* IF FILTER IS A LIST                                                           
         USING FLTINFO,R1                                                       
DFVW22   L     R1,SVINFADR                                                      
         MVC   SVFIND2,FLTIND2                                                  
         TM    FLTIND2,FDRF2LST    FILTER IS A LIST?                            
         BZ    DFVW50              NO                                           
         TM    FLTIND1,(FDRF1NOT+FDRF1EQ+FDRF1GT+FDRF1LT) THESE                 
         BNZ   DFVW50              BITS MUST BE OFF TO BE A LIST                
         TM    FLTIND2,FDRF2V1+FDRF2V2 THESE BITS MUST BE OFF TO BE             
         BNZ   DFVW50                  A LIST                                   
*                                                                               
         BRAS  RE,SVFILT           ISOLATE THE FILTER HERE                      
         BRAS  RE,BLDFILT          BUILD WHOLE FILTER HERE                      
         BNE   DFVW190             NO MORE TO PROCESS FOR THIS FLD              
         B     DFVW22              PROCESS NEXT LIST ENTRY                      
*                                                                               
* IF FILTER IS A RANGE                                                          
         USING FLTINFO,R1                                                       
DFVW50   L     R1,SVINFADR                                                      
         TM    FLTIND2,FDRF2RNG          RANGE IS VALID BIT MUST BE ON          
         BZ    DFVW60                     MUST BE ON FOR A RANGE                
         TM    FLTIND2,(FDRF2V1+FDRF2V2)  AT LEAST ONE OF THESE BITS            
         BZ    DFVW60                     MUST BE ON FOR A RANGE                
         TM    FLTIND1,(FDRF1NOT+FDRF1EQ+FDRF1GT+FDRF1LT) THESE                 
         BNZ   DFVW60              BITS MUST BE OFF TO BE A RANGE               
         OI    BYTE,RANGE1         PROCESS FIRST PART OF RANGE                  
*                                                                               
DFVW55   BRAS  RE,SVFILT                                                        
         LA    R1,SVFLTER2         BUILD WHOLE FILTER HERE                      
         AH    R1,DISPL                                                         
         TM    BYTE,COMMA                                                       
         BZ    *+12                                                             
         MVI   0(R1),C','                                                       
         AHI   R1,1                                                             
         TM    SVFIND2,(FDRF2V1+FDRF2V2)  ENTERED A 2 SIDED RANGE?              
         BO    DFVW55A                    YES-CONTINUE AS NORMAL                
         TM    SVFIND2,FDRF2V2            ENTERED -DECXX/XX?                    
         BZ    DFVW55A                    NO                                    
         MVI   0(R1),C'-'                                                       
         AHI   R1,1                                                             
         B     DFVW55B                                                          
DFVW55A  TM    BYTE,RANGE1                                                      
         BO    *+12                                                             
         MVI   0(R1),C'-'                                                       
         AHI   R1,1                                                             
DFVW55B  ZIC   RF,TRUELEN          RF CONTAINS TRUE LENGTH OF FILTER            
         BCTR  RF,0                                                             
         EXMVC RF,0(R1),SVFLTER                                                 
         AHI   RF,1                BUMP BACK TO REAL LENGTH                     
         TM    SVFIND2,(FDRF2V1+FDRF2V2)  ENTERED A 2 SIDED RANGE?              
         BO    DFVW55C                    YES - CONTINUE AS NORMAL              
         TM    SVFIND2,FDRF2V1            ENTERED JANXX/XX-                     
         BZ    DFVW55C                    NO                                    
         LR    RE,R1               USE RE TEMPORARILY TO MOVE IN THE            
         AR    RE,RF               HYPHEN                                       
         MVI   0(RE),C'-'                                                       
         AHI   RF,1                                                             
DFVW55C  LA    R2,SVFLTER2                                                      
         AR    R1,RF                                                            
         SR    R1,R2                                                            
         STH   R1,DISPL                                                         
         TM    SVFIND2,(FDRF2V1+FDRF2V2)  ENTERED A 2 SIDED RANGE?              
         BM    DFVW55D                    NO                                    
         TM    BYTE,RANGE1        IF JUST FINISHED PROCESSING 1ST PART          
         BO    DFVW57              OF RANGE DON'T DECREMENT COUNTER YET         
DFVW55D  ZIC   R1,SVFLTCNT         # OF FILTERS IN THIS LIST SEQUENCE           
         BCTR  R1,0                                                             
         STC   R1,SVFLTCNT                                                      
DFVW57   CLI   SVFLTCNT,0                                                       
         BNH   DFVW190                                                          
         L     R1,SVINFADR         POINT R1 TO CURRENT FLTR BLOCK ENTRY         
         ZIC   RF,SVFLTMAX         BUMP PAST FILTER DATA                        
         TM    BYTE,RANGE1         THE RHS OF THE RANGE DOES NOT HAVE           
         BZ    *+8                 THE 2 INDICATOR BYTES IN THE BLOCK           
         AHI   RF,L'FLTIND1+L'FLTIND2  SO DON'T BUMP PAST THEM                  
         AR    R1,RF                                                            
         ST    R1,SVINFADR         KEEP POINTER TO SUBSEQUENT ENTRIES           
         TM    SVFIND2,(FDRF2V1+FDRF2V2)  ENTERED A 2 SIDED RANGE?              
         BZ    DFVW58                     NO SO FINISHED PROCESSING THE         
         TM    BYTE,RANGE1                RANGE                                 
         BZ    *+16                                                             
         NI    BYTE,X'FF'-(RANGE1+COMMA)   SETUP TO PROCESS RHS OF              
         OI    BYTE,RANGE2                 RANGE                                
         B     DFVW55              PROCESS 2ND HALF OF RANGE                    
DFVW58   NI    BYTE,X'FF'-(RANGE1+RANGE2)                                       
         OI    BYTE,COMMA                                                       
         B     DFVW22                   PROCESS NEXT FILTER FOR FIELD           
*                                                                               
* FOR THE REST OF THE FILTERS >, >=, <, <=, *, =                                
         USING FLTINFO,R1                                                       
DFVW60   L     R1,SVINFADR                                                      
         BRAS  RE,SVFILT                                                        
         BRAS  RE,BLDFILT          BUILD WHOLE FILTER HERE                      
         BNE   DFVW190             NO MORE TO PROCESS FOR THIS FLD              
         B     DFVW22              PROCESS NEXT LIST ENTRY                      
*                                                                               
DFVW190  TM    BYTE,FLTHD          BUILDING HDLINES FOR REPORT?                 
         BO    DISFVWX             YES THAN DONE                                
*                                                                               
         MVC   10(L'SVFLTER2,R3),SVFLTER2  MOVE RHS TO UNSCAN BLOCK             
         ZIC   R1,BCBYTE1                                                       
         AHI   R1,1                                                             
         STC   R1,BCBYTE1                                                       
         LA    R3,30(R3)           BUMP TO NEXT UNSCAN LINE                     
DFVW195  L     R4,SVADDR           POINT R4 BACK TO FILTER BLOCK                
         ZIC   R1,1(R4)            LENGTH OF THIS ENTRY                         
         AR    R4,R1               BUMP TO NEXT ENTRY                           
         ST    R4,SVADDR           SAVE NEW ENTRY ADDRESS                       
         B     DFVW05                                                           
         DROP  R1                                                               
*                                                                               
DFVW200  ZIC   RF,BCBYTE1                                                       
         LTR   RF,RF                                                            
         BZ    DISFVWX                                                          
         TM    GENINDS,GENIREP+GENIDLD  DOING A REPORT OR DOWNLOAD?             
         BZ    DFVW210                                                          
         MVC   SVFLTVWH,BCSPACES                                                
         MVC   SVFLTVW,BCSPACES                                                 
         USING FVIHDR,R3           OUPUT THE BLOCK TO SVFLTVW.                  
         LA    R3,SVFLTVWH         NEED TO BUILD DUMMY HEADER                   
         XC    SVFLTVWH,SVFLTVWH   UNSCAN ONLY CARES ABOUT FIELD                
         MVI   FVTLEN,L'SVFLTVW+8  LENGTH                                       
         DROP  R3                                                               
         GOTO1 VUNSCAN,BOPARM,((RF),AIO5),(20,SVFLTVWH),C',= ='                 
         B     DFVW220                                                          
*                                                                               
DFVW210  GOTO1 VUNSCAN,BOPARM,((RF),AIO5),(20,FVIHDR),C',= ='                   
DFVW220  CLI   0(R1),0             DID ALL FILTERS FIT?                         
         BE    DISFVWX             YES                                          
*                                  NO DISPLAY ... TO INDICATE MORE              
         LA    R1,FVIFLD                                                        
         TM    GENINDS,GENIREP+GENIDLD  DOING A REPORT OR DOWNLOAD?             
         BZ    *+8                                                              
         LA    R1,SVFLTVWH              THAN THE FILTERS ARE HERE               
         ZIC   RF,0(R1)            R1=L'FIELD HEADER+L'FIELD                    
         SHI   RF,8                SUBTRACT 8 FOR HEADER                        
         MVC   BCBYTE1,1(R1)                                                    
         TM    BCBYTE1,FVAXTND     EXTENDED HEADER?                             
         BZ    *+8                 NO                                           
         SHI   RF,8                YES - SUBTRACT 8 MORE                        
         AR    R1,RF               POINT TO END OF FIELD                        
         STCM  R1,15,BOADDR1       SAVE THIS POINT IN THE FIELD                 
DFVW280  CLI   0(R1),C' '          ANYTHING AT THIS POINT?                      
         BH    DFVW290             YES                                          
         BCTR  R1,0                NO BUMP BACK ONE                             
         B     DFVW280                                                          
*                                                                               
DFVW290  SR    RF,RF                                                            
         ICM   RF,15,BOADDR1       FIGURE OUT HOW MANY EMPTY SPACES             
         CR    RF,R1               IS ENTIRE LINE FILLED?                       
         BE    DISFVWX             YES SO DONE                                  
         SR    RF,R1               RF=# OF EMPTY SPACES                         
         MVC   FILLER(1),=C'.'                                                  
         CHI   RF,1                ONE EMPTY SPACE?                             
         BE    DFVW300                                                          
         MVC   FILLER(2),=C'..'    TWO EMPTY SPACES?                            
         CHI   RF,2                                                             
         BE    DFVW300                                                          
         LA    RF,3                THREE OR MORE EMPTY SPACES                   
         MVC   FILLER(3),=C'...'                                                
*                                                                               
DFVW300  AHI   R1,1                BUMP TO FIRST EMPTY SPACE                    
         BCTR  RF,0                                                             
         EXMVC RF,0(R1),FILLER                                                  
*                                                                               
DISFVWX  XIT1                                                                   
         SPACE 2                                                                
*                                                                               
* FILTER TABLE                                                                  
FLTCD    DC    0C                                                               
         DC    AL2(ACC#ACCDE),X'C0'   ACCOUNT CODE (FOR HDLINES OF THE          
         DCDD  AC#AFM43,8             REPORT ONLY)   'AccCd'                    
         DC    AL2(ACC#UL),X'C0'      Unit and Ledger                           
         DCDD  AC#UNTLD,8             'UL'                                      
         DC    AL2(ACC#BBFWD),X'00'   ADDED OR PEELED DATE                      
         DCDD  AC#AFM3,8              'Add/Peel'                                
         DC    AL2(ACC#EQCD1),X'00'   ACC EQU CODE 1                            
         DCDD  AC#AFM4,8              'AE-A'                                    
         DC    AL2(ACC#EQCD2),X'00'   ACC EQU CODE 2                            
         DCDD  AC#AFM5,8              'AE-B'                                    
         DC    AL2(ACC#EQCD3),X'00'   ACC EQU CODE 3                            
         DCDD  AC#AFM6,8              'AE-C'                                    
         DC    AL2(ACC#EQCD4),X'00'   ACC EQU CODE 4                            
         DCDD  AC#AFM7,8              'AE-D'                                    
         DC    AL2(ACC#EQCD5),X'00'   ACC EQU CODE 5                            
         DCDD  AC#AFM8,8              'AE-E'                                    
         DC    AL2(ACC#CHADT),X'00'   LAST CHANGED DATE                         
         DCDD  AC#AFM14,8             'LstChg'                                  
         DC    AL2(ACC#ACTDT),X'00'   LAST TRANSACTION DATE                     
         DCDD  AC#AFM15,8             'LstTrn'                                  
         DC    AL2(ACC#ACIL),X'00'    PREVENT POSTINGS (LOCKED)                 
         DCDD  AC#AFM16,8             'PrvntPst'                                
         DC    AL2(ACC#OUTF),X'00'    BANK Pospay FILE                          
         DCDD  AC#AFM59,8             'Pospay'                                  
         DC    AL2(ACC#CLTYP),X'00'   CLIENT TYPE                               
         DCDD  AC#AFM18,8             'CliTyp'                                  
         DC    AL2(ACC#ANAL1R),X'00'  DIRECT LABOR ACCOUNT (1R)                 
         DCDD  AC#AFM19,8             'DirLab'                                  
         DC    AL2(ACC#ETYPE),X'00'   EMPLOYEE TYPE                             
         DCDD  AC#AFM20,8             'EmpTyp'                                  
         DC    AL2(ACC#ANAL1N),X'00'  INDIRECT TIME TYPE (1N)                   
         DCDD  AC#AFM21,8             'IndTim'                                  
         DC    AL2(ACC#ACTL),X'00'    STANDARD HOURS                            
         DCDD  AC#APG68,8             'StdHrs'                                  
         DC    AL2(ACC#MAIL),X'00'    CHECK DELIVERY                            
         DCDD  AC#AFM22,8             '820CkDel'                                
         DC    AL2(ACC#PAYEXL),X'00'  PAYMENT INSTRUCTIONS                      
         DCDD  AC#AFM23,8             'PayInst'                                 
         DC    AL2(ACC#CSTNG),X'00'   CLIENT ANALYSIS                           
         DCDD  AC#AFM24,8             'Cli'                                     
         DC    AL2(ACC#DEPT),X'00'    DEPARTMENT ANALYSIS                       
         DCDD  AC#AFM25,8             'Dept'                                    
         DC    AL2(ACC#ANALSE),X'00'  DIRECT EXPENSE ACCOUNT (SE)               
         DCDD  AC#AFM26,8             'DirExp'                                  
         DC    AL2(ACC#GPEI),X'00'    PERSONAL ANALYSIS                         
         DCDD  AC#AFM27,8             'Per'                                     
         DC    AL2(ACC#GACC),X'00'    GL ACCOUNT                                
         DCDD  AC#AFM28,8             'GLAcc'                                   
         DC    AL2(ACC#GLOFF),X'00'   GL OFFICE                                 
         DCDD  AC#AFM29,8             'GLOff'                                   
         DC    AL2(ACC#ANALSI),X'00'  BILL/REV ACCOUNT                          
         DCDD  AC#AFM30,8             'Bill/Rev'                                
         DC    AL2(ACC#PRCR),X'00'    REQUIRE RECONCILIATION ON                 
         DCDD  AC#AFM31,8             'ReqRec'                                  
         DC    AL2(ACC#COMM),X'00'    COMMISSION RATE (UNIT 3)                  
         DCDD  AC#AFM32,8             'CommRt'                                  
         DC    AL2(ACC#COST1C),X'00'  COSTING ACCOUNT (UNIT 3)                  
         DCDD  AC#AFM33,8             'CstAcc'                                  
         DC    AL2(ACC#RECV),X'00'    RECEIVABLE ACCOUNT (UNIT 3)               
         DCDD  AC#AFM34,8             'RcvAcc'                                  
         DC    AL2(ACC#TASK),X'00'    DEFAULT TASK CODE                         
         DCDD  AC#AFM35,8             'DfltTsk'                                 
         DC    AL2(ACC#OIN),X'00'     TIME INCOME ACCOUNT                       
         DCDD  AC#AFM36,8             'IncAcc'                                  
         DC    AL2(ACC#TMSLV),X'00'   REQUIRED ENTRY LEVEL                      
         DCDD  AC#AFM37,8             'ReqEnt'                                  
         DC    AL2(ACC#OWO),X'00'     TIME WRITE-OFF ACCOUNT                    
         DCDD  AC#AFM38,8             'W/OAcc'                                  
         DC    AL2(ACC#DISC),X'00'    CASH DISCOUNT PERCENT                     
         DCDD  AC#CDPCT,8             'CD%'                                     
         DC    AL2(ACC#GSTC),X'00'    GST CODE OVERRIDE                         
         DCDD  AC#VAT,8               'GST'                                     
         DC    AL2(ACC#PSTC),X'00'    PROVINCE/PST CODE                         
         DCDD  AC#PST,8               'PST'                                     
         DC    AL2(ACC#VTYPE),X'00'   VENDOR TYPE                               
         DCDD  AC#AFM39,8             'VenTyp'                                  
         DC    AL2(ACC#1099),X'00'    COMPENSATION TYPE                         
         DCDD  AC#AFM40,8             'CompTy'                                  
         DC    AL2(ACC#OTNUM),X'00'   TAX ID NUMBER                             
         DCDD  AC#AFM41,8             'TxID'                                    
         DC    AL2(ACC#TXTYPE),X'00'  TAX ID TYPE                               
         DCDD  AC#AFM42,8             'TxTyp'                                   
         DC    AL2(ACC#VEND2C),X'00'  1099 VENDOR                               
         DCDD  AC#1099,8              '1099'                                    
         DC    AL2(ACC#FLT1),X'80'    FILTER 1                                  
         DCDD  AC#AFM9,8              'F1'                                      
         DC    AL2(ACC#FLT2),X'80'    FILTER 2                                  
         DCDD  AC#AFM10,8             'F2'                                      
         DC    AL2(ACC#FLT3),X'80'    FILTER 3                                  
         DCDD  AC#AFM11,8             'F3'                                      
         DC    AL2(ACC#FLT4),X'80'    FILTER 4                                  
         DCDD  AC#AFM12,8             'F4'                                      
         DC    AL2(ACC#FLT5),X'80'    FILTER 5                                  
         DCDD  AC#AFM13,8             'F5'                                      
         DC    AL2(ACC#NAME),X'80'    ACCOUNT NAME                              
         DCDD  AC#AFM2,8              'AccNm'                                   
         DC    AL2(ACC#LVFLT),X'80'   ACCOUNT LEVEL                             
         DCDD  AC#AFM1,8              'AccLvl'                                  
         DC    AL2(ACC#DUEF),X'00'    DUE DATE FORMULA                          
         DCDD  AC#NF118,8             'Duedt'                                   
         DC    AL2(ACC#RCLEAR),X'00'  REFRESH W/ CLEARANCE                      
         DCDD  AC#NF125,8             'RfshClr'                                 
         DC    AL2(ACC#CNTNM),X'00'   CONTACT                                   
         DCDD  AC#CONTN,8             'Contact'                                 
         DC    AL2(ACC#FAXNO),X'00'   FAX                                       
         DCDD  AC#FAX,8               'Fax'                                     
         DC    AL2(ACC#PHONE),X'00'   PHONE                                     
         DCDD  AC#PHONE,8             'Phone'                                   
         DC    AL2(ACC#BPYACN),X'00'  BANK ACCOUNT (PAYABLES)                   
         DCDD  AC#AFM72,8             'Bnkacc'                                  
         DC    AL2(ACC#BSCACN),X'00'  BANK ACCOUNT (SC)                         
         DCDD  AC#AFM72,8             'Bnkacc'                                  
         DC    AL2(ACC#PAYMTH),X'00'  PAYMENT METHOD (PAYABLES)                 
         DCDD  AC#AFM73,8             'Paymeth'                                 
         DC    AL2(ACC#RMDELI),X'00'  REMITTANCE DELIVERY (PAYBLES)             
         DCDD  AC#AFM74,8             'Rmtdlvry'                                
         DC    AL2(ACC#ROUTN),X'00'   BANK ROUTING NUMBER (PAYABLES)            
         DCDD  AC#AFM94,8             'Bnkroute'                                
         DC    AL2(ACC#BNKCD),X'00'   BANK/BRANCH CODE (SC)                     
         DCDD  AC#AFM95,8             'BnkBrch'                                 
         DC    AL2(ACC#820DSN),X'00'  820 DATASET NAME                          
         DCDD  AC#AFM97,8             '820DstNm'                                
         DC    AL2(ACC#820MC),X'00'   820 MESSAGE CLASS                         
         DCDD  AC#AFM98,8             '820MsgCl'                                
         DC    AL2(ACC#CRDSN),X'00'   ACR DATASET NAME                          
         DCDD  AC#AFM69,8             'ACRDstNm'                                
         DC    AL2(ACC#CRMC),X'00'    ACR MESSAGE CLASS                         
         DCDD  AC#AFM71,8             'ACRMsgCl'                                
         DC    AL2(ACC#EFDSN),X'00'   EFT DATASET NAME                          
         DCDD  AC#AFMA2,8             'EFTDstNm'                                
         DC    AL2(ACC#EFMC),X'00'    EFT MESSAGE CLASS                         
         DCDD  AC#AFMA3,8             'EFTMsgCl'                                
         DC    AL2(ACC#PPDSN),X'00'   POSPAY DATASET NAME                       
         DCDD  AC#AFMA9,8             'POSDstNm'                                
         DC    AL2(ACC#PPMC),X'00'    POSPAY MESSAGE CLASS                      
         DCDD  AC#AFMA0,8             'POSMsgCl'                                
         DC    AL2(ACC#LCHGBY),X'00'  LAST CHANGED BY                           
         DCDD  AC#AFM96,8             'LstChaBy'                                
         DC    AL2(ACC#RVSGN),X'00'   GL REVERSE SIGN OF BALANCE                
         DCDD  AC#NF138,8             'RevSign'                                 
         DC    X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FORMAT A DATE FILTER INTO THE PROPER DISPLAY FORMAT FOR PROTECTED             
* FILTER DISPLAY LINE                                                           
* NTRY:  R1 POINTS TO FILTER DATA                                               
* EXIT:  SVFLTER CONTAINS THE FILTER                                            
***********************************************************************         
         SPACE 1                                                                
         USING FLTINFO,R1                                                       
DISDAT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         ZIC   RF,SVFLTMAX                                                      
         BCTR  RF,0                                                             
         EXMVC RF,BOWORK1,0(R1)                                                 
         GOTO1 VDATCON,BODMCB,(1,BOWORK1),(5,SVFLTER)                           
         XIT1                                                                   
         SPACE 2                                                                
***********************************************************************         
* FORMAT THE RETAIL COMMISSION RATE PROFILE IN THE PROPER DISPLAY               
* FORMAT FOR THE FILTER DISPLAY LINE                                            
* NTRY:  R1 POINTS TO FILTER DATA                                               
* EXIT:  SVFLTER CONTAINS THE FILTER                                            
***********************************************************************         
         SPACE 1                                                                
         USING FLTINFO,R1                                                       
DISCOMM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   0(L'AC@NFA94,R1),AC@NFA94   OPT/MAINT                            
         BNE   *+14                                                             
         MVC   SVFLTER(L'AC@NFA94),AC@NFA94                                     
         B     DISCOMMX                                                         
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         UNPK  BOWORK1(6),0(4,R1)                                               
         OI    BOWORK1+5,X'F0'                                                  
         LA    RF,SVFLTER                                                       
         CLI   BOWORK1,C'0'            TENS  DIGIT ZERO ?                       
         BE    DSCOMM10                                                         
         MVC   0(1,RF),BOWORK1        MOVE  TENS  DIGIT                         
         LA    RF,1(,RF)              NEXT  'TO'  CHARACTER                     
*                                                                               
DSCOMM10 CLI   BOWORK1+1,C'0'          UNITS DIGIT ZERO ?                       
         BNE   DSCOMM20               NO,   MOVE  IT                            
         CLI   BOWORK1,C'0'            WAS   TENS  DIGIT ALSO ZERO ?            
         BE    DSCOMM30               YES,  SKIP  UNITS DIGIT                   
*                                                                               
DSCOMM20 MVC   0(1,RF),BOWORK1+1       MOVE  UNITS DIGIT                        
         LA    RF,1(,RF)              NEXT  'TO'  CHARACTER                     
DSCOMM30 MVI   0(RF),C'.'             INSERT      DECIMAL   POINT               
         MVC   1(4,RF),BOWORK1+2      MOVE  THE   REST OF   THE  NUMBER         
         MVI   5(RF),C'%'                                                       
*                                                                               
DISCOMMX XIT1                                                                   
         SPACE 2                                                                
***********************************************************************         
* FORMAT THE VENDOR CASH DISCOUNT PERCENT PROFILE IN THE PROPER DISPLAY         
* FORMAT FOR THE FILTER DISPLAY LINE                                            
* NTRY:  R1 POINTS TO FILTER DATA                                               
* EXIT:  SVFLTER CONTAINS THE FILTER                                            
***********************************************************************         
         SPACE 1                                                                
         USING FLTINFO,R1                                                       
DISPCT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   0(L'AC@NONE,R1),AC@NONE                                          
         BNE   *+14                                                             
         MVC   SVFLTER(L'AC@NONE),AC@NONE                                       
         B     DISPCTX                                                          
         XC    BOWORK1,BOWORK1                                                  
         ZIC   RF,SVFLTMAX                                                      
         BCTR  RF,0                                                             
         EXMVC RF,BOWORK1,0(R1)                                                 
         CURED (2,BOWORK1),(8,SVFLTER),2,ALIGN=LEFT,MINUS=YES,         +        
               DMCB=BOPARM                                                      
*                                                                               
DISPCTX  XIT1                                                                   
         SPACE 2                                                                
***********************************************************************         
* SAVE THE FILTER VALUE FOR THE LIST PROTECTED LINE                             
* NTRY:  R1 POINTS TO THE FILTER DATA FROM THE FILTER BLOCK                     
* EXIT:  TRUELEN CONTAINS THE REAL LENGTH OF THE FILTER ENTERED                 
*        SVFLTER CONTAINS THE FILTER                                            
***********************************************************************         
         SPACE 1                                                                
         USING FLTINFO,R1                                                       
SVFILT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    BYTE,RANGE2         IF THIS BIT IS ON R1 POINTS TO THE           
         BO    *+8                 DATA ALREADY.                                
         LA    R1,FLTDATA                                                       
         XC    SVFLTER,SVFLTER                                                  
*                                                                               
         USING DISPTBLD,R3         CHECK TO SEE HOW TO DISPLAY THE DATA         
         LA    R3,DISPTBL          IF FIELD NUMBER NOT IN DISPTBL TABLE         
SVFILT5  CLI   0(R3),X'FF'         THAN JUST DO A STRAIGHT CHARACTER            
         BE    SVFILT40            MOVE                                         
         CLC   SVFLTNM,DISPNUM                                                  
         BE    SVFILT10                                                         
         LA    R3,DISPLNQ(R3)                                                   
         B     SVFILT5                                                          
*                                                                               
SVFILT10 SR    RF,RF                                                            
         L     RF,DISPRTN                                                       
         A     RF,RTRELO                                                        
         BASR  RE,RF                                                            
         B     SVFILT45                                                         
*                                                                               
SVFILT40 ZIC   RF,SVFLTMAX                                                      
         BCTR  RF,0                                                             
         EXMVC RF,SVFLTER,0(R1)    SAVE OF THE MAX FILTER DATA                  
*                                                                               
SVFILT45 OC    SVFLTER,BCSPACES                                                 
         LA    R1,SVFLTER                                                       
         ZIC   RF,SVFLTMAX                                                      
         AR    R1,RF               BUMP R1 TO LAST POSITION IN FILTER           
         BCTR  R1,0                BUMP BACK ONE FOR TRUE LAST POSITION         
SVFLT50  CLI   0(R1),C' '                                                       
         BNE   SVFLT60                                                          
         BCTR  R1,0                BUMP BACK ONE BYTE                           
         BCT   RF,SVFLT50                                                       
*                                                                               
SVFLT60  STC   RF,TRUELEN          SAVE THE TRUE LENGTH OF FILTER               
*                                                                               
         XIT1                                                                   
         DROP  R1,R3                                                            
         SPACE 1                                                                
*                                                                               
*DISPLAY TABLE                                                                  
DISPTBL  DC    AL2(ACC#BBFWD),AL2(0),AL4(DISDAT)  ADDED OR PEELED DATE          
         DC    AL2(ACC#CHADT),AL2(0),AL4(DISDAT)  LAST CHANGED DATE             
         DC    AL2(ACC#ACTDT),AL2(0),AL4(DISDAT)  LAST TRANSACTION DATE         
         DC    AL2(ACC#COMM),AL2(0),AL4(DISCOMM)  COMMISSION RATE               
         DC    AL2(ACC#DISC),AL2(0),AL4(DISPCT)   CASH DISCOUNT PERCENT         
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD THE FILTER VALUE AND BUMP TO NEXT ENTRY IF MORE FILTERS FOR             
* THIS FIELD.                                                                   
* NTRY:  SVFLTER CONTAINS THE ISOLATED CURRENT FILTER                           
*        SVFLTER2 CONTAINS THE ACCUMULATED FILTER BEING BUILT                   
***********************************************************************         
         SPACE 1                                                                
         USING FLTINFO,R1                                                       
BLDFILT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,SVFLTER2         BUILD WHOLE FILTER HERE                      
         AH    RE,DISPL                                                         
         TM    BYTE,COMMA                                                       
         BZ    *+12                                                             
         MVI   0(RE),C','                                                       
         AHI   RE,1                                                             
         TM    FLTIND1,FDRF1NOT+FDRF1LT GREATER THAN/EQUAL TO                   
         BNO   *+14                                                             
         MVC   0(2,RE),=C'>='                                                   
         B     BLDFIL10                                                         
         TM    FLTIND1,FDRF1NOT+FDRF1GT LESS THAN/EQUAL TO                      
         BNO   *+14                                                             
         MVC   0(2,RE),=C'<='                                                   
         B     BLDFIL10                                                         
         TM    FLTIND1,FDRF1NOT    NOT FILTER                                   
         BZ    *+12                                                             
         MVI   0(RE),C'*'                                                       
         B     BLDFIL20                                                         
         TM    FLTIND1,FDRF1EQ     EQUAL FILTER                                 
         BZ    *+12                                                             
         MVI   0(RE),C'='                                                       
         B     BLDFIL20                                                         
         TM    FLTIND1,FDRF1GT     GREATER THAN FILTER                          
         BZ    *+12                                                             
         MVI   0(RE),C'>'                                                       
         B     BLDFIL20                                                         
         TM    FLTIND1,FDRF1LT     LESS THAN FILTER                             
         BZ    BLDFIL30                                                         
         MVI   0(RE),C'<'                                                       
         B     BLDFIL20                                                         
*                                                                               
BLDFIL10 AHI   RE,2                                                             
         B     BLDFIL30                                                         
*                                                                               
BLDFIL20 AHI   RE,1                                                             
*                                                                               
BLDFIL30 ZIC   RF,TRUELEN          RF CONTAINS TRUE LENGTH OF FILTER            
         BCTR  RF,0                                                             
         EXMVC RF,0(RE),SVFLTER                                                 
         AHI   RF,1                BUMP BACK TO REAL LENGTH                     
         OI    BYTE,COMMA                                                       
         LA    R2,SVFLTER2                                                      
         AR    RE,RF                                                            
         SR    RE,R2                                                            
         STH   RE,DISPL                                                         
         ZIC   RE,SVFLTCNT         # OF FILTERS IN THIS LIST SEQUENCE           
         BCTR  RE,0                                                             
         STC   RE,SVFLTCNT                                                      
         CLI   SVFLTCNT,0          ANY MORE FILTERS FOR THIS FIELD?             
         BNH   BLDFLTL             NO                                           
         L     R1,SVINFADR         POINT R1 TO CURRENT FLTR BLOCK ENTRY         
         ZIC   RF,SVFLTMAX                       BUMP PAST FILTER DATA          
         AHI   RF,L'FLTIND1+L'FLTIND2                                           
         AR    R1,RF                                                            
         ST    R1,SVINFADR         KEEP POINTER TO SUBSEQUENT ENTRIES           
         B     BLDFLTE                                                          
*                                                                               
BLDFLTL  CLI   *,FF                                                             
         B     BLDFLTX                                                          
BLDFLTE  CR    RB,RB                                                            
BLDFLTX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
***********************************************************************         
* ADD FIELD ENTRY INFORMATION TO THE FIELD TABLE                                
* NTRY - R2 POINTS TO THE LFIXCLM/LVARCLM ENTRY                                 
***********************************************************************         
         SPACE 1                                                                
         USING FLDTABD,RF                                                       
         USING DCTABD,R2                                                        
         USING FDRELD,R3                                                        
ADDFLD   NTR1  BASE=*,LABEL-*                                                   
         LA    RF,FLDTAB                                                        
         AH    RF,TABDISP          BUMP TO NEXT AVAILABLE ENTRY                 
         LA    RE,FLDEND                                                        
         CR    RF,RE                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FLDNUM,DCTFLD#      FIELD NUMBER                                 
         MVC   FLDCWID,FDRLHLEN    COLUMN WIDTH                                 
         MVC   FLDDWID,FDRLCLEN    DATA WIDTH                                   
         MVC   FLDDISP,FDRLCDSP    DISPL. TO FIELD WITHIN HEADING               
         LA    RE,FLDTAB                                                        
         LA    RF,FLDTBLNQ(RF)     FIGURE DISPLACEMENT INTO TABLE               
         SR    RF,RE                                                            
         STH   RF,TABDISP                                                       
         XIT1                                                                   
         DROP  R2,RF                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD DOWNLOAD HEADLINES                                                      
* READ FIELD RECORDS TO PRINT COLUMN HEADINGS                                   
***********************************************************************         
         SPACE 1                                                                
         USING DCTABD,R2                                                        
         USING DLCBD,R4                                                         
DCOLUMNS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,DLCB                                                          
         XR    R0,R0                                                            
         ICM   R0,3,LSFIXNUM       NUMBER OF FIXED COLUMNS                      
         BZ    DCOL10              NO FIXED COLS(IS THIS POSSIBLE?)             
         STCM  R0,3,SVNUMCOL       SAVE # OF FIXED COLUMNS                      
         OI    BYTE,FIXCLM         PROCESSING FIXED COLUMNS                     
         LA    R2,LSFIXCLM                                                      
         B     DCOL20                                                           
DCOL10   ICM   R0,3,LSVARNUM       NUMBER OF VARIABLE COLUMNS                   
         BNZ   *+6                                                              
         DC    H'0'                NEED SOMETHING                               
         STCM  R0,3,SVNUMCOL       SAVE # OF VARIABLE COLUMNS                   
         LA    R2,LSVARCLM                                                      
*                                                                               
COL      USING FDRRECD,IOKEY                                                    
DCOL20   XC    COL.FDRKEY,COL.FDRKEY                                            
         MVI   COL.FDRKMIN,FDRKMINQ  BUILD FIELD RECORD KEY FOR COLUMN          
         MVI   COL.FDRKTYP,FDRKTYPQ                                             
         MVC   COL.FDRKSYS,GCOVSYS                                              
         MVC   COL.FDRKPRG,GCPRGNO                                              
         MVC   COL.FDRKREC,CSREC                                                
         MVC   COL.FDRKNUM,DCTFLD#   FIELD # FROM LAVARCLM/LSFIXCLM             
         MVC   COL.FDRKCTRY,CUCTRY                                              
         XI    COL.FDRKCTRY,FF                                                  
         MVI   COL.FDRKSUB,FF                                                   
         MVC   COL.FDRKTEST,ASTEST                                              
         GOTOX ('GETFLD',AGROUTS),BOPARM,GFREAD,AIO2                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO2                                                          
         LA    R3,FDRFIRST(R3)                                                  
         USING FDRELD,R3                                                        
         XR    RF,RF                                                            
DCOL30   CLI   FDREL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FDREL,FDRELQ                                                     
         BE    DCOL40                                                           
         IC    RF,FDRLN                                                         
         LA    R3,0(RF,R3)                                                      
         B     DCOL30                                                           
*                                                                               
DCOL40   OC    FDRLHED1,FDRLHED1   FIRST HEADLINE?                              
         BZ    DCOL60                                                           
*                                                                               
*FDRLHED1/FDRLHED2 CONTAIN THE 4 BYTE DATA DICTIONARY ENTRY CONSISTING          
*OF THE FOLLOWING:                                                              
*1ST BYTE: ESCAPE SEQUENCE                                                      
*2ND & 3RD BYTES:  DDICT EQUATE NUMBER                                          
*4TH BYTE: LENGTH                                                               
*                                                                               
* PROCESS 1ST HEADLINE                                                          
* LET OVERLAY OVERRIDE THE HEADLINES                                            
* RTWORK WILL CONTAIN THE 1ST HEADLINE OVERRIDE                                 
* BCWORK WILL CONTAIN THE 2ND HEADLINE OVERRIDE (AN UNDERLINE)                  
* BCBYTE1 WILL CONTAIN THE LENGTH OF 1ST HEADLINE                               
         MVC   RTWORK,BCSPACES                                                  
         MVC   BCWORK,BCSPACES                                                  
         MVC   FVIFLD,BCSPACES                                                  
         GOTOX AOLY,RTPARM,ODLOAD,DSETCOLS,FVIFLD,FDRELD,RTWORK,BCWORK          
         CLC   RTWORK,BCSPACES     IS THERE AN COLUMN HEADING OVERRIDE          
         BE    DCOL50              FROM THE OVERLAY?                            
         ZIC   R1,BCBYTE1          YES - SAVE THE LENGTH OF THE                 
         STC   R1,SVHLEN           OVERRIDE AND MOVE THE OVERRIDE INTO          
         BCTR  R1,0                FVIFLD FOR PROCESSING                        
         EXMVC R1,FVIFLD,RTWORK                                                 
         B     DCOL60                                                           
*                                                                               
DCOL50   MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'FDRLHED1),FDRLHED1                                      
         MVI   FVIFLD,DD#ESCL      SET HEADLINE 1 LEFT ALIGNED                  
         MVC   SVHLEN,FVIFLD+3     SAVE THE LENGTH OF THE HEADLINE              
*                                                                               
         ICM   RF,15,=C'SL  '      RESOLVE FIRST HEADLINE                       
         ICM   RF,2,COL.FDRKSYS                                                 
         GOTOX VDICTAT,BOPARM,(RF),FVIFLD,0,0                                   
*                                                                               
* PROCESS 2ND HEADLINE                                                          
* IN THE DOWNLOAD THE HEADLINES ARE SEPARATED BY A SLASH E.G.,                  
* "ACCOUNT/CODE" SO IF THE 2ND HEADLINE IS AN UNDERLINE DON'T                   
* DISPLAY IT.                                                                   
DCOL60   CLC   BCWORK,BCSPACES     DID WE OVERRIDE THE 1ST COLUMN               
         BE    DCOL70              HEADING? NO                                  
         CLI   BCWORK,C'-'         IS THE OVERRIDE AN UNDERLINE?                
         BE    DCOL80              YES                                          
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '          PUT HEADING 2 AFTER HEADING 1                
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'          PUT SLASH BETWEEN THEM                       
         LA    RF,2(RF)                                                         
         ZIC   R1,SVHLEN                                                        
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),BCWORK                                                  
         B     DCOL80                                                           
*                                                                               
DCOL70   CLI   FDRLHED2,DD#ESUL2   SECOND HEADLINE IS UNDERLINE?                
         BNL   DCOL80              YES                                          
         OC    FDRLHED2,FDRLHED2   SECOND HEADLINE?                             
         BZ    DCOL80              NO                                           
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '          PUT HEADING 2 AFTER HEADING 1                
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'          PUT SLASH BETWEEN THEM                       
         LA    RF,2(RF)                                                         
         MVC   0(L'FDRLHED2,RF),FDRLHED2                                        
         MVI   0(RF),DD#ESCL       SET LEFT ALIGN                               
         LA    R5,FVIFLD+L'FVIFLD-1                                             
         SR    R5,RF                                                            
         CLM   R5,1,3(RF)          WHAT'S WANTED LESS THAN WHAT'S LEFT?         
         BNL   *+8                 YES                                          
         STC   R5,3(RF)                                                         
         ICM   R5,15,=C'SL  '      RESOLVE SECOND HEADLINE                      
         ICM   R5,2,COL.FDRKSYS                                                 
         GOTOX VDICTAT,BOPARM,(R5),(RF)                                         
*                                                                               
DCOL80   LA    RF,FVIFLD+L'FVIFLD-1                                             
         LA    R1,L'FVIFLD         REPLACE ANY `"` WITH `'` IN                  
         CLI   0(RF),C'"'          ELSE DOWNLOAD NOT HAPPY...                   
         BNE   *+8                                                              
         MVI   0(RF),C''''                                                      
         BCTR  RF,0                                                             
         BCT   R1,*-14                                                          
*                                                                               
         MVI   DLCBFLX,C' '        FILL FIELD WITH SPACES                       
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
         MVC   DLCBFLX(L'FVIFLD),FVIFLD                                         
         MVI   DLCBACT,DLCBPUT     PUTTING TEXT (FOR NOW)                       
         MVI   DLCBTYP,DLCBTXT                                                  
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN                                       
         GOTOX VDLFLD,DLCBD                                                     
*                                                                               
         LA    R2,DCTABL(R2)       NEXT COLUMN FOR THIS LINE                    
         SR    R1,R1                                                            
         ICM   R1,3,SVNUMCOL                                                    
         BCTR  R1,0                                                             
         STCM  R1,3,SVNUMCOL                                                    
         LTR   R1,R1               # OF COLS LEFT TO PROCESS                    
         BNZ   DCOL20                                                           
         TM    BYTE,FIXCLM         DID WE JUST FINISH THE FIXED COLS?           
         BZ    DCOLX               NO SO DONE                                   
         NI    BYTE,X'FF'-FIXCLM                                                
         SR    R0,R0                                                            
         ICM   R0,3,LSVARNUM       NO VARIABLE COLUMNS (IS THIS                 
         BZ    DCOLX               POSSIBLE?)                                   
         STCM  R0,3,SVNUMCOL         SAVE # OF VARIABLE COLUMNS                 
         LA    R2,LSVARCLM                                                      
         B     DCOL20                                                           
*                                                                               
DCOLX    MVI   DLCBACT,DLCBEOL     END OF HEADING TEXT LINE                     
         GOTOX VDLFLD,DLCBD                                                     
         XIT1                                                                   
         DROP  R2,R4,COL                                                        
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FIND FIELD ENTRY IN TABLE AND MOVE DATA INTO PRINTLINE BASE ON      *         
* COLUMN/WIDTH TABLE INFO                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING DCTABD,R2                                                        
         USING REPD,R5                                                          
         USING FLDTABD,RF                                                       
RPLIN    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,FLDTAB                                                        
RPLIN10  LA    RE,FLDEND                                                        
         CR    RF,RE                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         CLC   FLDNUM,DCTFLD#      MATCH ON FIELD NUMBER?                       
         BE    RPLIN20                                                          
         LA    RF,FLDTBLNQ(RF)                                                  
         B     RPLIN10                                                          
*                                                                               
RPLIN20  LA    RE,REPP1                                                         
         CLI   REPWIDTH,REPWIDEQ   PRINTING WIDE?                               
         BNE   *+8                                                              
         LA    RE,REPPW1                                                        
         AH    RE,LINDISP          ADD DISPLACEMENT INTO PLINE                  
         ZIC   R1,FLDDISP          ADD ANY DISPLACEMENT INTO COLUMN             
         AR    RE,R1               IN THE PLINE                                 
         ZIC   R1,FLDDWID          R1=DATA FIELD WIDTH                          
         EXMVC R1,0(RE),FVIFLD                                                  
*                                                                               
         ZIC   R1,FLDCWID          COLUMN WIDTH                                 
         AR    RE,R1                                                            
         AHI   RE,1                BUMP TO NEXT AVAILABLE ENTRY                 
         LA    RF,REPP1            NOW FIGURE DISPLACMENT INTO PLINE            
         CLI   REPWIDTH,REPWIDEQ   FOR NEXT ENTRY                               
         BNE   *+8                                                              
         LA    RF,REPPW1                                                        
         SR    RE,RF                                                            
         STH   RE,LINDISP          SAVE DISPLACEMENT TO NEXT SPOT               
*                                                                               
         XIT1                                                                   
         DROP  R2,R5,RF                                                         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FORMAT DATA INTO DOWNLOAD FORMAT                                    *         
* ENTRY:  FVIFLD CONTAINS THE DATA                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING DLCBD,R4                                                         
DWLLIN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,DLCB                                                          
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         LA    R1,L'FVIFLD         REPLACE ANY `"` WITH `'` IN DATA             
         CLI   0(RF),C'"'          ELSE DOWNLOAD NOT HAPPY...                   
         BNE   *+8                                                              
         MVI   0(RF),C''''         ALL THAT FOR ONE LOUSY DINK...               
         BCTR  RF,0                                                             
         BCT   R1,*-14                                                          
*---------------------------------------------------------JRD                   
         TM    SVINDS1,X'08'       NUMERIC DOWNLOAD?                            
         BZ    DWLIN10             NO                                           
*                                                                               
         MVI   DLCBFLX,C' '        FILL FIELD WITH SPACES                       
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
         LA    RE,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         CH    RE,=Y(L'DLCBFLD-1)                                               
         BNH   *+8                                                              
         LA    RE,L'DLCBFLD-1                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),FVIFLD                                                
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUTTING TEXT (FOR NOW)                       
         MVI   DLCBTYP,DLCBNUM                                                  
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN                                       
         B     DWLIN20                                                          
*                                                                               
DWLIN10  DS    0H                                                               
         MVI   DLCBFLX,C' '        FILL FIELD WITH SPACES                       
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
         MVC   DLCBFLX(L'FVIFLD),FVIFLD                                         
         MVI   DLCBACT,DLCBPUT     PUTTING TEXT (FOR NOW)                       
         MVI   DLCBTYP,DLCBTXT                                                  
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN                                       
*                                                                               
DWLIN20  GOTOX VDLFLD,DLCBD                                                     
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* USER SUPPLIED PRINT LINE ROUTINE                                    *         
***********************************************************************         
         SPACE 1                                                                
DLLINE   NTR1                                                                   
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         MVC   REPPAGE,=AL2(2)     DOWNLOADED DATA ALWAYS ON P2                 
         MVI   REPLINE,1           ??                                           
*                                                                               
         MVI   REPACTN,REPAPUT     OPEN THE REPORT                              
         GOTOX VREPORT,REPBLK                                                   
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
*  RESET HEADLINES/FOOTLINES FOR NEW PAGE                            *          
**********************************************************************          
         SPACE 1                                                                
         USING REPD,R4                                                          
         USING CPYRECD,R2                                                       
REPHOOK  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AREP                                                          
*        OI    REPHEADI,REPHSPAC   SKIP A LINE AFTER HEADLINES                  
         MVC   CPYNAME,BCSPACES                                                 
         LA    R2,IOKEY            READ COMPANY RECORD & EXTRACT VALUES         
         MVC   CPYKEY,BCSPACES                                                  
         MVC   CPYKCPY,CUABIN                                                   
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTOR AIO                                                              
         BNE   REPHK20                                                          
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO1             LOCATE NAME ELEMENT                          
         LA    R1,CPYRFST                                                       
         USING NAMELD,R1                                                        
         SR    R0,R0                                                            
REPHK10  CLI   NAMEL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NAMEL,NAMELQ        COMPANY NAME ELEMENT                         
         BE    REPHK15                                                          
         ZIC   R0,NAMLN                                                         
         AR    R1,R0                                                            
         B     REPHK10                                                          
*                                                                               
REPHK15  ZIC   RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1        RE=LENGTH OF NAME - 1                        
         EXMVC RE,CPYNAME,NAMEREC                                               
         DROP  R1,R2                                                            
*                                                                               
REPHK20  CLI   CSREC,X'19'         ACCOUNT RECORD?                              
         BNE   REPHOOKX                                                         
         LA    R2,REPH2                                                         
         LA    R3,REPH3                                                         
         CLI   REPWIDTH,REPWIDEQ   PRINTING WIDE?                               
         BNE   *+12                                                             
         LA    R2,REPHW2                                                        
         LA    R3,REPHW3                                                        
         MVC   50(L'AC@ACCL,R2),AC@ACCL   SET TITLE,UNDERLINE                   
         MVC   50(L'AC@ACCLU,R3),AC@ACCLU                                       
         LA    R2,REPH4                                                         
         LA    R3,REPH5                                                         
         CLI   REPWIDTH,REPWIDEQ   PRINTING WIDE?                               
         BNE   *+12                                                             
         LA    R2,REPHW4                                                        
         LA    R3,REPHW5                                                        
         MVC   1(L'AC@CUID,R2),AC@CUID             'COMPANY USER ID'            
         LA    R1,BCCPYEL                                                       
         USING CPYELD,R1                                                        
         MVC   18(L'CPYLOGO,R2),CPYLOGO                                         
         DROP  R1                                                               
*                                                                               
         MVC   26(L'CPYNAME,R2),CPYNAME     COMPANY NAME                        
         MVC   0(L'AC@NF19,R3),AC@NF19      'UNIT AND LEDGER'                   
         L     R1,AIOREC                                                        
         MVC   L'AC@NF19+1(L'ACTKUNT+L'ACTKLDG,R3),1(R1)                        
         OC    SVFLTACC,BCSPACES   ANY ACCT CODE FILTER?                        
         BE    *+16                NO                                           
         MVC   25(L'AC@ACC,R3),AC@ACC       'ACCOUNT CODE'                      
         MVC   L'AC@ACC+26(L'SVFLTACC,R3),SVFLTACC                              
         LA    R2,REPH6                                                         
         CLI   REPWIDTH,REPWIDEQ   PRINTING WIDE?                               
         BNE   *+8                                                              
         LA    R2,REPHW6                    PRINT FILTERS                       
         MVC   1(L'SVFLTVW,R2),SVFLTVW                                          
*                                                                               
REPHOOKX XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
FILTLN   EQU   17                  USED TO DISPLAY FILTER FIELD                 
*                                                                               
* NEED SPECS HERE INSTEAD OF THE OVERLAYS B/C THE HEADLINE HOOK IS              
* REFERENCED IN THE REPHOOK ROUTINE WHICH ALSO CANNOT BE IN THE                 
* OVERLAYS DUE TO THE HEADLINES BEING BUILT AT THE 04 LEVEL                     
*                                                                               
SPECS    DS    0X                                                               
         SPEC  H1,2,RUNONAT                                                     
         SPEC  H1,140,PAGE                                                      
         SPEC  H2,140,AC#RAAL,10                                                
         SPEC  END                                                              
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
RTRELO   DS    A                                                                
RTPARMA  DS    A                   A(INCOMING PARAMETER LIST)                   
RTPARMO  DS    F                                                                
RTPARMS  DS    0XL24               SAVED PARAMETERS                             
RTPARMS1 DS    A                                                                
RTPARMS2 DS    A                                                                
RTPARMS3 DS    A                                                                
RTPARMS4 DS    A                                                                
RTPARMS5 DS    A                                                                
RTPARMS6 DS    A                                                                
RTPARM   DS    XL24                * PARAMETERS 1-6 *                           
VDLFLD   DS    A                   V(DLFLD) FOR DOWNLOADING                     
RTDATA   DS    XL300                                                            
RTWORK   DS    XL80                                                             
RTBYTE1  DS    X                                                                
SVADDR   DS    A                                                                
SVINFADR DS    A                                                                
TABEND   DS    A                                                                
DISPL    DS    H                                                                
SVFLD#   DS    XL2                 SAVED FIELD NUMBER                           
LINDISP  DS    XL2                 DISPLACEMENT INTO PRINT LINE                 
TABDISP  DS    XL2                 DISPLACEMENT INTO FIELD TABLE                
HD1DISP  DS    XL2                 DISPLACEMENT INTO 1ST HEADLINE               
HD2DISP  DS    XL2                 DISPLACEMENT INTO 2ND HEADLINE               
SVHLEN   DS    XL1                 LENGTH OF HEADLINE FROM FIELD REC            
SVNUMCOL DS    XL2                 SAVED # OF COLUMNS (FIXED OR VAR)            
BYTE     DS    CL1                                                              
FIXCLM   EQU   X'80'               PROCESSING FIXED COLUMNS                     
COMMA    EQU   X'40'                                                            
RANGE1   EQU   X'20'                                                            
RANGE2   EQU   X'10'                                                            
FLTHD    EQU   X'08'               PROCESSING THE FLTRS FOR THE HDLINES         
*                                                                               
FILLER   DS    CL3                                                              
SVFLTER  DS    XL(FILTLN)                                                       
SVFLTER2 DS    XL(FILTLN)                                                       
SVFLTACC DS    XL(FILTLN)          RHS OF ACCT CODE FLTR FOR REP HDLINE         
SVFLTCNT DS    XL1                                                              
SVFLTMAX DS    XL1                                                              
SVFIND2  DS    XL1                 SAVED FLTIND2 FIELD                          
SVFLDNM  DS    XL2                 FLD # OF FILTER FOR REPORT HDLINES           
SVFLTNM  DS    XL2                 SAVED FIELD NUMBER OF THIS FILTER            
TRUELEN  DS    XL1                                                              
SVINDS1  DS    XL1                 SAVED INDICATOR BYTE FROM DCTAB              
CPYNAME  DS    CL(L'NAMEREC)       COMPANY NAME                                 
DLCB     DS    XL(DLCBXLX)                                                      
KEY      DS    XL(ACCKLEN)                                                      
*                                                                               
* DO NOT SEPERATE                                                               
SVFLTVWH DS    XL8                 NEED A FAKE HEADER FOR UNSCAN                
SVFLTVW  DS    CL80                AREA FOR UNSCAN CALL FOR FILTER VIEW         
* DO NOT SEPERATE                                                               
*                                                                               
FLDTAB   DS    (VARMAX)CL(FLDTBLNQ) TABLE OF FIELD EQUS+MORE                    
FLDEND   DS    C                                                                
* VARMAX=MAX # OF FIXED COLUMNS+MAX # OF VARIABLE COLUMNS                       
*                                                                               
RTLISTU  DS    0D                  DICTIONARY EQUATES USED                      
AC@DEL   DS    CL6                 DELETE                                       
AC@ACCL  DS    CL15                ACCOUNT LISTING                              
AC@ACCLU DS    CL15                ACCOUNT LISTING UNDERLINE                    
AC@FLT   DS    CL6                 FILTER                                       
AC@CUID  DS    CL15                COMPANY USER ID                              
AC@NF19  DS    CL15                UNIT AND LEDGER                              
AC@NFA94 DS    CL9                 OPT/MAINT                                    
AC@NONE  DS    CL4                 NONE                                         
AC@ACC   DS    CL12                ACCOUNT CODE                                 
AC@NFA42 DS    CL6                 (None)                                       
RU@SOON  DS    CL4                                                              
RU@OVNT  DS    CL2                 OVER NIGHT                                   
RU@NOW   DS    CL3                 NOW                                          
*                                                                               
RTWORKL  EQU   *-RTWORKD                                                        
*                                                                               
*                                                                               
ACFIL04  CSECT                                                                  
         EJECT                                                                  
*        CONBLKD                                                                
CONBLKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
*                                                                               
LNLEVELS DS    0XL4       LENGTHS OF DIFF LEVEL IN LEDGER RECORD                
LNLEV1   DS    XL1                                                              
LNLEV2   DS    XL1                                                              
LNLEV3   DS    XL1                                                              
LNLEV4   DS    XL1                                                              
TLEVS    DS    0XL4                TEMP LENGTHS OF EACH LEVEL                   
TLEV1    DS    XL1                                                              
TLEV2    DS    XL1                                                              
TLEV3    DS    XL1                                                              
TLEV4    DS    XL1                                                              
*                                                                               
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
         EJECT                                                                  
ACFIL04  CSECT                                                                  
         ORG   ACFIL04+(((*-ACFIL04)/2048)+1)*2048                              
         SPACE 2                                                                
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
FLDTABD  DSECT                                                                  
FLDNUM   DS    XL2                 FIELD NUMBER                                 
FLDCWID  DS    XL1                 COLUMN FIELD WIDTH                           
FLDDWID  DS    XL1                 DATA FIELD WIDTH                             
FLDDISP  DS    XL1                 DISP TO FIELD WITHIN TAG                     
FLDTBLNQ EQU   *-FLDTABD                                                        
         SPACE 2                                                                
FLTCDD   DSECT                                                                  
FLTCID   DS    XL2                 FIELD EQUATE ID                              
FLTBYTE  DS    XL1                                                              
FLTNOLST EQU   X'80'               DON'T SHOW THIS FILTER IF LISTING            
FLTHDL   EQU   X'40'               THIS FILTER IS FOR REP HDLINES ONLY          
FLTNAME  DS    CL8                 FILTER NAME                                  
FLTCLNQ  EQU   *-FLTCID                                                         
         SPACE 2                                                                
DISPTBLD DSECT                                                                  
DISPNUM  DS    XL2                 FIELD EQUATE ID                              
         DS    XL2                 SPARE                                        
DISPRTN  DS    A                  A(DSPLY RTN FOR PROTECTED FILTER FLD)         
DISPLNQ  EQU   *-DISPNUM                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACFIL04   08/28/20'                                      
         END                                                                    
