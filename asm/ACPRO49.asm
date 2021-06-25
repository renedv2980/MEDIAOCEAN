*          DATA SET ACPRO49    AT LEVEL 011 AS OF 09/12/02                      
*PHASE T60B49A,*                                                                
*INCLUDE ACDISCOL                                                               
*INCLUDE ACGETSCH                                                               
*INCLUDE ACGETTXT                                                               
*INCLUDE DLFLD                                                                  
*INCLUDE RIGHT                                                                  
         TITLE 'T60B49 - PRODUCTION ESTIMATE REPORT MODULE'                     
T60B49   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B49**,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         ST    R2,RELO                                                          
         SPACE 1                                                                
EST1     CLI   MODE,RUNFRST        TEST FOR RUN FIRST HOOK                      
         BNE   EST2                                                             
         BAS   RE,INIT             INITIALIZE BUFFERS                           
         B     ESTX                                                             
*                                                                               
EST2     CLI   MODE,PRINTREP                                                    
         BNE   EST4                                                             
*                                                                               
         L     RE,RELO                                                          
         LA    R0,NRELOS           R0=LOOP COUNTER                              
         LA    R1,RELOTAB          R1=A(ADCONS TO RELOCATE)                     
         LA    R2,VGETTXT          R2=A(OUTPUT)                                 
*                                                                               
EST3     L     RF,0(R1)            GET ADCON                                    
         AR    RF,RE               RELOCATE IT                                  
         ST    RF,0(R2)                                                         
         LA    R1,L'RELOTAB(R1)                                                 
         LA    R2,4(R2)                                                         
         BCT   R0,EST3                                                          
*                                                                               
         BAS   RE,DEF              GET DEFAULT NAMES                            
         BAS   RE,RDF              READ THE FILE/PRINT THE ESTIMATES            
         B     ESTX                                                             
         SPACE 1                                                                
EST4     CLI   MODE,RUNLAST        TEST LAST TIME HOOK                          
         BNE   ESTX                                                             
*                                                                               
         L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
         L     R0,TSPFUSER         RESTORE SAVED ADDRESSES                      
         LA    R1,SVDATAL                                                       
         LA    RE,SVDATA                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R1,ABUFF                                                         
         L     R0,BUFFLEN                                                       
         FREEMAIN R,A=(1),LV=(0)                                                
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         XC    MCUSRDMP(8),MCUSRDMP                                             
         B     ESTX                                                             
         DROP  R1,RE                                                            
*                                                                               
ESTX     XMOD1 1                                                                
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
RELOTAB  DS    0F                                                               
         DC    V(ACGETTXT)                                                      
         DC    V(ACGETSCH)                                                      
         DC    V(DLFLD)                                                         
         DC    V(ACDISCOL)                                                      
         DC    V(RIGHT)                                                         
         DC    A(DISVAL)                                                        
         DC    A(WN)                                                            
NRELOS   EQU   (*-RELOTAB)/L'RELOTAB                                            
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO INITIALIZE FOR ESTIMATE PRINTING                *              
*                                                                *              
* ROUTINE ACQUIRES A DYNAMIC BUFFER VIA GETMAIN.  THE BUFFER     *              
* IS THEN ALLOCATED TO SPECIFIC DATA BUFFERS MAINTAINED          *              
* ACROSS REQUESTS.  A SET OF ADCONS POINTING TO THE BUFFERS      *              
* IS PART OF STORAGE SAVED IN BETWEEN REQUESTS IN TSPFUSER.      *              
******************************************************************              
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     R0,BUFFLEN                                                       
         GETMAIN R,LV=(0)                                                       
         ST    R1,ABUFF            SAVE A(ACQUIRED BUFFER)                      
         LR    R2,R1               R2=A(BUFFER)                                 
         LR    RE,R2                                                            
         L     RF,BUFFLEN                                                       
         SR    R1,R1                                                            
         LR    R0,RE                                                            
         MVCL  RE,R0               CLEAR ACQUIRED BUFFER                        
*                                                                               
INIT2    L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         STCM  R2,15,MCUSRDMP      SET POINTERS FOR EXTENDED DUMP               
         STCM  RE,15,MCUSRDMP+4    RE=END OF BUFFER AFTER MVCL                  
         DROP  R1                                                               
*                                                                               
INIT4    MVC   0(8,R2),=C'**OFFB**'                                             
         LA    R2,8(R2)                                                         
         ST    R2,AOFFBUFF                                                      
         LA    R2,OFFBUFFL(R2)     OFFICE/OFFICE GROUP BUFFER                   
*                                                                               
         MVC   0(8,R2),=C'**MEDB**'                                             
         LA    R2,8(R2)                                                         
         ST    R2,AMEDBUFF         MEDIA RECORD BUFFER                          
         A     R2,MEDBUFFL                                                      
*                                                                               
         MVC   0(8,R2),=C'**WRKB**'                                             
         LA    R2,8(R2)                                                         
         ST    R2,AWRKBUFF         WORKCODE BUFFER                              
         A     R2,WRKBUFFL                                                      
*                                                                               
INIT6    MVC   0(8,R2),=C'**COMP**' COMPANY RECORD AREA                         
         LA    R2,8(R2)                                                         
         ST    R2,ACOMP                                                         
         LA    R2,1000(R2)                                                      
*                                                                               
         MVC   0(8,R2),=C'**LEDG**'                                             
         LA    R2,8(R2)            LEDGER RECORD AREA                           
         ST    R2,ALEDG                                                         
         LA    R2,1000(R2)                                                      
*                                                                               
         MVC   0(8,R2),=C'**CLI***'                                             
         LA    R2,8(R2)            CLIENT RECORD AREA                           
         ST    R2,ACLI                                                          
         LA    R2,2000(R2)                                                      
*                                                                               
         MVC   0(8,R2),=C'**PROD**'                                             
         LA    R2,8(R2)            PRODUCT RECORD AREA                          
         ST    R2,APROD                                                         
         LA    R2,2000(R2)                                                      
*                                                                               
         MVC   0(8,R2),=C'**JOB***'                                             
         LA    R2,8(R2)            JOB RECORD AREA                              
         ST    R2,AJOB                                                          
         LA    R2,2000(R2)                                                      
*                                                                               
         MVC   0(8,R2),=C'***OE***'                                             
         LA    R2,8(R2)            ORIGINAL ESTIMATE AREA                       
         ST    R2,AOE                                                           
         LA    R2,2000(R2)                                                      
*                                                                               
         MVC   0(8,R2),=C'***CE***'                                             
         LA    R2,8(R2)            CURRENT ESTIMATE AREA                        
         ST    R2,ACE                                                           
         LA    R2,2000(R2)                                                      
*                                                                               
INIT8    MVC   0(8,R2),=C'**COLT**' JOBBER TABLES                               
         LA    R2,8(R2)                                                         
         ST    R2,ACOLTAB                                                       
         MVC   LCOLTAB,COLTABL                                                  
         A     R2,COLTABL                                                       
*                                                                               
         MVC   0(8,R2),=C'**OPVT**'                                             
         LA    R2,8(R2)                                                         
         ST    R2,AOPVTAB                                                       
         MVC   LOPVTAB,OPVTABL                                                  
         A     R2,OPVTABL                                                       
*                                                                               
         MVC   0(8,R2),=C'*PRINT**'                                             
         LA    R2,8(R2)                                                         
         ST    R2,APRTTAB                                                       
         A     R2,PRTTABL                                                       
*                                                                               
INIT10   MVC   0(8,R2),=C'**SCH***'                                             
         LA    R2,8(R2)                                                         
         ST    R2,ASCHTAB          SCHEME/CATEGORY TABLE                        
         A     R2,SCHTABL                                                       
*                                                                               
         MVC   0(8,R2),=C'*PANEL**'                                             
         LA    R2,8(R2)            PANEL BUFFER                                 
         ST    R2,APANEL                                                        
         A     R2,PANELTL                                                       
*                                                                               
INIT12   MVC   0(8,R2),=C'**OPTB**'                                             
         LA    R2,8(R2)                                                         
         ST    R2,AOPTBUFF                                                      
         MVC   LOPTBUFF,OPTBUFFL                                                
*                                                                               
INIT15   GOTO1 VBLDOFF,PARAS,AOFFBUFF                                           
         GOTO1 VBLDMED,PARAS,AMEDBUFF                                           
         GOTO1 VBLDWC,PARAS,AWRKBUFF                                            
         MVI   TWAFIRST,2          WANT A RUNLAST HOOK                          
         L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
         L     RE,TSPFUSER         GET A(USER BUFFER)                           
         LA    RF,SVDATAL                                                       
         LA    R0,SVDATA                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0               SAVE DATA                                    
*                                                                               
INITX    B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO GET THE DEFAULT NAMES--CALLED FROM MAIN LINE-- *               
*****************************************************************               
         SPACE 1                                                                
DEF      NTR1  ,                                                                
         LA    R3,NAMELIST                                                      
         LA    R5,COLLIST                                                       
         USING JBCLD,R5                                                         
         ZIC   R4,NCOLS                                                         
*                                                                               
DEF2     CLC   0(L'NAMELIST,R3),SPACES  TEST FOR NO NAME                        
         BNE   DEF4                                                             
         CLI   JBCLTYP,JBCLCOL     IS THIS A SINGLE COLUMN ?                    
         BE    DEF3                YES                                          
         CLI   JBCLTYP,JBCLSUPP    NO, IS IT A SUPPLEMENT ?                     
         BNE   DEF4                NO                                           
*                                                                               
DEF3     GOTO1 VDISCOL,DMCB,JBCLD,0(R3)                                         
*                                                                               
DEF4     LA    R3,L'NAMELIST(R3)                                                
         LA    R5,JBCLENQ(R5)                                                   
         BCT   R4,DEF2                                                          
*                                                                               
DEFX     B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
************************************************************                    
* SUB-ROUTINE TO READ THE FILE--CALLED FROM EST2           *                    
************************************************************                    
         SPACE 1                                                                
RDF      NTR1  ,                                                                
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),CUL     READ COMPANY RECORD                          
         GOTO1 READ                                                             
*                                                                               
         GOTO1 SAVREC,ACOMP                                                     
         MVC   GOADM,DATAMGR       INITIALIZE GENERAL GOBLOCK VALUES            
         MVC   GOSELCUL,CUL                                                     
         MVC   GOABUFF,AOPTBUFF    OPTIONS BUFFER                               
         MVC   GOLBUFF,LOPTBUFF                                                 
         MVC   GOACOMP,ACOMP                                                    
         MVC   GOALEDG,ALEDG                                                    
         MVC   GOACLI,ACLI                                                      
         MVC   GOAPRO,APROD                                                     
         MVC   GOAJOB,AJOB                                                      
         MVC   GOACOVL,COVAIL                                                   
         MVC   GOABINSR,BINSRCH                                                 
*                                                                               
RDF2     MVC   ACKEYACC(3),CUL     GET THE LEDGER RECORD                        
         GOTO1 READ                                                             
         GOTO1 SAVREC,ALEDG                                                     
         LA    R3,2                SET EXECUTE LENGTH                           
*                                                                               
RDF4     OC    QCLI,QCLI           TEST FOR CLIENT REQUEST                      
         BZ    RDF12               NO-GO RIGHT INTO FILE                        
*                                                                               
         MVC   ACKEYACC+3(L'QCLI),QCLI                                          
         GOTO1 READ                                                             
         BAS   RE,CLIENT                                                        
         ZIC   R1,LCLI                                                          
         AR    R3,R1                                                            
*                                                                               
RDF6     OC    QPROD,QPROD         TEST PRODUCT REQUESTED                       
         BZ    RDF12               NO-READ NEXT RECORD                          
*                                                                               
         ZIC   R1,LCLI                                                          
         LA    R1,ACKEYACC+3(R1)                                                
         MVC   0(L'QPROD,R1),QPROD                                              
         GOTO1 READ                                                             
         ZIC   R1,LPRO                                                          
         AR    R3,R1                                                            
         BAS   RE,PRODUCT                                                       
*                                                                               
RDF8     OC    QJOB,QJOB           TEST FOR SINGLE JOB                          
         BZ    RDF12               NO-GET NEXT RECORD                           
*                                                                               
         ZIC   R1,LCLIPRO                                                       
         LA    R1,ACKEYACC+3(R1)                                                
         MVC   0(L'QJOB,R1),QJOB   SET JOB IN KEY                               
         ZIC   R1,LJOB                                                          
         AR    R3,R1                                                            
*                                                                               
RDF10    OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         B     RDF20                                                            
*                                                                               
RDF12    OI    DMINBTS,X'08'                                                    
         GOTO1 SEQ                                                              
*                                                                               
RDF20    EX    R3,COMPKEY                                                       
         BNE   RDFX                END OF FILE                                  
*                                                                               
RDF22    TM    ACSTATUS,X'80'                                                   
         BO    RDF30                                                            
         ZIC   R5,LCLI                                                          
         LA    R5,ACKEYACC+3(R5)                                                
         CLI   0(R5),C' '          TEST FOR CLIENT RECORD                       
         BH    RDF24               NO                                           
         BAS   RE,CLIENT                                                        
         BE    RDF12               GET NEXT RECORD                              
         MVI   0(R5),X'FF'         FORCE NEXT CLIENT                            
         B     RDF10                                                            
*                                                                               
RDF24    ZIC   R5,LCLIPRO                                                       
         LA    R5,ACKEYACC+3(R5)                                                
         CLI   0(R5),C' '                                                       
         BH    RDF26               ITS A JOB                                    
         BAS   RE,PRODUCT                                                       
         BE    RDF12                                                            
         MVI   0(R5),X'FF'         FORCE NEXT PRODUCT                           
         B     RDF10                                                            
*                                                                               
RDF26    BAS   RE,JOB                                                           
*                                                                               
RDF30    MVI   ACKEYWRK,X'FF'      FORCE NEXT ACCOUNT                           
         B     RDF10                                                            
*                                                                               
RDFX     B     XIT                                                              
         SPACE 2                                                                
COMPKEY  CLC   ACKEYD(0),KEYSAVE                                                
         EJECT                                                                  
****************************************************************                
* HOOK ROUTINES TO PROCESS CLIENT, PRODUCT, AND JOB RECORDS    *                
****************************************************************                
         SPACE 1                                                                
CLIENT   NTR1  ,                                                                
         OC    QBGR,QBGR           TEST FOR BILLING GROUP FILTER                
         BZ    CLIENT2             NO                                           
*                                                                               
         MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   NOTOKXIT                                                         
         USING ACPROFD,R6                                                       
         TM    QBGR,X'40'          TEST FOR POSITIVE FILTER                     
         BO    CLIENT1                                                          
*                                                                               
         MVC   DUB(L'QBGR),QBGR                                                 
         OI    DUB,X'40'                                                        
         CLC   ACPRGRUP,DUB                                                     
         BE    NOTOKXIT                                                         
         B     CLIENT2                                                          
*                                                                               
CLIENT1  CLC   QBGR,ACPRGRUP       APPLY BILLING GROUP FILTER                   
         BNE   NOTOKXIT                                                         
         DROP  R6                                                               
*                                                                               
CLIENT2  GOTO1 SAVREC,ACLI                                                      
         GOTO1 SETCLI                                                           
         GOTO1 =A(GETPRO),DMCB,(RC),RR=RELO                                     
         B     OKXIT                                                            
         SPACE 1                                                                
PRODUCT  NTR1  ,                                                                
         GOTO1 SAVREC,APROD                                                     
         GOTO1 SETPROD                                                          
         BAS   RE,FILOFF                                                        
         B     XIT                                                              
         SPACE 1                                                                
JOB      NTR1  ,                                                                
         GOTO1 SAVREC,AJOB                                                      
         GOTO1 SETJOB                                                           
         TM    JOBJSTAT,ACJBNEWQ   TEST JOB USES NEW ESTIMATES                  
         BZ    JOBX                NO                                           
*                                                                               
         GOTO1 =A(FILMG),DMCB,(RC),RR=RELO                                      
         BNE   JOBX                                                             
*                                                                               
         CLI   QMED,0                                                           
         BE    JOB2                                                             
         CLC   QMED,JOBNUM                                                      
         BNE   JOBX                                                             
*                                                                               
JOB2     MVC   BYTE,QFILT          FILTER 1                                     
         GOTO1 APPFIL,EFF1                                                      
         BNE   JOBX                                                             
*                                                                               
         MVC   BYTE,QFILT2         FILTER 2                                     
         GOTO1 APPFIL,EFF2                                                      
         BNE   JOBX                                                             
*                                                                               
         MVC   BYTE,QFILT3         FILTER 3                                     
         GOTO1 APPFIL,EFF3                                                      
         BNE   JOBX                                                             
*                                                                               
         MVC   BYTE,QFILT4         FILTER 4                                     
         GOTO1 APPFIL,EFF4                                                      
         BNE   JOBX                                                             
*                                                                               
         MVC   BYTE,QFILT5         FILTER 5                                     
         GOTO1 APPFIL,EFF5                                                      
         BNE   JOBX                                                             
*                                                                               
JOB4     BAS   RE,STAT             FILTER ON STATUS(CLOSED,LOCKED)              
         BNE   JOBX                                                             
         BAS   RE,FILDATE          FILTER ON START/END DATES                    
         BNE   JOBX                                                             
         BAS   RE,RDOPT            READ OPTIONS-HANDLE MORE FILTERING           
         BNE   JOBX                                                             
*                                                                               
         OC    QSCHEME,QSCHEME     MUST HAVE SCHEME TO DO JOB LOOKUP            
         BNZ   *+14                                                             
         OC    GOSCHEME,GOSCHEME                                                
         BZ    JOBX                NO SCHEME AVAILABLE SO SKIP JOB              
*                                                                               
         BAS   RE,LOOK             PERFORM JOBBER LOOKUP                        
         BAS   RE,FILOVER          FILTER FOR OVERESTIMATE                      
         BNE   JOBX                                                             
*                                                                               
JOB6     BAS   RE,COLN             SET COLUMN NAMES FOR JOB                     
         LA    RF,DOWN                                                          
         CLI   QDOWN,C'Y'          TEST FOR DOWNLOAD OPTION                     
         BE    *+8                                                              
         LA    RF,PRINT            NO-GO TO ROUTINE TO PRINT ESTIMATE           
         BASR  RE,RF                                                            
*                                                                               
JOB8     L     RE,AJOB             RE-READ JOB KEY                              
         MVC   KEY,0(RE)                                                        
         GOTO1 READ                                                             
*                                                                               
JOBX     B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE A RECORD IN A BUFFER, AT ENTRY, R1=A(BUFF ADCON)          
*                                                                               
SAVREC   NTR1  ,                                                                
         L     RE,0(R1)            RE=A(BUFFER)                                 
         L     R4,AIO              R4=A(RECORD TO BE SAVED)                     
         USING ACKEYD,R4                                                        
         LH    R1,ACLENGTH                                                      
         LA    RF,1(R1)                                                         
         LR    R0,R4                                                            
         MVCL  RE,R0               SAVE THE RECORD                              
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER ON OFFICE AND OFFICE GROUP                              
*                                                                               
FILOFF   CLI   QOG,0                                                            
         BNE   FILOFF2                                                          
         CLI   QOFF,0                                                           
         BER   RE                                                               
*                                                                               
FILOFF1  CLC   QOFF,EFFOFFC        MATCH ON REQUESTED OFFICE                    
         BR    RE                                                               
*                                                                               
FILOFF2  CLI   EFFOFFC,C' '        TEST RECORD HAS OFFICE                       
         BNH   FILOFFN             NO-REJECT RECORD                             
         L     R1,AOFFBUFF         R1=A(OFFICE BUFFER)                          
*                                                                               
FILOFF4  CLI   0(R1),0             TEST FOR EOB                                 
         BE    FILOFFN             NO                                           
         CLC   EFFOFFC,0(R1)       MATCH ON OFFICE                              
         BE    FILOFF6                                                          
         LA    R1,3(R1)                                                         
         B     FILOFF4                                                          
*                                                                               
FILOFF6  CLC   QOG,2(R1)           TEST FOR MATCH ON OGROUP                     
         BER   RE                  YES-EXIT WITH CC=EQ                          
*                                                                               
FILOFFN  LTR   RB,RB               SET CC=NEQ                                   
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
* SUB-ROUTINE TO APPLY A FILTER TO JOB'S FILTER                                 
* AT ENTRY, R1=A(JOB FILTER VALUE) BYTE=INPUT FILTER VALUE                      
* ON EXIT, CC=EQ IF OK, CC=NEQ TO REJECT JOB                                    
*                                                                               
APPFIL   CLI   BYTE,0              TEST FOR NO REQUESTED FILTER                 
         BER   RE                  YES-EXIT WITH CC=EQ                          
         CLI   BYTE,C' '           TEST FOR BLANK=TAKE EVERYTHING               
         BER   RE                                                               
         CLI   BYTE,C'.'           TEST FOR NO FILTER ON JOB                    
         BNE   APPFIL2                                                          
         CLI   0(R1),C' '          FILTER POSITION CANNOT BE A VALUE            
         BH    APPFILN                                                          
         B     APPFILY                                                          
*                                                                               
APPFIL2  TM    BYTE,X'40'          TEST FOR POSITIVE FILTER                     
         BZ    APPFIL4                                                          
         CLC   BYTE,0(R1)          TEST FOR MATCH ON FILTER                     
         BE    APPFILY                                                          
         B     APPFILN                                                          
*                                                                               
APPFIL4  OI    BYTE,X'40'          RESTORE UPPER CASE BIT                       
         CLC   BYTE,0(R1)          TEST FOR DIFFERENCE                          
         BNE   APPFILY                                                          
         B     APPFILN                                                          
*                                                                               
APPFILY  CR    RB,RB                                                            
         B     APPFILX                                                          
*                                                                               
APPFILN  LTR   RB,RB                                                            
*                                                                               
APPFILX  BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER ON STATUS                                               
*                                                                               
STAT     CLI   QCLOSE,C'Y'         TEST TO INCLUDE CLOSED JOBS                  
         BE    STAT2               YES                                          
         CLI   QCLOSE,C'C'         TEST FOR CLOSED JOBS ONLY                    
         BE    STAT1                                                            
         TM    JOBSTAT,X'40'       TEST FOR CLOSED JOB                          
         BO    STATN               YES-BOUNCE IT                                
         B     STAT2                                                            
*                                                                               
STAT1    TM    JOBSTAT,X'40'       TEST JOB IS CLOSED                           
         BZ    STATN                                                            
*                                                                               
STAT2    CLI   QLOCK,C'Y'          TEST TO INCLUDE LOCKED JOBS                  
         BE    STATY                                                            
         CLI   QLOCK,C'L'          TEST LOCKED JOBS ONLY                        
         BE    STAT3                                                            
         TM    JOBSTAT,X'20'                                                    
         BO    STATN               BOUNCE LOCKED JOBS                           
         B     STATY                                                            
*                                                                               
STAT3    TM    JOBSTAT,X'20'       TEST JOB IS LOCKED                           
         BZ    STATN               NO-REJECT IT                                 
*                                                                               
STATY    CR    RB,RB               SET CC=EQ                                    
         BR    RE                                                               
*                                                                               
STATN    LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO APPLY START/END DATE FILTERS                                   
*                                                                               
FILDATE  CLC   QSTART(6),=X'000000FFFFFF' TEST NO DATES                         
         BER   RE                  EXIT WITH CC=EQ                              
         ST    RE,SAVERE                                                        
         MVI   ELCODE,ACJBELQ                                                   
         BAS   RE,GETELIO                                                       
         USING ACJOBD,R6                                                        
         CLI   QCLOSE,C'N'         TEST SUPPRESSING CLOSED JOBS                 
         BE    FILDATE2            YES                                          
*                                                                               
FILDATE1 CLC   ACJBCLOS,QSTART     TEST JOB HAS CLOSE DATE W/IN                 
         BL    FILDATN             START/END DATE FILTERS                       
         CLC   ACJBCLOS,QEND                                                    
         BH    FILDATN                                                          
         B     FILDATY                                                          
*                                                                               
FILDATE2 CLC   ACJBOPND,QSTART     TEST OPEN DATE W/IN FILTERS                  
         BL    FILDATN                                                          
         CLC   ACJBOPND,QEND                                                    
         BH    FILDATN                                                          
*                                                                               
FILDATY  CR    RB,RB               SET CC=EQ                                    
         B     FILDATX                                                          
*                                                                               
FILDATN  LTR   RB,RB               SET CC=NEQ                                   
*                                                                               
FILDATX  L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO READ THE OPTIONS AND APPLY MORE FILTERS                        
*                                                                               
RDOPT    ST    RE,SAVERE                                                        
         MVC   GOSELCLI,CLICODE                                                 
         MVC   GOSELPRO,PRODCODE                                                
         MVC   GOSELJOB,JOBNUM                                                  
         XC    GOSELWC,GOSELWC                                                  
         MVC   GOAKEY,AIO                                                       
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
*                                                                               
RDOPT2   CLI   QBT,0               TEST FOR BILLING TYPE FILTER                 
         BE    RDOPTY                                                           
         CLC   QBT,GOBILTYP                                                     
         BNE   RDOPTN                                                           
*                                                                               
         OC    QBILAMT,QBILAMT     TEST BILL AMOUNT FILTER                      
         BZ    RDOPTY              NO-OK                                        
         CLC   QBILAMT,GOBILAM1                                                 
         BE    RDOPTY                                                           
*                                                                               
RDOPTN   LTR   RB,RB                                                            
         B     RDOPTX                                                           
*                                                                               
RDOPTY   CR    RB,RB                                                            
*                                                                               
RDOPTX   L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO LOOK UP JOB VALUES WITH JOBBER                                 
*                                                                               
LOOK     NTR1  ,                                                                
         MVC   SCACOM,ACOMFACS     GET THE JOB'S SCHEME                         
         MVC   SCABUFF,ASCHTAB                                                  
         MVC   SCCUL,CUL                                                        
         MVC   SCSCHEME,GOSCHEME                                                
         OC    QSCHEME,QSCHEME     TEST FOR SCHEME OVERRIDE                     
         BZ    *+10                                                             
         MVC   SCSCHEME,QSCHEME    YES                                          
         GOTO1 VGETSCH,DMCB,SCBLOCK                                             
*                                                                               
LOOK2    MVC   JBAJOB,AJOB         SET UP JOBBER BLOCK                          
         LA    R1,COLLIST                                                       
         ST    R1,JBACOLS                                                       
         MVC   JBACOM,ACOMFACS                                                  
         LA    R1,GOBLOCK                                                       
         ST    R1,JBAGOBLK                                                      
         MVC   JBAIO,AIO2                                                       
         LA    R1,SCBLOCK                                                       
         ST    R1,JBASCH                                                        
         MVC   JBAKEY,AJOB                                                      
         MVC   JBGETOPT,GETOPT                                                  
         MVC   JBSELSCH,SCSCHEME                                                
         CLI   QROUND,C'Y'                                                      
         BE    *+12                                                             
         CLI   QROUND,C'P'                                                      
         BNE   *+8                                                              
         MVI   JBSELRND,JBRNDDOL   SET ROUND TO DOLLARS                         
         CLI   QCROUND,C'N'        ROUNDING COMMISSION ?                        
         BNE   *+8                 YES                                          
         MVI   JBRNDCOM,JBRNDCNO   NO                                           
         MVC   JBGETDFT,QDRAFT     SET DRAFT OPTION                             
         MVC   JBACOLTB,ACOLTAB    COLUMN OUTPUT TABLE                          
         MVC   JBLCOLTB,LCOLTAB                                                 
         MVC   JBAOPVTB,AOPVTAB                                                 
         MVC   JBLOPVTB,LOPVTAB                                                 
         MVC   JBAOE,AOE                                                        
         MVC   JBACE,ACE                                                        
*                                                                               
LOOK4    GOTO1 JOBBER,DMCB,JBLOCK                                               
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOOKX    B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER ON JOBS THAT ARE OVERESTIMATE                           
*                                                                               
FILOVER  CLI   QOVER,C'Y'          TEST FOR OVERESTIMATE FILTER                 
         BNE   FILOVERY            NO-EXIT WITH CC=EQ                           
*                                                                               
         L     R1,ACOLTAB          FIRST ENTRY IS JOB TOTALS                    
         USING JBCOLD,R1                                                        
         LA    RF,JBCOLVAL         RF=A(COLUMN VALUES)                          
         ZIC   R2,MAXCECOL                                                      
         ZIC   R3,ACTCOL                                                        
         BCTR  R2,0                                                             
         BCTR  R3,0                                                             
         MH    R2,=Y(L'JBCOLVAL)   COMPUTE INDEX TO VALUES                      
         MH    R3,=Y(L'JBCOLVAL)                                                
         LA    R2,0(RF,R2)         POINT TO VALUES                              
         LA    R3,0(RF,R3)                                                      
         CP    0(L'JBCOLVAL,R2),0(L'JBCOLVAL,R3)                                
         BH    FILOVERY                                                         
*                                                                               
FILOVERN LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILOVERY CR    RB,RB                                                            
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO SET THE ACTUAL COLUMN NAMES--CALLED FROM JOB-- *               
*****************************************************************               
         SPACE 1                                                                
COLN     NTR1  ,                                                                
         LA    R2,BLOCK            BLOCK=ACTUAL NAMES AREA                      
         LA    R3,NAMELIST         R3=INPUT COLUMN NAMES                        
         LA    R5,COLLIST          R5=A(COLUMN LIST)                            
         USING JBCLD,R5                                                         
         ZIC   R4,NCOLS            R4=LOOP COUNTER                              
*                                                                               
COLN2    MVC   0(L'NAMELIST,R2),0(R3) COPY OVER NAME                            
         CLI   JBCLTYP,JBCLCOL     TEST FOR SINGLE COLUMN                       
         BE    COLN3               YES                                          
         CLI   JBCLTYP,JBCLFRM     NO, IS IT A FORMULA ?                        
         BNE   COLN10              NO                                           
         OC    JBCLSNV,JBCLSNV     YES, IS IT A SUPPLEMENT                      
         BZ    COLN10              NO                                           
         CLC   JBCLCN1,=Y(JBDEST)  YES, IS IT NET ?                             
         BNE   COLN10              NO, SKIP IT                                  
         LA    R1,JBCLCN1E         YES, USE HIGH R                              
         B     COLN4                                                            
*                                                                               
COLN3    LA    R1,JBCLCN1E         R1=A(ESTIMATE TYPE/VERSION)                  
         CLC   JBCLCN1,=Y(JBDEST)                                               
         BE    COLN4                                                            
         LA    R1,JBORGTYP                                                      
         CLC   JBCLCN1,=Y(JBDORG)                                               
         BE    COLN4                                                            
         LA    R1,JBCURTYP                                                      
         CLC   JBCLCN1,=Y(JBDCUR)  TEST FOR NET CURRENT ESTIMATE                
         BE    COLN4                                                            
         B     COLN10                                                           
*                                                                               
COLN4    LA    R6,KEY                                                           
         USING ACEVKEY,R6                                                       
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,CLICODE                                                  
         MVC   ACEVPROD,PRODCODE                                                
         MVC   ACEVJOB,JOBNUM                                                   
         MVC   ACEVTYPE(2),0(R1)                                                
         MVC   AIO,AIO2            USE IO2 FOR SUPPLEMENTARY IO                 
         GOTO1 HIGH                                                             
         CLC   ACEVKEY,KEYSAVE                                                  
         BNE   COLN10              DID NOT FIND IT                              
*                                                                               
COLN6    MVC   NAME1,SPACES                                                     
         MVC   NAME2,SPACES                                                     
         MVI   BYTE,1                                                           
         BAS   RE,GETEN            GET FIRST ESTIMATE NAME                      
         MVI   BYTE,2                                                           
         BAS   RE,GETEN                                                         
         CLC   NAME1(L'NAMELIST),SPACES TEST FOR ANY ESTIMATE NAME              
         BNE   COLN8               YES, GET IT                                  
*                                                                               
         USING ACEUD,R6                                                         
         OC    JBCLSNV,JBCLSNV     NO, IS THIS A SUPPLEMENT ?                   
         BZ    COLN10              NO                                           
         MVI   ELCODE,ACEUELQ      YES, GET UPDATE DATE                         
         BAS   RE,GETELIO                                                       
         BNE   COLN10                                                           
         MVC   NAME1(L'SUPPNAME),SUPPNAME                                       
         CURED (B1,JBCLSNV+1),(2,NAME1+L'SUPPNAME+1),0,ALIGN=LEFT               
         GOTO1 DATCON,DMCB,(1,ACEUADD),(8,NAME2)                                
*                                                                               
COLN8    MVC   0(L'NAMELIST,R2),NAME1 INSERT NAME                               
*                                                                               
COLN10   LA    R2,L'NAMELIST(R2)                                                
         LA    R3,L'NAMELIST(R3)                                                
         LA    R5,JBCLENQ(R5)                                                   
         BCT   R4,COLN2                                                         
         MVC   AIO,AIO1                                                         
*                                                                               
COLNX    B     XIT                                                              
         DROP  R5,R6                                                            
*                                                                               
*                                                                               
* SUB-ROUTINE TO GET THE ESTIMATE NAME (AT ENTRY, BYTE = 1 OR 2)                
*                                                                               
*                                                                               
GETEN    ST    RE,SAVERE                                                        
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('ACENELQ',AIO),(1,BYTE)                
         CLI   12(R1),0                                                         
         BNE   GETENX                                                           
*                                                                               
         L     R6,12(R1)                                                        
         USING ACEND,R6                                                         
         ZIC   R1,ACENLEN                                                       
         AHI   R1,-(ACENAME-ACEND)                                              
         BNP   GETENX                NOTHING THERE, THEN EXIT                   
         LA    RE,L'NAME1                                                       
         CR    R1,RE                                                            
         BL    *+6                                                              
         LR    R1,RE                                                            
         BCTR  R1,0                                                             
         LA    RE,NAME1                                                         
         CLI   BYTE,1              TEST FOR FIRST NAME                          
         BE    *+8                                                              
         LA    RE,NAME2                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ACENAME                                                  
*                                                                               
GETENX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO DOWNLOAD AN ESTIMATE--CALLED FROM JOB---       *               
*****************************************************************               
         SPACE 1                                                                
DOWN     NTR1  ,                                                                
         L     R2,AIO3                                                          
         USING DLCBD,R2            R2=A(DOWNLOAD CONTROL BLOCK)                 
         XC    DLCBD(DLCBXL),DLCBD                                              
         MVI   DLCBACT,DLCBINIT    INITIALIZE CALL                              
         LA    R1,DOWNHK                                                        
         ST    R1,DLCBAPR          PRINT ROUTINE HOOK                           
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         L     R4,BOXAWIDE                                                      
         USING WIDED,R4                                                         
         LA    R1,XP               FORM PRINT LINE ADDRESS                      
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         L     R1,BOXWIDTH                                                      
         AHI   R1,-2                                                            
         STH   R1,DLCXMAXL         MAXIMUM PRINT LINE LENGTH                    
         MVI   DLCXDELC,C' '       DELIMITER CHARACTER                          
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER CHARACTER                     
         MVI   DLCXEOTA,C''''                                                   
         MVI   DLCXEOLC,X'5E'      END OF PRINT LINE IS SEMI-COLON              
         MVI   DLCXEORC,C':'       END OF REPORT CHARACTER                      
*                                                                               
DOWN2    GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
* OUTPUT TWO ROWS OF COLUMN HEADLINES                                           
*                                                                               
DOWN4    MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(8),=C'WORKCODE'                                          
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
         ZIC   R4,NCOLS            R4=LOOP COUNTER                              
         LA    R5,BLOCK            R5=A(COLUMN NAMES)                           
*                                                                               
DOWN6    MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(L'NAME1),0(R5)                                           
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
         LA    R5,L'NAMELIST(R5)                                                
         BCT   R4,DOWN6                                                         
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
DOWN8    MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(11),=C'DESCRIPTION'                                      
         GOTO1 VDLFLD,DLCBD                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R4,NCOLS                                                         
         LA    R5,BLOCK                                                         
*                                                                               
DOWN10   MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(L'NAME2),L'NAME1(R5)                                     
         GOTO1 VDLFLD,DLCBD                                                     
         LA    R5,L'NAMELIST(R5)                                                
         BCT   R4,DOWN10                                                        
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 VDLFLD,DLCBD                                                     
         TM    DLCBRETC,DLCBRCNF   TEST FOR ERROR                               
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* OUTPUT THE ROWS                                                               
*                                                                               
DOWN12   L     R5,ACOLTAB          R5=A(COLUMN OUTPUT TABLE)                    
         USING JBCOLD,R5                                                        
         LH    R4,JBNROWS                                                       
         MVI   SWSTRT,C'N'                                                      
*                                                                               
DOWN14   CLI   JBCOLTYP,JBCOLTWC   TEST FOR WORKCODE ROW                        
         BE    *+12                                                             
         CLI   JBCOLTYP,JBCOLTAG   TEST FOR AGENCY COMMISSION ROW               
         BNE   DOWN16                                                           
*                                                                               
         CLC   JBCOLCAT,LASTCAT    TEST CHANGE IN CATEGORY                      
         BE    *+12                                                             
         MVI   COMNUM,1            RESET COMMISSION AND SUB-TOTAL               
         MVI   SUBNUM,1            NUMBERS                                      
         MVC   LASTCAT,JBCOLCAT                                                 
*                                                                               
         GOTO1 FILCAT,NCATS                                                     
         BNE   DOWN16              REJECT IT                                    
         BAS   RE,FILZERO                                                       
         BE    DOWN16                                                           
         GOTO1 FILWC,NWC                                                        
         BNE   DOWN16              REJECT IT                                    
         BAS   RE,FILSW                                                         
         BNE   DOWN16                                                           
*                                                                               
         GOTO1 AWN,DMCB,(RC)                                                    
         MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBTXT                                                  
         MVC   DLCBFLD(L'ROWNAME),ROWNAME                                       
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
         ZIC   R0,NCOLS            R0=LOOP COUNTER                              
         LA    R3,JBCOLVAL         R3=A(COLUMN VALUES)                          
         LA    R6,COLLIST          R6=A(COLUMN LIST ENTRY)                      
         USING JBCLD,R6                                                         
*                                                                               
DOWN15   MVI   DLCBACT,DLCBPUT                                                  
         MVI   DLCBTYP,DLCBPACK                                                 
         MVC   DLCBFLD(L'JBCOLVAL),0(R3) PASS VALUE                             
         MVI   DLCBLEN,L'JBCOLVAL                                               
         MVI   DLCBNDP,2           TWO DECIMAL PLACES                           
         CLC   JBCLCN1,COMRATE     TEST FOR COMMISSION RATE                     
         BNE   *+8                                                              
         MVI   DLCBNDP,4                                                        
         GOTO1 VDLFLD,DLCBD                                                     
         LA    R3,L'JBCOLVAL(R3)                                                
         LA    R6,JBCLENQ(R6)                                                   
         BCT   R0,DOWN15                                                        
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 VDLFLD,DLCBD                                                     
         TM    DLCBRETC,DLCBRCNF   TEST FOR UNEQUAL COLUMNS                     
         BZ    *+6                                                              
         DC    H'0'                YES-TAKE A HIT                               
*                                                                               
DOWN16   AH    R5,JBLCOL                                                        
         BCT   R4,DOWN14           NEXT ROW                                     
*                                                                               
DOWN18   MVI   DLCBACT,DLCBEOR     END OF REPORT                                
         GOTO1 VDLFLD,DLCBD                                                     
*                                                                               
DOWNX    B     XIT                                                              
         SPACE 2                                                                
* HOOK ROUTINE FOR DOWNLOAD                                                     
*                                                                               
DOWNHK   NTR1  ,                                                                
         GOTO1 SPOOL,PARAS,ASPOOLD                                              
         MVI   LINE,1              MAKE SURE THERE IS NO PAGE BREAK             
         B     XIT                                                              
         DROP  R2,R3,R4,R6                                                      
         EJECT                                                                  
* SUB-ROUTINE TO FILTER OUT ZERO LINES                                          
* AT ENTRY, R5=A(COLUMN OUTPUT ENTRY)                                           
* ON EXIT, CC=EQ TO REJECT ZERO ROW, CC=NEQ TO INCLUDE ROW                      
*                                                                               
         USING JBCLD,R6                                                         
FILZERO  NTR1                                                                   
         CLI   QZERO,C'S'          TEST FOR ZERO SUPPRESSION                    
         BE    *+12                YES                                          
         CLI   QZERO,C'A'          TEST ZERO SUPPRESS BUT SHOW AGY COMM         
         BNE   FILZXIT             CATEGORY - NO                                
*                                                                               
         ZIC   R0,NCOLS                                                         
         LA    R1,JBCOLVAL                                                      
         LA    R6,COLLIST                                                       
*                                                                               
FILZ02   CLI   JBCLTYP,JBCLCOL     IS THIS A SINGLE COLUMN ?                    
         BNE   FILZ04              NO, CAN'T BE RATE THEN                       
         CLC   JBCLCN1,COMRATE     IS THIS A RATE ?                             
         BE    FILZ06              YES,SKIP OVER IT                             
*                                                                               
FILZ04   CP    0(L'JBCOLVAL,R1),=P'0'                                           
         BNE   FILZXIT                                                          
*                                                                               
FILZ06   LA    R1,L'JBCOLVAL(R1)                                                
         LA    R6,JBCLENQ(R6)                                                   
         BCT   R0,FILZ02                                                        
*                                                                               
         CLI   QZERO,C'S'          TEST FOR FULL ZERO SUPPRESS                  
         BE    FILZXIT             YES                                          
*                                                                               
         CLI   JBCOLTYP,JBCOLTCA   TEST FOR AGY COMM CAT                        
         BE    *+10                YES-SHOW IT                                  
         CR    RB,RB               SET CC=EQ TO REJECT                          
         B     FILZXIT                                                          
*                                                                               
         LTR   RB,RB               SET CC=NEQ TO INCLUDE                        
*                                                                               
FILZXIT  XIT1                                                                   
         DROP  R6                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER AGAINST CATEGORY LIST                                   
* AT ENTRY, R5=A(COLUMN TABLE ENTRY) AND R1=A(COUNTER/LIST)                     
* ON EXIT, CC=EQ IF OK ,CC=NEQ TO REJECT CATEGORY                               
*                                                                               
FILCAT   CLI   0(R1),0             TEST FOR NO FILTERS                          
         BER   RE                                                               
         ZIC   R0,0(R1)                                                         
         LA    R1,1(R1)            BUMP AHEAD TO FILTERS                        
         TM    0(R1),X'40'         TEST POSITIVE FILTER                         
         BZ    FILCAT2             NO                                           
*                                                                               
FILCAT1  CLC   JBCOLCAT,0(R1)                                                   
         BE    FILCATY                                                          
         LA    R1,L'JBCOLCAT(R1)                                                
         BCT   R0,FILCAT1                                                       
         B     FILCATN                                                          
*                                                                               
FILCAT2  MVC   HALF,0(R1)                                                       
         OI    HALF,X'40'                                                       
         CLC   JBCOLCAT,HALF                                                    
         BE    FILCATN                                                          
         LA    R1,L'JBCOLCAT(R1)                                                
         BCT   R0,FILCAT2                                                       
*                                                                               
FILCATY  CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILCATN  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
         SPACE 2                                                                
* SUB-ROUTINE TO FILTER AGAINST WORKCODE LIST                                   
* AT ENTRY, R5=A(COLUMN TABLE ENTRY) AND R1=A(COUNTER/LIST)                     
* ON EXIT, CC=EQ IF OK ,CC=NEQ TO REJECT WORKCODE                               
*                                                                               
FILWC    CLI   0(R1),0             TEST FOR NO FILTERS                          
         BER   RE                                                               
         ZIC   R0,0(R1)                                                         
         LA    R1,1(R1)            BUMP AHEAD TO FILTERS                        
         TM    0(R1),X'40'         TEST POSITIVE FILTER                         
         BZ    FILWC2              NO                                           
*                                                                               
FILWC1   CLC   JBCOLWC,0(R1)                                                    
         BE    FILWCY                                                           
         LA    R1,L'JBCOLWC(R1)                                                 
         BCT   R0,FILWC1                                                        
         B     FILWCN                                                           
*                                                                               
FILWC2   MVC   HALF,0(R1)                                                       
         OI    HALF,X'40'                                                       
         CLC   JBCOLWC,HALF                                                     
         BE    FILWCN                                                           
         LA    R1,L'JBCOLWC(R1)                                                 
         BCT   R0,FILWC2                                                        
*                                                                               
FILWCY   CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILWCN   LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO FILTER AGAINST WORKCODE START                                  
* AT ENTRY, R5=A(COLUMN TABLE ENTRY)                                            
* ON EXIT, CC=EQ IF OK ,CC=NEQ TO REJECT WORKCODE                               
*                                                                               
FILSW    CLI   QSW,0                                                            
         BER   RE                                                               
         CLI   SWSTRT,C'Y'         DID WE FIND START ALREADY ?                  
         BER   RE                  YES, OK                                      
         CLC   JBCOLWC,QSW                                                      
         BNE   FILSWN                                                           
         MVI   SWSTRT,C'Y'         INDICATE WE FOUND A START                    
*                                                                               
FILSWY   CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
FILSWN   LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
*****************************************************************               
* SUB-ROUTINE TO PRINT AN ESTIMATE FOR A JOB--CALLED FROM JOB   *               
*****************************************************************               
         SPACE 1                                                                
PRINT    NTR1  ,                                                                
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK         SET A(HOOK ROUTINE)                          
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         MVC   REPWIDTH,BOXWIDTH+3                                              
         MVC   AWIDE,BOXAWIDE                                                   
         SR    RE,RE                                                            
         ICM   RE,1,QMAXLINE                                                    
         BZ    *+10                NO MAXLINES OVERRIDE                         
         BCTR  RE,0                                                             
         STC   RE,MAXLINES                                                      
         MVI   BOXFONT,0                                                        
         CLI   QWIDE,C'Y'          TEST FOR WIDE LINE REPORT                    
         BNE   *+8                                                              
         MVI   BOXFONT,1           YES-CHANGE THE FONT TO DG15                  
*                                                                               
         CLI   PROGPROF+2,C'Y'     USE LONG NAMES?                              
         BNE   *+8                 NO                                           
         MVI   QSQUEEZE,0          YES, CLEAR SQUEEZE                           
*                                                                               
         LA    R1,WCRDISPQ                                                      
         SR    R0,R0               DEAL W HORRIBLE YNR SQUEEZE OPTION           
         ICM   R0,1,QSQUEEZE       GET SQUEEZE AMOUNT                           
         BZ    *+6                 NONE                                         
         SR    R1,R0               YES-DEDUCT IT FROM WORKCODE COL LEN          
         STH   R1,WCRDISP          SET DISP TO RIGHT SIDE OF WC                 
*                                                                               
         BAS   RE,WCOLS            SET WORKCODE COLUMNS                         
         XC    LASTCAT,LASTCAT                                                  
         MVI   FOOTSW,C'N'                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT THE HEADER AREA (SUB-TITLE, HEADER, SUB-HEADER)                         
*                                                                               
PRINT1   GOTO1 JOBTXT,DMCB,C'ST',0                                              
*                                                                               
PRINT2   GOTO1 JOBTXT,DMCB,C'H ',0                                              
*                                                                               
PRINT4   GOTO1 JOBTXT,DMCB,C'SH',0                                              
*                                                                               
* PRINT THE FIRST COLUMN HEADINGS                                               
*                                                                               
PRINT6   CLI   QPBWORK,C'Y'        PAGE BREAK BEFORE DETAIL ?                   
         BE    PRINT6A             YES                                          
         ZIC   R1,LINE                                                          
         LA    R1,4(R1)            TEST IF ROOM FOR COLUMN HEADINGS             
         LA    RF,COLHED                                                        
         CLM   R1,1,MAXLINES                                                    
         BL    *+8                                                              
*                                                                               
PRINT6A  LA    RF,BREAK                                                         
         BASR  RE,RF                                                            
*                                                                               
PRINT7   L     R5,ACOLTAB                                                       
         USING JBCOLD,R5                                                        
         LH    R4,JBLCOL                                                        
         LH    R6,JBNROWS                                                       
         MVI   SWSTRT,0                                                         
*                                                                               
         LA    R5,0(R4,R5)                                                      
         AHI   R6,-1                                                            
         BZ    PRINT20             ONLY A JOB TOTAL LINE                        
*                                                                               
* PRINT THE JOB DETAIL IN A LOOP                                                
*                                                                               
PRINT8   GOTO1 FILCAT,NCATS                                                     
         BNE   PRINT12             REJECT CATEGORY                              
         GOTO1 FILWC,NWC                                                        
         BNE   PRINT12             REJECT WORKCODE                              
         BAS   RE,FILSW                                                         
         BNE   PRINT12             REJECT WORKCODE                              
         CLC   JBCOLCAT,LASTCAT    TEST FOR CHANGE IN CATEGORY                  
         BE    PRINT9                                                           
         CLI   JBCOLTYP,JBCOLTCA   TEST FOR AGENCY COMM CAT                     
         BE    *+12                                                             
         CLI   JBCOLTYP,JBCOLTCF   OR FORMULA CATEGORY                          
         BNE   *+16                                                             
         BAS   RE,FILZERO                                                       
         BE    PRINT12             REJECT ZERO LINE                             
         B     PRINT8A             TAKE THE LINE                                
*                                                                               
         GOTO1 =A(CATNAME),DMCB,(RC),RR=RELO    GET CATEGORY NAME               
         L     RE,ACATEL                                                        
         USING ACCDD,RE                                                         
         TM    ACCDSTAT,ACCDCONT   TEST FOR CONTINUATION                        
         BO    PRINT9              YES-SKIP OVER HEADER ROUTINE                 
         DROP  RE                                                               
*                                                                               
         BAS   RE,TSTCAT           TEST THE CATEGORY FOR INCLUSION              
         BE    PRINT8A             YES                                          
         LTR   R6,R6               TEST IF AT EOT                               
         BP    PRINT8              NO-START WITH NEW CATEGORY                   
         B     PRINT15             YES-WRAP UP                                  
*                                                                               
PRINT8A  BAS   RE,CATHED                                                        
*                                                                               
PRINT9   CLI   JBCOLTYP,JBCOLTWC                                                
         BE    PRINT10             TEST FOR WORKCODE LINE                       
         CLI   JBCOLTYP,JBCOLTSB                                                
         BE    PRINT10                                                          
         CLI   JBCOLTYP,JBCOLTAG                                                
         BE    PRINT10                                                          
*                                                                               
         BAS   RE,CATDET                                                        
         BAS   RE,CATFOOT                                                       
         B     PRINT12                                                          
*                                                                               
PRINT10  CLI   QWCDET,C'N'         TEST FOR WORKCODE SUPPRESSION                
         BE    PRINT12             YES                                          
         GOTO1 FILCAT,NDETS        FILTER ON WC DETAIL CATEGORIES               
         BNE   PRINT12             SKIP IT                                      
         BAS   RE,FILZERO          FILTER ZERO LINES                            
         BE    PRINT12             REJECT LINE                                  
         BAS   RE,WC                                                            
*                                                                               
PRINT12  LA    R5,0(R4,R5)                                                      
         BCT   R6,PRINT8                                                        
*                                                                               
* PRINT JOB TOTALS LINE                                                         
*                                                                               
PRINT15  CLI   QJOBTOT,C'N'        TEST IF JOB TOTALS SUPPRESSED                
         BE    *+8                 YES                                          
         BAS   RE,TOTAL                                                         
         ZIC   R1,LINE             CLOSE OFF BOXING                             
         LA    R1,1(R1)                                                         
         CLM   R1,1,MAXLINES       TEST FOR PAGE BREAK                          
         BL    PRINT16             NO                                           
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         B     PRINT17                                                          
*                                                                               
PRINT16  L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         MVI   BOXREQ,C'C'         CLOSE OFF BOX                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRINT17  MVC   BOXCOLS,SPACES      SHUT OFF BOXES                               
         MVC   BOXCOLSR,SPACES                                                  
*                                                                               
* PRINT FOOTER AREA                                                             
*                                                                               
PRINT20  MVI   FOOTSW,C'Y'                                                      
         BAS   RE,SKIPLIN                                                       
         GOTO1 JOBTXT,DMCB,C'F ',0                                              
         GOTO1 JOBTXT,DMCB,C'SF',0                                              
*                                                                               
PRINTX   B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO PRINT THE HEADLINES                           *                
****************************************************************                
         SPACE 1                                                                
HOOK     NTR1  ,                                                                
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
         MVI   BOXROWS,C'T'        ALWAYS SET A TOP AND BOTTOM                  
         ZIC   RE,MAXLINES                                                      
         LA    RE,BOXROWS(RE)                                                   
         MVI   0(RE),C'B'          SET BOX BOTTOM                               
         LA    RE,BUFF             SAVE TEXT BLOCK                              
         LA    RF,TXBLOCKL                                                      
         LA    R0,TXBLOCK                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
HOOK1    GOTO1 JOBTXT,DMCB,C'T ',0                                              
         BE    HOOK2               WE HAVE A TITLE THERE                        
*                                                                               
         GOTO1 =A(BLDT),DMCB,(RC),RR=RELO  BUILD PANEL FOR TITLE                
         MVI   TXSELFUN,TXPROPAN                                                
         MVC   TXSELCAP,QCAPS      CAPITALS OPTION                              
         GOTO1 VGETTXT,DMCB,TXBLOCK                                             
*                                                                               
HOOK2    LA    RE,TXBLOCK                                                       
         LA    RF,TXBLOCKL                                                      
         LA    R0,BUFF                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         XC    TXLPAN,TXLPAN       CLEAR PANEL TO FORCE RE-LOOK UP              
*                                                                               
HOOKX    B     XIT                                                              
         SPACE 2                                                                
***************************************************************                 
* SUB-ROUTINE TO PRINT THE JOB TOTALS--CALLED FROM PRINT      *                 
***************************************************************                 
         SPACE 1                                                                
TOTAL    NTR1  ,                                                                
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         ZIC   R1,LINE                                                          
         LA    R1,3(R1)            TEST IF TOTALS LINE FITS                     
         CLM   R1,1,MAXLINES                                                    
         BL    TOTAL2                                                           
         BAS   RE,BREAK                                                         
*                                                                               
TOTAL2   MVI   BOXREQ,C'O'         NEED TO OPEN A BOX IF                        
         CLC   BOXCOLS,SPACES      BOXCOLS/R ARE SPACES                         
         BNE   *+14                                                             
         CLC   BOXCOLSR,SPACES                                                  
         BE    *+8                                                              
         MVI   BOXREQ,C'B'         ELSE JUST DRAW A LINE BEFORE TOTALS          
*                                                                               
         LM    RE,RF,AWRKCOL1                                                   
         MVC   BOXCOLS,0(RE)                                                    
         MVC   BOXCOLSR,0(RF)                                                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
TOTAL4   L     R2,AWIDE                                                         
         USING WIDED,R2                                                         
         MVC   XP+WCDISP(10),=C'JOB TOTALS'                                     
         L     R5,ACOLTAB                                                       
         GOTO1 ADISVAL,DMCB,(RC)                                                
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
TOTALX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO PERFORM A PAGE BREAK                           *               
*****************************************************************               
         SPACE 1                                                                
BREAK    NTR1  ,                                                                
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         L     RE,ASAVCOL1         SAVE EXISTING BOX COLUMNS                    
         MVC   0(L'BOXCOLS,RE),BOXCOLS                                          
         L     RE,ASAVCOL2                                                      
         MVC   0(L'BOXCOLSR,RE),BOXCOLSR                                        
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         CLI   FOOTSW,C'Y'         TEST PRINTING FOOTER                         
         BE    *+8                                                              
         BAS   RE,COLHED           GENERATE COLUMN HEADINGS                     
         L     RE,ASAVCOL1                                                      
         MVC   BOXCOLS,0(RE)                                                    
         L     RE,ASAVCOL2         RESTORE EXISTING BOX COLUMNS                 
         MVC   BOXCOLSR,0(RE)                                                   
         B     XIT                                                              
         SPACE 2                                                                
**********************************************************                      
* SUB-ROUTINE TO PERFORM A LINE SKIP                     *                      
**********************************************************                      
         SPACE 1                                                                
SKIPLIN  ST    RE,SAVERE                                                        
         ZIC   RE,LINE                                                          
         LA    RE,1(RE)                                                         
         CLM   RE,1,MAXLINES       TEST FOR POSSIBLE PAGE BREAK                 
         BNL   SKIPLINX            IGNORE LINE SKIP                             
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
SKIPLINX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO GENERATE THE COLUMN HEADINGS--CALLED FROM     *                
* PRINT AND BREAK                                              *                
****************************************************************                
         SPACE 1                                                                
COLHED   NTR1  ,                                                                
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         CLI   QBOXES,C'C'                                                      
         BE    *+12                                                             
         CLI   QBOXES,C'N'         TEST FOR BOX SUPPRESSION                     
         BNE   COLHED1                                                          
         MVI   BOXYORN,C'N'        YES NO-OP THE BOX CODE                       
         MVI   BOXOFF,C'Y'                                                      
*                                                                               
COLHED1  MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
*                                                                               
COLHED2  CLI   QBOXES,C'C'         TEST FOR BOX COMPRESSION                     
         BE    COLHED4                                                          
         LM    RE,RF,AWRKCOL1                                                   
         MVC   BOXCOLS,0(RE)       OPEN A NEW BOX                               
         MVC   BOXCOLSR,0(RF)                                                   
         MVI   BOXREQ,C'O'                                                      
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         PUSH  USING                                                            
*                                                                               
COLHED4  L     R8,AWIDE                                                         
         USING WIDED,R8                                                         
         CLI   QDES,C'N'           SUPPRESS 'DESCRIPTION' ?                     
         BE    *+10                YES                                          
         MVC   XP+WCDISP(11),=C'DESCRIPTION'                                    
         LA    R4,XP+1             R4=FIRST PRINT LINE POSITION                 
         AH    R4,WCRDISP                                                       
         LA    R5,L'XP(R4)         R5=UNDERNEATH FIRST LINE                     
         ZIC   R6,NCOLS                                                         
         LA    R2,BLOCK                                                         
*                                                                               
COLHED6  MVC   0(L'NAME1,R4),0(R2) TOP HALF OF NAME                             
         CLC   0(L'NAME1,R4),XSPACES TEST FOR SPACES                            
         BE    COLHED8             YES                                          
         CLI   QCAPS,C'Y'                                                       
         BNE   *+10                                                             
         OC    0(L'NAME1,R4),XSPACES                                            
         CLI   QRIGHT,C'Y'         TEST FOR RIGHT JUSTIFIED NAMES               
         BE    COLHED7             YES                                          
         GOTO1 CENTER,DMCB,(R4),L'NAME1                                         
         B     COLHED8                                                          
*                                                                               
COLHED7  GOTO1 VRIGHT,DMCB,(R4),L'NAME1                                         
         MVI   DUB,C'N'            SET FLAG FOR LEFT SHIFT                      
         CLI   0(R4),C' '          TEST LEFTMOST POSITION USED                  
         BNE   COLHED8             YES                                          
         MVI   DUB,C'Y'                                                         
         MVC   0(L'NAME1-1,R4),1(R4) NO-MOVE ONE POSITION TO LEFT               
         MVI   L'NAME1-1(R4),C' '                                               
*                                                                               
COLHED8  MVC   0(L'NAME2,R5),L'NAME1(R2) BOTTOM HALF OF NAME                    
         CLC   0(L'NAME2,R5),XSPACES                                            
         BE    COLHED10                                                         
         CLI   QCAPS,C'Y'                                                       
         BNE   *+10                                                             
         OC    0(L'NAME2,R5),XSPACES                                            
         CLI   QRIGHT,C'Y'         TEST TO RIGHT JUSTIFY NAME                   
         BE    COLHED9             YES                                          
         GOTO1 CENTER,DMCB,(R5),L'NAME2                                         
         B     COLHED10                                                         
*                                                                               
COLHED9  GOTO1 VRIGHT,DMCB,(R5),L'NAME2                                         
         CLI   0(R5),C' '          TEST LEFTMOST POSITION USED                  
         BE    COLHED9A            NO                                           
         CLI   DUB,C'N'            TEST IF LEFT SHIFTED FIRST NAME              
         BE    COLHED10            NO                                           
         GOTO1 VRIGHT,DMCB,(R4),L'NAME1                                         
         B     COLHED10                                                         
*                                                                               
COLHED9A CLI   DUB,C'N'            TEST FIRST NAME LEFT SHIFTED                 
         BE    COLHED10            NO-DON'T SHIFT SECOND                        
         MVC   0(L'NAME2-1,R5),1(R5)                                            
         MVI   L'NAME2-1(R5),C' '                                               
*                                                                               
COLHED10 LA    R4,COLDISP(R4)                                                   
         LA    R5,COLDISP(R5)                                                   
         LA    R2,L'NAMELIST(R2)                                                
         BCT   R6,COLHED6                                                       
*                                                                               
COLHED15 GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
COLHEDX  B     XIT                                                              
         POP   USING               RESTORE USING SETTINGS                       
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO SET WORKCODE DETAIL COLUMNS--CALLED FROM PRINT  *              
******************************************************************              
         SPACE 1                                                                
WCOLS    ST    RE,SAVERE                                                        
         L     RE,AWRKCOL1                                                      
         MVI   LCOLDISP(RE),C'L'                                                
         AH    RE,WCRDISP                                                       
         MVI   0(RE),C'C'                                                       
         ZIC   R0,NCOLS            INSERT BOXING FOR COLUMN NAMES               
*                                                                               
         LA    RE,COLDISP(RE)                                                   
         MVI   0(RE),C'C'                                                       
         BCT   R0,*-8                                                           
*                                                                               
         MVI   0(RE),C'R'                                                       
         CLI   NCOLS,MINCOLS       TEST FOR MINIMUM COLUMNS                     
         BNL   WCOLS2              YES                                          
         MVI   0(RE),C'C'                                                       
         L     RE,AWRKCOL1                                                      
         LA    RE,MINCDISQ(RE)     FIND POSITION OF RIGHT HAND MARGIN           
*                                                                               
         CLI   QSQUEEZE,0          TEST SQUEEZED ESTIMATE                       
         BE    *+12                                                             
         SR    RF,RF                                                            
         IC    RF,QSQUEEZE                                                      
         SR    RE,RF               DEDUCT SQUEEZE FROM RIGHT MARGIN             
*                                                                               
         MVI   0(RE),C'R'          FORCE IN RIGHT HAND MARGIN                   
*                                                                               
WCOLS2   L     R1,AOUTCOL1                                                      
         MVI   LCOLDISP(R1),C'L'                                                
         L     RF,AWRKCOL1                                                      
         SR    RE,RF               FIND DISP INTO WRKCOLS                       
         LA    RE,0(R1,RE)         POINT TO RIGHT HAND MARGIN                   
         MVI   0(RE),C'R'                                                       
*                                                                               
WCOLSX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
******************************************************************              
* SUB-ROUTINE TO TEST THE CATEGORY FOR INCLUSION IN THE ESTIMATE *              
* AT ENTRY, R5=A(COLUMN ENTRY), R4=L'COLUMN ENTRY, R6=COUNTER    *              
* ON EXIT, CC=EQ TO INCLUDE CATEGORY, CC=NEQ TO REJECT CATEGORY  *              
* AND R4-R6 WILL REFLECT NEXT CATEGORY POSITION OR EOT           *              
******************************************************************              
         SPACE 1                                                                
TSTCAT   NTR1  ,                                                                
         USING JBCOLD,R5                                                        
         MVC   HALF,JBCOLCAT                                                    
         CLC   HALF,SLUSHCAT       TEST FOR SLUSH CATEGORY                      
         BE    TSTCAT2             YES-ALWAYS APPLY ZERO TEST                   
         CLI   QZERO,C'S'          TEST ZERO LINE SUPPRESSION                   
         BE    TSTCAT2                                                          
         CLI   QZERO,C'A'          TEST FOR SPECIAL ZERO SUPPRESS               
         BNE   TSTCATY             NO-TAKE THE CATEGORY                         
*                                                                               
TSTCAT2  CLC   JBCOLCAT,HALF       TEST SAME CATEGORY                           
         BE    TSTCAT3             YES                                          
*                                                                               
         GOTO1 =A(CATNAME),DMCB,(RC),RR=RELO    GET CATEGORY NAME               
         L     RE,ACATEL                                                        
         USING ACCDD,RE                                                         
         TM    ACCDSTAT,ACCDCONT   TEST IF ITS A CONTINUATION                   
         BZ    TSTCATN             NO-SO ALL ROWS MUST BE ZERO                  
*                                                                               
TSTCAT3  ZIC   R0,NCOLS                                                         
         LA    RE,JBCOLVAL                                                      
         CP    0(L'JBCOLVAL,RE),=P'0'                                           
         BNE   TSTCATY             ONE ITEM IS NON-ZERO                         
         LA    RE,L'JBCOLVAL(RE)                                                
         BCT   R0,*-14                                                          
*                                                                               
TSTCAT4  LA    R5,0(R4,R5)         NEXT ENTRY                                   
         BCT   R6,TSTCAT2                                                       
         B     TSTCATN             ALL ROWS ARE ZERO                            
*                                                                               
TSTCATY  B     OKXIT                                                            
*                                                                               
TSTCATN  LTR   RB,RB               SET CC=NEQ                                   
         XIT1  REGS=(R4,R6)        EXIT RETURNING REGISTERS                     
         DROP  RE                                                               
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO PRINT THE CATEGORY HEADER--CALLED FROM PRINT *                 
* AT ENTRY, R5=A(COLUMN OUTPUT ENTRY)                         *                 
* NOTE-FIRST ENTRY FOR REGULAR CATEGORY IS A WORKCODE ENTRY   *                 
***************************************************************                 
         SPACE 1                                                                
CATHED   NTR1  ,                                                                
         USING JBCOLD,R5                                                        
         L     R2,AWIDE                                                         
         USING WIDED,R2                                                         
         MVI   COMNUM,1            HOUSEKEEPING                                 
         MVI   SUBNUM,1                                                         
         MVC   LASTCAT,JBCOLCAT                                                 
         GOTO1 =A(CATNAME),DMCB,(RC),RR=RELO    GET CATEGORY NAME               
*                                                                               
* GET THE HEADER TEXT FOR CATEGORY, SKIP LINE(S) BEFORE NEW CATEGORY            
*                                                                               
CATHED2  GOTO1 JOBTXT,DMCB,C'H ',(C'C',JBCOLD)                                  
*                                                                               
* SEE IF CATEGORY HEADER WILL FIT ON PAGE                                       
*                                                                               
CATHED4  ZIC   R0,TXACTNUM                                                      
         ZIC   R1,LINE                                                          
         AR    R1,R0                                                            
         LA    R1,2(R1)                                                         
         CLI   QBOXES,C'C'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         CLI   JBCOLTYP,JBCOLTCA                                                
         BE    CATHED5                                                          
         CLI   JBCOLTYP,JBCOLTCF                                                
         BE    CATHED5                                                          
         LA    R1,5(R1)            YES-ADD IN ALLOWANCE FOR W/C DETAIL          
*                                                                               
CATHED5  MVI   BYTE,C'N'           SET PAGE BREAK SWITCH                        
         CLM   R1,1,MAXLINES       TEST FOR FIT                                 
         BL    CATHED6             YES                                          
         BAS   RE,BREAK                                                         
         MVI   BYTE,C'Y'           NOTE PAGE BREAK TAKEN                        
*                                                                               
* DRAW A LINE ABOVE HEADER AND PRINT CATEGORY NAME IF NEEDED                    
*                                                                               
CATHED6  CLI   QBOXES,C'C'                                                      
         BE    CATHED7                                                          
         MVI   BOXREQ,C'C'         CLOSE OFF THE BOX ABOVE                      
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
*                                                                               
CATHED7  BAS   RE,SKIPLIN                                                       
         CLI   JBCOLTYP,JBCOLTCA   SKIP HEADLINE FOR CATEGORY                   
         BE    CATHED8             COMMISSION ROWS AND FORMULA ROWS             
         CLI   JBCOLTYP,JBCOLTCF                                                
         BE    CATHED8                                                          
         CLI   QCN,C'N'            SUPPRESS CATEGORY NAME ?                     
         BE    CATHED8             YES                                          
         L     RE,ACATEL                                                        
         TM    ACCDSTAT-ACCDD(RE),ACCDNPRT                                      
         BO    CATHED8                                                          
         MVC   XP+WCDISP(L'ROWNAME),ROWNAME                                     
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         CLI   QBOXES,C'C'                                                      
         BE    *+8                                                              
         BAS   RE,SKIPLIN                                                       
*                                                                               
* PRINT ANY HEADER TEXT                                                         
*                                                                               
CATHED8  CLI   BYTE,C'N'           TEST FOR PAGE BREAK                          
         BE    CATHED10                                                         
         GOTO1 JOBTXT,DMCB,C'H ',(C'C',JBCOLD)                                  
*                                                                               
CATHED10 CLI   TXACTNUM,0                                                       
         BE    CATHEDX                                                          
         GOTO1 PRT,DESCDISP                                                     
         BAS   RE,SKIPLIN                                                       
*                                                                               
CATHEDX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PRINT THE CATEGORY DETAIL LINE                                 
*                                                                               
CATDET   NTR1  ,                                                                
         USING JBCOLD,R5                                                        
         L     R6,ACATEL                                                        
         USING ACCDD,R6                                                         
         L     R2,AWIDE                                                         
         USING WIDED,R2                                                         
         CLI   JBCOLTYP,JBCOLTCT                                                
         BNE   CATDET2                                                          
         TM    ACCDSTAT,ACCDNOTO+ACCDCONT  TEST EITHER NOTOTAL OR CONT.         
         BNZ   CATDETX                                                          
*                                                                               
CATDET2  ZIC   R1,LINE                                                          
         LA    R1,2(R1)            TEST FOR FIT ON PAGE                         
         CLM   R1,1,MAXLINES                                                    
         BL    *+8                                                              
         BAS   RE,BREAK                                                         
*                                                                               
CATDET4  CLI   QBOXES,C'C'         TEST FOR BOX COMPRESSION                     
         BE    CATDET6             YES                                          
         MVI   BOXREQ,C'O'         DRAW A LINE ACROSS PAGE                      
         CLC   BOXCOLS,SPACES                                                   
         BNE   *+14                                                             
         CLC   BOXCOLSR,SPACES                                                  
         BE    *+8                                                              
         MVI   BOXREQ,C'B'                                                      
*                                                                               
         LM    RE,RF,AWRKCOL1                                                   
         MVC   BOXCOLS,0(RE)       SET WORKCODE COLUMNS                         
         MVC   BOXCOLSR,0(RF)                                                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
CATDET6  CLI   JBCOLTYP,JBCOLTCA   TEST AGENCY COMMISSION CATEGORY              
         BNE   *+14                                                             
         MVC   XP+DESCDISP(17),=C'AGENCY COMMISSION'                            
         B     CATDET7                                                          
*                                                                               
         MVC   XP+DESCDISP(5),=C'TOTAL'                                         
         LA    RE,XP+DESCDISP+6                                                 
         MVC   0(L'ACCDNAME,RE),ACCDNAME                                        
*                                                                               
CATDET7  CLC   ACCDTNAM,SPACES                                                  
         BNH   CATDET8                                                          
         MVC   XP+DESCDISP(L'ACCDNAME+6),SPACES                                 
         MVC   XP+DESCDISP(L'ACCDTNAM),ACCDTNAM                                 
*                                                                               
CATDET8  CLI   QCAPS,C'Y'                                                       
         BNE   *+10                                                             
         OC    XP+DESCDISP(L'ACCDNAME+6),SPACES                                 
         GOTO1 ADISVAL,DMCB,(RC)                                                
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
CATDETX  B     XIT                                                              
         DROP  R2,R6                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO PRINT THE CATEGORY FOOTER                                      
*                                                                               
CATFOOT  NTR1  ,                                                                
         GOTO1 JOBTXT,DMCB,C'F ',(C'C',JBCOLD)                                  
         BNE   CATFOOTX                                                         
*                                                                               
CATFOOT2 ZIC   R1,LINE                                                          
         LA    R1,2(R1)            TEST FOR FIT ON PAGE                         
         CLI   QBOXES,C'C'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   BYTE,C'N'           SET PAGE BREAK SWITCH                        
         CLM   R1,1,MAXLINES                                                    
         BL    CATFOOT4                                                         
         BAS   RE,BREAK                                                         
         MVI   BYTE,C'Y'           NOTE PAGE BREAK                              
*                                                                               
CATFOOT4 CLI   QBOXES,C'C'                                                      
         BE    CATFOOT5                                                         
         MVI   BOXREQ,C'C'         CLOSE THE BOX BEFORE COMMENT                 
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
*                                                                               
CATFOOT5 CLI   BYTE,C'N'           TEST FOR NO PAGE BREAK                       
         BE    CATFOOT6                                                         
         GOTO1 JOBTXT,DMCB,C'F ',(C'C',JBCOLD)                                  
*                                                                               
CATFOOT6 BAS   RE,SKIPLIN                                                       
         GOTO1 PRT,DESCDISP                                                     
         CLI   QBOXES,C'C'                                                      
         BE    *+8                                                              
         BAS   RE,SKIPLIN                                                       
*                                                                               
CATFOOTX B     XIT                                                              
         EJECT                                                                  
***************************************************************                 
* SUB-ROUTINE TO PRINT THE WORKCODE DETAIL--CALLED FROM PRINT *                 
* AT ENTRY, R5=A(COLUMN OUTPUT ENTRY)                         *                 
***************************************************************                 
         SPACE 1                                                                
WC       NTR1  ,                                                                
         USING JBCOLD,R5                                                        
         L     R3,ABOX                                                          
         USING BOXD,R3                                                          
         L     R2,AWIDE                                                         
         USING WIDED,R2                                                         
         GOTO1 JOBTXT,DMCB,C'H ',(C'W',JBCOLD)                                  
         BNE   WC10                NO TEXT TO PRINT                             
*                                                                               
* PRINT THE TEXT HEADER FOR THE WORKCODE                                        
*                                                                               
WC1      ZIC   R1,LINE                                                          
         ZIC   R0,TXACTNUM                                                      
         AR    R1,R0                                                            
         LA    R1,4(R1)            TEST THAT TEXT/DETAIL WILL FIT               
         CLI   QBOXES,C'C'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   BYTE,C'N'           SET PAGE BREAK SWITCH                        
         CLM   R1,1,MAXLINES                                                    
         BL    WC2                                                              
         BAS   RE,BREAK            HANDLE THE PAGE BREAK                        
         MVI   BYTE,C'Y'                                                        
*                                                                               
WC2      CLI   QBOXES,C'C'                                                      
         BE    WC4                                                              
         MVI   BOXREQ,C'O'         NEED TO OPEN A BOX                           
         CLC   BOXCOLS,SPACES      NEED TO OPEN A BOX IF BOXCOLS                
         BNE   *+14                AND BOXCOLSR ARE SPACES                      
         CLC   BOXCOLSR,SPACES                                                  
         BE    *+8                                                              
         MVI   BOXREQ,C'B'                                                      
*                                                                               
         LM    RE,RF,AOUTCOL1                                                   
         MVC   BOXCOLS,0(RE)                                                    
         MVC   BOXCOLSR,0(RF)                                                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
WC4      BAS   RE,SKIPLIN                                                       
         CLI   BYTE,C'Y'           TEST PAGE BREAK OCCURRED                     
         BNE   WC6                                                              
         GOTO1 JOBTXT,DMCB,C'H ',(C'W',JBCOLD)                                  
*                                                                               
WC6      GOTO1 PRT,DESCDISP        PRINT THE TEXT                               
*                                                                               
* PRINT THE WORKCODE DETAIL                                                     
*                                                                               
WC10     MVI   BYTE,C'N'                                                        
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)            TEST IF DETAIL WILL FIT                      
         CLI   QBOXES,C'C'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         CLM   R1,1,MAXLINES                                                    
         BL    WC12                YES                                          
         BAS   RE,BREAK            NO                                           
         MVI   BYTE,C'Y'                                                        
*                                                                               
WC12     CLI   QBOXES,C'C'                                                      
         BE    WC14                                                             
         MVI   BOXREQ,C'O'                                                      
         CLC   BOXCOLS,SPACES      TEST IF BOX MUST BE OPENED                   
         BNE   *+14                                                             
         CLC   BOXCOLSR,SPACES                                                  
         BE    *+8                                                              
         MVI   BOXREQ,C'B'                                                      
*                                                                               
         LM    RE,RF,AWRKCOL1                                                   
         MVC   BOXCOLS,0(RE)       WORKCODE COLUMNS                             
         MVC   BOXCOLSR,0(RF)                                                   
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
WC14     GOTO1 AWN,DMCB,(RC)                                                    
         MVC   XP+WCDISP(L'ROWCODE),ROWCODE                                     
         MVC   XP+DESCDISP(L'ROWNAME),ROWNAME                                   
         GOTO1 ADISVAL,DMCB,(RC)                                                
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
* PRINT THE TEXT FOOTER                                                         
*                                                                               
WC20     GOTO1 JOBTXT,DMCB,C'F ',(C'W',JBCOLD)                                  
         BNE   WCX                 NO FOOTER TEXT TO PRINT                      
*                                                                               
         MVI   BYTE,C'N'           SET PAGE BREAK SWITCH                        
         ZIC   R1,LINE                                                          
         ZIC   R0,TXACTNUM                                                      
         AR    R1,R0                                                            
         LA    R1,4(R1)            TEST THAT TEXT/DETAIL WILL FIT               
         CLI   QBOXES,C'C'                                                      
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         CLM   R1,1,MAXLINES       TEST FOR PAGE BREAK                          
         BL    WC22                                                             
         BAS   RE,BREAK                                                         
         MVI   BYTE,C'Y'                                                        
*                                                                               
WC22     CLI   QBOXES,C'C'         TEST FOR COMPRESSION                         
         BE    WC23                                                             
         LM    RE,RF,AOUTCOL1      REVERT TO OUTLINE BOX                        
         MVC   BOXCOLS,0(RE)                                                    
         MVC   BOXCOLSR,0(RF)                                                   
         MVI   BOXREQ,C'B'         CLOSE OFF THE DETAIL                         
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
WC23     CLI   BYTE,C'Y'           TEST FOR PAGE BREAK                          
         BNE   WC24                NO                                           
         GOTO1 JOBTXT,DMCB,C'F ',(C'W',JBCOLD)                                  
*                                                                               
WC24     GOTO1 PRT,DESCDISP                                                     
         BAS   RE,SKIPLIN          SKIP A LINE AFTER COMMENT                    
*                                                                               
WCX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO PRINT THE TEXT                                                 
* PAGE BREAKS MUST BE HANDLED BEFORE CALLING THIS ROUTINE                       
*                                                                               
* AT ENTRY, R1=DISPLACEMENT INTO PRINT LINE                                     
*                                                                               
PRT      NTR1  ,                                                                
         LR    R3,R1               SAVE DISPLACEMENT IN R3                      
         L     R2,AWIDE                                                         
         USING WIDED,R2                                                         
         ZIC   R6,TXACTNUM         R6=COUNTER                                   
         L     R5,TXAPRT           R5=A(TEXT PRINT LINES)                       
         L     R4,TXLPRT           R4=L'TEXT PRINT LINE                         
         BCTR  R4,0                                                             
*                                                                               
PRT2     LA    RE,XP(R3)                                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R5)                                                    
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
*                                                                               
PRT4     LA    R5,1(R4,R5)                                                      
         BCT   R6,PRT2                                                          
*                                                                               
PRTX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO LOOK FOR JOB TEXT--CALLED FROM PRINT--         *               
* AT ENTRY, P1=CL2'WHERE VALUE', P2=0 FOR JOB LEVEL OR          *               
*              P2 BYTE 0 = C'C' OR C'W' CATEGORY OR WORKCODE    *               
*                 BYTES 1-3 = A(COLUMN OUTPUT TABLE ENTRY)      *               
*              P3 (RC)                                                          
* ON EXIT, CC=EQ IF TEXT TO PRINT, CC=NEQ IF NO TEXT            *               
*****************************************************************               
         SPACE 1                                                                
JOBTXT   NTR1                                                                   
         MVI   TXACTNUM,0          CLEAR NUMBER OF TEXT LINES                   
         MVC   HALF,0(R1)          SAVE WHERE VALUE                             
         MVC   BYTE,4(R1)          SAVE CATEGORY/WORKCODE INDICATOR             
         SR    R5,R5                                                            
         ICM   R5,7,5(R1)                                                       
         CLI   HALF,C'T'           TEST FOR TITLE                               
         BE    JOBTXT1             YES                                          
         CLC   HALF,=C'ST'         TEST FOR SUB-TITLE                           
         BE    JOBTXT1             YES                                          
         CLI   QTEXT,C'N'          TEST TO PRINT TEXT                           
         BE    NOTOKXIT            NO-EXIT RIGHT NOW                            
*                                                                               
JOBTXT1  MVC   TXACOM,ACOMFACS                                                  
         MVC   TXAPRT,APRTTAB                                                   
         ZIC   R1,REPWIDTH                                                      
         ST    R1,TXLPRT                                                        
         CLI   HALF,C'T'           TEST FOR TITLE CALL                          
         BNE   JOBTXT2             NO                                           
         L     R2,AWIDE                                                         
         USING WIDED,R2                                                         
         LA    R1,XHEAD1           POINT AT HEADLINES                           
         ST    R1,TXAPRT                                                        
         LA    R1,L'XHEAD1                                                      
         ST    R1,TXLPRT                                                        
*                                                                               
JOBTXT2  MVC   TXAMAST,TWAMASTC                                                 
         MVC   TXACOMP,ACOMP                                                    
         MVC   TXALEDG,ALEDG                                                    
         MVC   TXAMED,AMEDBUFF     MEDIA BUFFER                                 
         MVC   TXACLI,ACLI                                                      
         MVC   TXAPRO,APROD                                                     
         MVC   TXAJOB,AJOB                                                      
         MVC   TXAPAN,APANEL                                                    
         XC    TXAHOOK,TXAHOOK     CLEAR HOOK ADDRESS                           
         LA    R1,JBLOCK                                                        
         ST    R1,TXAJBLK                                                       
         LA    R1,USERNAME                                                      
         ST    R1,TXAORIG                                                       
         MVC   TXPAGE,PAGE                                                      
*                                                                               
         XC    TXSELECT,TXSELECT   CLEAR SELECT FIELDS                          
         MVI   TXSELFUN,TXGETTXT   GET TEXT                                     
         MVC   TXSELCUL,CUL                                                     
         MVI   TXSELFRM,C'E'       FORM=ESTIMATE                                
         MVC   TXSELWHR,HALF                                                    
         MVC   TXSELCAP,QCAPS      CAPITALS OPTION                              
*                                                                               
JOBTXT3  MVC   TXSELCLI,CLICODE    FIRST LOOK AT JOB LEVEL                      
         MVC   TXSELPRO,PRODCODE                                                
         MVC   TXSELJOB,JOBNUM                                                  
         LTR   R5,R5               TEST CATEGORY/WORKCODE LEVEL                 
         BP    JOBTXT20            YES                                          
*                                                                               
JOBTXT10 MVC   TXSELOFC,EFFOFFC    SET OFFICE/OFF GRP FOR HEADER/FOOTER         
         MVC   TXSELOG,EFFOFG                                                   
         MVI   TXSELFUN,TXGETFT                                                 
         CLC   TXSELWHR,=C'SF'     TEST FOR SUB-FOOTER                          
         BE    JOBTXT12                                                         
         CLI   TXSELWHR,C'F'       TEST FOR FOOTER                              
         BE    JOBTXT12                                                         
         MVI   TXSELFUN,TXGETHD    GET HEADER LOGIC FOR TITLE/SUB-TITLE         
*                                                                               
JOBTXT12 LA    R1,TXTHOOK          PASS TEXT HOOK                               
         CLI   TXSELWHR,C'T'       TEST FOR TITLE                               
         BNE   *+6                                                              
         SR    R1,R1               YES-NO HOOK NEEDED                           
         ST    R1,TXAHOOK                                                       
         BAS   RE,CALLTXT                                                       
         BNE   OKXIT                                                            
         B     NOTOKXIT                                                         
*                                                                               
         USING JBCOLD,R5                                                        
JOBTXT20 CLI   BYTE,C'C'           TEST FOR CATEGORY                            
         BE    JOBTXT22            YES                                          
*                                                                               
         MVC   TXSELWC,JBCOLWC                                                  
         MVC   TXSELSUF,JBCOLSUF                                                
         B     JOBTXT24                                                         
*                                                                               
JOBTXT22 MVC   TXSELCAT,JBCOLCAT                                                
*                                                                               
JOBTXT24 BAS   RE,CALLTXT                                                       
         BNE   OKXIT                                                            
         B     NOTOKXIT                                                         
*                                                                               
* SUB-ROUTINE TO CALL GETTXT                                                    
*                                                                               
CALLTXT  ST    RE,SAVERE                                                        
         GOTO1 VGETTXT,DMCB,TXBLOCK                                             
         L     RE,SAVERE                                                        
         CLI   TXACTNUM,0          TEST FOR TEXT FOUND                          
         BR    RE                                                               
*                                                                               
* HOOK ROUTINE FOR HEADER AND FOOTER TEXT                                       
*                                                                               
TXTHOOK  NTR1  ,                                                                
         ZIC   R1,LINE                                                          
         ZIC   RE,TXACTNUM                                                      
         LA    R1,1(RE,R1)                                                      
         CLM   R1,1,MAXLINES       TEST FOR FIT ON PAGE                         
         BL    *+8                                                              
         BAS   RE,BREAK                                                         
*                                                                               
TXTHOOK2 LA    R1,WCDISP           DISPLACEMENT INTO PRINT LINE                 
         OC    TXACTPAN,TXACTPAN   TEST FOR PANEL                               
         BZ    TXTHOOK4            NO                                           
         CLC   TXACTPAN,DEFPAN     TEST FOR DEFAULT PANEL                       
         BE    TXTHOOK4            YES                                          
         SR    R1,R1               CLEAR R1 TO LET PANEL CONTROL                
*                                                                               
TXTHOOK4 GOTO1 PRT,(R1)                                                         
         BAS   RE,SKIPLIN                                                       
*                                                                               
TXTHOOKX B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
OKXIT    CR    RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
NOTOKXIT LTR   RB,RB                                                            
         B     XIT                                                              
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
INVEND   MVI   ERROR,INVALID       INVALID EXIT                                 
         B     ERREND                                                           
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         EJECT                                                                  
RELO     DS    A                                                                
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
SLUSHCAT DC    C'&&&&'                                                          
DEFPAN   DC    C'9999'                                                          
COMRATE  DC    Y(JBDRATE)                                                       
BUFFLEN  DC    F'512000'                                                        
MEDBUFFL DC    AL4(30000)                                                       
WRKBUFFL DC    AL4(36*36*((ACANLENQ+2)+(L'ACNMNAME+2))+4)                       
COLTABL  DC    F'20000'                                                         
OPVTABL  DC    F'24000'                                                         
PRTTABL  DC    AL4(20*198)                                                      
SCHTABL  DC    F'40000'                                                         
PANELTL  DC    F'25000'                                                         
OPTBUFFL DC    F'200000'                                                        
SUPPNAME DC    CL11'SUPPLEMENT'                                                 
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
* ADDITIONAL STORAGE AREAS                                                      
*                                                                               
ASAVCOL1 DC    A(SAVCOLS)                                                       
ASAVCOL2 DC    A(SAVCOLSR)                                                      
AOUTCOL1 DC    A(OUTCOLS)                                                       
AOUTCOL2 DC    A(OUTCOLSR)                                                      
AWRKCOL1 DC    A(WRKCOLS)                                                       
AWRKCOL2 DC    A(WRKCOLSR)                                                      
*                                                                               
WRKCOLS  DC    CL132' '                                                         
WRKCOLSR DC    CL132' '                                                         
OUTCOLS  DC    CL132' '                                                         
OUTCOLSR DC    CL132' '                                                         
SAVCOLS  DS    CL(L'BOXCOLS)                                                    
SAVCOLSR DS    CL(L'BOXCOLSR)                                                   
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO BUILD A PANEL BUFFER FOR THE DEFAULT TITLE    *                
****************************************************************                
         SPACE 1                                                                
BLDT     NMOD1 0,**BLDT**                                                       
         L     RC,0(R1)            RE-ESTABLISH ADDRESSABILITY                  
         L     R4,TXAPAN                                                        
         LA    R3,TITAB                                                         
         USING TITABD,R3                                                        
         LA    R6,ELEM                                                          
         USING ACFDD,R6                                                         
*                                                                               
BLDT2    SR    R1,R1                                                            
         ICM   R1,1,TIOPT                                                       
         BZ    BLDT3                                                            
         BCTR  R1,0                                                             
         LA    R1,PROGPROF(R1)                                                  
         CLI   0(R1),C'Y'          SUPPRESS THIS FIELD ?                        
         BE    BLDT6               YES                                          
*                                                                               
BLDT3    XC    ELEM,ELEM                                                        
         MVI   ACFDEL,ACFDELQ                                                   
         MVC   ACFDPLIN,TIPLIN                                                  
         MVC   ACFDPCOL,TIPCOL                                                  
         MVC   ACFDPDTY,TIPDTY                                                  
         MVC   ACFDPDLN,TIPDLN                                                  
         ZIC   R1,ACFDPDLN         GET PRINT DATA LENGTH                        
         LR    RE,R1               SAVE LENGTH IN RE                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACFDPRT(0),TIDATA                                                
         LA    R1,ACFDPRT-ACFDD+1(R1) COMPUTE EL LEN                            
         STC   R1,ACFDLEN                                                       
*                                                                               
BLDT4    EX    R1,*+8                                                           
         B     *+10                MOVE EL + 1 BYTE OF ZERO                     
         MVC   0(0,R4),ACFDD                                                    
         LA    R4,0(R1,R4)         NEXT POSITION                                
*                                                                               
BLDT6    LA    R3,TIDATA-TITABD(RE,R3) NEXT TABLE ENTRY                         
         CLI   0(R3),X'FF'         TEST FOR EOT                                 
         BNE   BLDT2               NO                                           
*                                                                               
BLDTX    XMOD1 1                                                                
         DROP  R3,R6                                                            
         SPACE 2                                                                
* TITLE TABLE                                                                   
*                                                                               
TITAB    DS    0C                                                               
         DC    AL1(1),AL1(2),C'I',AL1(10),AL1(0)                                
         DC    X'02',AL1(10),CL8'TD'                                            
         DC    AL1(1),AL1(42),C'I',AL1(10),AL1(0)                               
         DC    X'02',AL1(10),CL8'PE'                                            
         DC    AL1(1),AL1(98),C'I',AL1(10),AL1(0)                               
         DC    X'02',AL1(10),CL8'PG'                                            
*                                                                               
         DC    AL1(3),AL1(42),C'I',AL1(10),AL1(1)                               
         DC    X'02',AL1(10),CL8'AN'                                            
         DC    AL1(4),AL1(42),C'I',AL1(10),AL1(2)                               
         DC    X'02',AL1(10),CL8'AA'                                            
*                                                                               
         DC    AL1(5),AL1(02),C'C',AL1(6),AL1(0),CL6'CLIENT'                    
         DC    AL1(5),AL1(10),C'I',AL1(10),AL1(0)                               
         DC    X'02',AL1(10),CL8'CC'                                            
         DC    AL1(5),AL1(18),C'I',AL1(10),AL1(0)                               
         DC    X'02',AL1(10),CL8'CN'                                            
*                                                                               
         DC    AL1(6),AL1(02),C'C',AL1(7),AL1(0),CL7'PRODUCT'                   
         DC    AL1(6),AL1(10),C'I',AL1(10),AL1(0)                               
         DC    X'02',AL1(10),CL8'PC'                                            
         DC    AL1(6),AL1(18),C'I',AL1(10),AL1(0)                               
         DC    X'02',AL1(10),CL8'PN'                                            
*                                                                               
         DC    AL1(7),AL1(02),C'C',AL1(3),AL1(0),CL3'JOB'                       
         DC    AL1(7),AL1(10),C'I',AL1(10),AL1(0)                               
         DC    X'02',AL1(10),CL8'JC'                                            
         DC    AL1(7),AL1(18),C'I',AL1(10),AL1(0)                               
         DC    X'02',AL1(10),CL8'JN'                                            
*                                                                               
         DC    AL1(8),AL1(02),C'I',AL1(10),AL1(0)                               
         DC    X'02',AL1(10),CL8'PB'                                            
*                                                                               
         DC    AL1(05),AL1(78),C'C',AL1(7),AL1(0),CL7'BILLING'                  
         DC    AL1(06),AL1(78),C'C',AL1(7),AL1(0),CL7'ADDRESS'                  
         DC    AL1(05),AL1(87),C'I',AL1(10),AL1(0)                              
         DC    X'02',AL1(10),CL8'BA'                                            
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO FILTER ON MEDIA GROUP                                          
*************************************************************                   
*                                                                               
FILMG    NMOD1 0,*FLTMG*                                                        
         L     RC,0(R1)                                                         
         CLI   QMG,0                                                            
         BE    FILMGY                                                           
         L     R1,AMEDBUFF                                                      
         SR    R0,R0                                                            
*                                                                               
FILMG2   CLI   0(R1),0             TEST FOR EOB                                 
         BE    FILMGN              NO                                           
         USING ACMEDIAD,R1                                                      
         CLC   ACMDCODE,JOBNUM                                                  
         BNE   FILMG4                                                           
         CLC   QMG,ACMDGRP         TEST WILL SET CC                             
         BE    FILMGY                                                           
         B     FILMGN                                                           
*                                                                               
FILMG4   ZIC   R0,ACMDLEN                                                       
         AR    R1,R0                                                            
         B     FILMG2                                                           
*                                                                               
FILMGN   LTR   RB,RB               EXIT WITH CC=NEQ                             
         B     FILMGX                                                           
*                                                                               
FILMGY   CR    RB,RB               EXIT WITH CC=EQ                              
*                                                                               
FILMGX   XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO LOOK UP THE WORKCODE CODE AND NAME         *                   
* ON ENTRY, R5=A(COLUMN OUTPUT TABLE)                       *                   
* ON EXIT, ROWCODE AND ROWNAME ARE SET                      *                   
*************************************************************                   
         SPACE 1                                                                
         USING JBCOLD,R5                                                        
WN       NMOD1 0,**WN**                                                         
         L     RC,0(R1)                                                         
         MVC   ROWNAME,SPACES                                                   
         MVC   ROWCODE,SPACES                                                   
         CLI   JBCOLTYP,JBCOLTWC   TEST FOR WORKCODE LINE                       
         BNE   WN20                NO                                           
*                                                                               
         CLI   JBCOLSUF,0          TEST FOR SUFFIX                              
         BNE   WN2                 YES                                          
*                                                                               
         MVI   GOANYWC,C'Y'                                                     
         MVC   GOSELWC,JBCOLWC     READ FOR WORKCODE OPTIONS                    
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         XC    GOSELWC,GOSELWC                                                  
*                                                                               
         OC    GOCLIWC,GOCLIWC     TEST FOR CLIENT OVERRIDE CODE                
         BZ    *+10                                                             
         MVC   ROWCODE(L'GOCLIWC),GOCLIWC                                       
         OC    GOOVERWK,GOOVERWK                                                
         BZ    *+10                                                             
         MVC   ROWNAME(L'GOOVERWK),GOOVERWK                                     
*                                                                               
WN2      CLC   ROWCODE,SPACES      TEST ANY WORKCODE FOUND                      
         BNE   *+10                YES                                          
         MVC   ROWCODE,JBCOLWC                                                  
         CLC   ROWNAME,SPACES      TEST IF NAME FOUND                           
         BNE   WNX                 YES-ALL DONE                                 
         L     R6,ACATEL                                                        
         USING ACCWD,R6                                                         
         SR    R0,R0                                                            
*                                                                               
WN4      CLI   ACCWEL,0            TEST FOR EOR                                 
         BE    WN8                 YES                                          
         CLI   ACCWEL,ACCWELQ                                                   
         BNE   WN5                                                              
         CLI   ACCWTYPE,1          TEST FOR WORKCODE ITEM                       
         BNE   WN5                                                              
         CLC   JBCOLWC(3),ACCWWORK                                              
         BE    WN6                                                              
*                                                                               
WN5      IC    R0,ACCWLEN                                                       
         AR    R6,R0                                                            
         B     WN4                                                              
*                                                                               
WN6      CLI   ACCWLEN,ACCWLNQ1    TEST FOR SHORT LENGTH                        
         BE    WN8                 YES                                          
         MVC   ROWNAME(L'ACCWDESC),ACCWDESC EXTRACT NAME OVERRIDE               
*                                                                               
WN8      L     R2,AWRKBUFF                                                      
         SR    R1,R1                                                            
*                                                                               
WN9      CLI   0(R2),0                                                          
         BE    WNX                                                              
         LA    R6,2(R2)                                                         
         USING ACANALD,R6                                                       
         CLC   ACANCODE,JBCOLWC    TEST FOR RIGHT WORKCODE                      
         BE    WN10                                                             
         IC    R1,ACANLEN                                                       
         LA    R2,0(R1,R6)         NEXT POSITION                                
*                                                                               
         CLI   0(R2),ACNMELQ       NAME ELEMENT?                                
         BNE   WN9                 NO                                           
         IC    R1,1(R2)            YES, SKIP OVER IT                            
         LA    R2,0(R1,R2)                                                      
         B     WN9                                                              
*                                                                               
WN10     CLC   ROWNAME,SPACES      DO WE HAVE AN OVERRIDE NAME?                 
         BNE   *+10                YES, SKIP REGULAR AND LOOK FOR LONG          
         MVC   ROWNAME(L'ACANDESC),ACANDESC                                     
*                                                                               
         CLI   PROGPROF+2,C'Y'     USE LONG NAMES?                              
         BNE   WNX                 NO                                           
         IC    R1,ACANLEN          YES, GET NEXT POSITION                       
         LA    R6,0(R1,R6)                                                      
*                                                                               
         USING ACNAMED,R6                                                       
         CLI   0(R6),ACNMELQ       NAME ELEMENT?                                
         BNE   WNX                 NO                                           
         SR    R1,R1                                                            
         IC    R1,ACNMLEN                                                       
         AHI   R1,-3               SUBTRACT OVERHEAD+1 FOR MOVE                 
         BM    WNX                 IF NOTHING THEN EXIT                         
         MVC   ROWNAME,SPACES      GET READY FOR LONG NAME                      
         EX    R1,*+8                                                           
         B     WNX                                                              
         MVC   ROWNAME(0),ACNMNAME                                              
*                                                                               
* SUB-TOTAL, AGENCY COMMISSION LINES                                            
*                                                                               
WN20     MVC   BYTE,COMNUM                                                      
         CLI   JBCOLTYP,JBCOLTAG   TEST FOR AGENCY COMMISSION NUM               
         BE    *+10                                                             
         MVC   BYTE,SUBNUM                                                      
         ZIC   R2,BYTE                                                          
         SR    R0,R0                                                            
         L     R6,ACATEL                                                        
         USING ACCWD,R6                                                         
*                                                                               
WN22     CLI   ACCWEL,0            TEST FOR EOR                                 
         BE    WN30                YES                                          
         CLI   ACCWEL,ACCWELQ                                                   
         BNE   WN23                                                             
         CLC   ACCWTYPE,JBCOLTYP   MATCH ON COLUMN TYPE                         
         BNE   WN23                                                             
         BCT   R2,WN23                                                          
         B     WN24                FOUND RIGHT ELEMENT                          
*                                                                               
WN23     IC    R0,ACCWLEN                                                       
         AR    R6,R0                                                            
         B     WN22                                                             
*                                                                               
WN24     CLI   ACCWLEN,ACCWLNQ1    TEST FOR SHORT LENGTH                        
         BE    WN25                                                             
         MVC   ROWNAME(L'ACCWDESC),ACCWDESC                                     
         B     WN30                                                             
*                                                                               
WN25     MVC   ROWNAME(11),=C'*SUB-TOTAL*'                                      
         CLI   JBCOLTYP,JBCOLTSB   TEST FOR SUB-TOTAL                           
         BE    *+10                                                             
         MVC   ROWNAME(17),=C'AGENCY COMMISSION'                                
*                                                                               
WN30     ZIC   R1,BYTE                                                          
         LA    R1,1(R1)            INCREMENT COUNTER                            
         LA    RF,COMNUM                                                        
         CLI   JBCOLTYP,JBCOLTSB                                                
         BE    *+8                                                              
         LA    RF,SUBNUM                                                        
         STC   R1,0(RF)                                                         
*                                                                               
WNX      CLI   QCAPS,C'Y'          TEST FORCE TO ALL CAPS                       
         BNE   *+16                                                             
         OC    ROWCODE,SPACES                                                   
         OC    ROWNAME,SPACES                                                   
         XMOD1 1                                                                
         DROP  R5,R6                                                            
         SPACE 1                                                                
* LITERAL POOL FOR WN                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
************************************************************                    
* SUB-ROUTINE TO GET THE CATEGORY NAME--CALLED FROM PRINT  *                    
* ON OUTPUT, ROWNAME CONTAINS NAME AND ACATEL=A(CATEGORY   *                    
*   DESCRIPTION ELEMENT)                                   *                    
************************************************************                    
         SPACE 1                                                                
         USING JBCOLD,R5                                                        
CATNAME  NMOD1 0,*CATNAM*                                                       
         L     RC,0(R1)                                                         
         MVC   HALF,JBCOLCAT       EXTRACT CATEGORY                             
         CLC   HALF,SLUSHCAT       TEST FOR SLUSH                               
         BNE   *+10                NO                                           
         MVC   HALF,=X'FFFF'       YES-CONVERT TO INTERNAL KEY                  
         L     R4,SCAFSTCT                                                      
         USING ACCTKEY,R4                                                       
*                                                                               
CATNAME2 CLI   0(R4),0             TEST FOR EOB                                 
         BNE   *+6                                                              
         DC    H'0'                THIS IS A PROBLEM                            
         CLC   ACCTCODE,HALF       MATCH ON CATEGORY                            
         BE    CATNAME4                                                         
         SR    R1,R1                                                            
         ICM   R1,3,ACLENGTH                                                    
         LA    R4,1(R1,R4)         NEXT RECORD IN BUFFER                        
         B     CATNAME2                                                         
*                                                                               
CATNAME4 MVI   ELCODE,ACCDELQ      FIND CATEGORY DESCRIPTION ELEM               
         L     R2,AIO              SAVE IO AREA POINTER                         
         ST    R4,AIO              RESET IT TO POINT TO CATEGORY                
         ST    R4,ACAT                                                          
         BAS   RE,GETELIO                                                       
         USING ACCDD,R6                                                         
         TM    ACCDSTAT,ACCDCONT   TEST FOR CONTINUATION CATEGORY               
         BO    CATNAME6            YES-SKIP NAME AND CODE EXTRACT               
*                                                                               
         MVC   THISCAT,ACCTCODE                                                 
         MVC   ROWNAME,SPACES                                                   
         MVC   ROWNAME(L'ACCDNAME),ACCDNAME    EXTRACT NAME                     
         CLI   QCAPS,C'Y'                                                       
         BNE   *+10                                                             
         OC    ROWNAME,SPACES      SPACE PAD TO FORCE CAPITALS                  
*                                                                               
CATNAME6 ST    R6,ACATEL                                                        
         ST    R2,AIO              RESTORE IO AREA POINTER                      
*                                                                               
CATNAMEX XMOD1                                                                  
         DROP R5,R6                                                             
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**************************************************************                  
* SUB-ROUTINE TO DISPLAY THE ROW VALUES ON THE PRINT LINE    *                  
* AT ENTRY, R5=A(JBCOLD)                                     *                  
**************************************************************                  
         SPACE 1                                                                
DISVAL   NMOD1 0,**DISV**                                                       
         L     RC,0(R1)                                                         
         USING JBCOLD,R5                                                        
         L     R2,AWIDE                                                         
         USING WIDED,R2                                                         
         XC    EBLOCK,EBLOCK                                                    
         MVI   EBLIN,L'JBCOLVAL    INPUT LENGTH                                 
         MVI   EBTIN,C'P'          PACKED VALUES                                
         MVI   EBLOUT,COLLEN                                                    
*                                                                               
         MVI   BYTE,EBOQBMF        DEFAULT IS BRACKETS FOR NEG. NUMBERS         
         CLI   QMINUS,C'Y'         TEST FOR MINUS=YES                           
         BNE   *+8                                                              
         MVI   BYTE,EBOQMEY                                                     
         CLI   QZEROES,C'B'        TEST ZERO=BLANK                              
         BE    *+8                 YES                                          
         OI    BYTE,EBOQZEN        SET ZERO=NOBLANK                             
         CLI   QCOMMAS,C'Y'        PRINT COMMAS IN NUMBERS ?                    
         BNE   *+8                 NO                                           
         OI    BYTE,EBOQCEY        YES                                          
         MVC   EBOPT,BYTE                                                       
*                                                                               
DISVAL2  ZIC   R0,NCOLS            R0=LOOP COUNTER                              
         LA    R6,XP+1             R6=PRINT POSITION                            
         AH    R6,WCRDISP          ADD IN DISP TO END OF WC COL                 
         LA    R3,JBCOLVAL         R3=A(INPUT VALUES)                           
         LA    R4,COLLIST          R4=A(COLUMN LIST)                            
         USING JBCLD,R4                                                         
*                                                                               
DISVAL3  MVI   EBDECS,2            SET FOR TWO DECIMAL PLACES                   
         MVI   EBROUND,0           CLEAR ROUND OPTION                           
         OC    QROUND,QROUND       ANY ROUNDING ?                               
         BZ    DISVAL4             NO                                           
         CLI   QCROUND,C'N'        ARE WE ROUNDING COMMISSION ?                 
         BNE   DISVAL4             YES, CONTINUE AS USUAL                       
         CLC   JBCLCN1,=Y(JBDESTC) NO, IS THIS ONE                              
         BNL   DISVAL5             YES, SKIP OVER EVERYTHING                    
         SRP   0(L'JBCOLVAL,R3),64-2,5                                          
         SRP   0(L'JBCOLVAL,R3),2,0                                             
*                                                                               
DISVAL4  CLI   QROUND,C'P'         TEST ROUND/PRINT PENNIES                     
         BE    DISVAL5             YES                                          
         CLI   QROUND,C'Y'         TEST ROUND/PRINT DOLLARS                     
         BNE   DISVAL5             NO                                           
         TM    JBCLIND,JBCLIPER    TEST PERCENTAGE COLUMN                       
         BO    DISVAL5             YES                                          
         CLI   JBCLTYP,JBCLCOL     TEST FOR SINGLE COLUMN                       
         BNE   *+14                NO                                           
         CLC   JBCLCN1,=Y(JBDRATE)                                              
         BE    DISVAL5                                                          
         MVI   EBDECS,0            NO-SET NO DECIMALS                           
         MVI   EBROUND,2           GET RID OF THE PENNIES                       
*                                                                               
DISVAL5  CLC   JBCLCN1,=Y(JBDRATE) TEST FOR COMMISSION RATE                     
         BNE   *+16                                                             
         MVI   EBDECS,4            YES-PRINT TO FOUR DECIMAL PLACES             
         C     R5,ACOLTAB          TEST FOR FIRST ENTRY                         
         BE    DISVAL8             YES-DO NOT PRINT RATE IN TOTAL LINE          
*                                                                               
         CP    0(L'JBCOLVAL,R3),=P'0' TEST FOR ZERO                             
         BNE   DISVAL6                                                          
         CLI   QZEROES,C'Z'        TEST FOR 0='ZERO'                            
         BNE   DISVAL6             NO                                           
*                                                                               
         MVC   COLDISP-6(4,R6),=C'ZERO'                                         
         B     DISVAL8                                                          
*                                                                               
DISVAL6  ST    R3,EBAIN                                                         
         ST    R6,EBAOUT                                                        
         GOTO1 EDITOR,DMCB,EBLOCK                                               
*                                                                               
DISVAL8  LA    R3,L'JBCOLVAL(R3)                                                
         LA    R4,JBCLENQ(R4)                                                   
         LA    R6,COLDISP(R6)                                                   
         BCT   R0,DISVAL3                                                       
*                                                                               
DISVALX  XMOD1 1                                                                
         DROP  R2,R4                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
* SUB-ROUTINE TO LOOK UP THE CONTROL PROFILE                *                   
*************************************************************                   
         SPACE 1                                                                
         USING PROFKD,R5                                                        
GETPRO   NMOD1 0,*GETP*                                                         
         L     RC,0(R1)                                                         
         LA    R5,WORK                                                          
         XC    PROFKEY,PROFKEY                                                  
         XC    PROGPROF,PROGPROF                                                
         MVI   PROFKSYS,C'A'                                                    
         MVC   PROFKPGM,=C'0ES'                                                 
         MVC   PROFKAGY,AGYALPHA                                                
         MVC   PROFKUNL,=C'SJ'                                                  
         MVC   PROFKACC,CLICODE                                                 
         CLI   EFFOFFC,X'00'       IS THERE AN OFFICE ?                         
         BE    GETP04              NO                                           
         TM    COMPSTA4,X'01'      NEW OFFICES ?                                
         BO    GETP02                                                           
         MVC   PROFKOFF,EFFOFF                                                  
         MVI   PROFKAST,C'*'                                                    
         B     GETP04                                                           
*                                                                               
GETP02   MVI   PROFKAST,C'+'                                                    
         MVC   PROFKOFC,EFFOFFC                                                 
*                                                                               
GETP04   GOTO1 GETPROF,DMCB,PROFKEY,PROGPROF,DATAMGR                            
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
         EJECT                                                                  
       ++INCLUDE ACPROWORKD                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDMASTD                                                                        
*ACGENBOTH                                                                      
*DDBIGBOX                                                                       
*DDTWADCOND                                                                     
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE ACPRO39COM                                                     
         SPACE 2                                                                
* DSECT TO COVER THE TITLE TABLE                                                
*                                                                               
TITABD   DSECT                                                                  
TIPLIN   DS    X                   PRINT LINE                                   
TIPCOL   DS    C                   PRINT COLUMN                                 
TIPDTY   DS    X                   PRINT DATA TYPE (C,I)                        
TIPDLN   DS    X                   PRINT DATA LENGTH                            
TIOPT    DS    C                   RELATED CONTROL PROFILE OPTION #             
TIDATA   DS    0C                  PRINT DATA                                   
         SPACE 2                                                                
* DSECT TO COVER THE GETPROF CALL                                               
*                                                                               
PROFKD   DSECT                                                                  
PROFKEY  DS    0CL16                                                            
PROFKSYS DS    C                                                                
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    C                                                                
PROFKOFF DS    C                                                                
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                                                              
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
LCOLDISP EQU   0                   LEFT HAND MARGIN                             
WCDISP   EQU   2                                                                
DESCDISP EQU   6                   DESCRIPTION                                  
WCRDISPQ EQU   DESCDISP+35+1       WORKCODE DESCRIPTION BORDER                  
COLLEN   EQU   14                  LENGTH OF NUMERIC COLUMN                     
COLDISP  EQU   COLLEN+1            DISPLACEMENT FOR COLUMN                      
MINCOLS  EQU   4                                                                
MINCDISQ EQU   WCRDISPQ+(COLDISP*MINCOLS) MINIMUM RIGHT HAND MARGIN             
         EJECT                                                                  
* DSECT TO COVER WIDE BLOCK                                                     
*                                                                               
       ++INCLUDE DDWIDED                                                        
         EJECT                                                                  
* DSECT TO COVER DOWNLOAD BLOCK                                                 
*                                                                               
       ++INCLUDE DDDLCB                                                         
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACPRO49   09/12/02'                                      
         END                                                                    
