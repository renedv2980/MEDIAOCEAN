*          DATA SET ACREPSB02  AT LEVEL 002 AS OF 04/09/19                      
*PHASE ACSB02A                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE DATVAL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE PUBEDIT                                                                
*INCLUDE PQPROF                                                                 
                                                                                
*====================================================================           
* PROCESSING STRATEGY                                                           
* FIND A HEADER RECORD ON INPUT FILE FOR REQUESTING AGENCY                      
* VALIDATE THE HEADER                                                           
* READ AND BUFFER ALL DETAIL RECORDS                                            
* IF ERRORS IN THE HEADER, DON'T VALIDATE DETAILS                               
* WHEN NEXT HEADER IS READ, IF ERRORS, JUST CREATE OUTPUT FILE                  
* IF NO ERRORS, UPDATE TRANSACTIONS ON ACCFILE AND CREATE OUTPUT FILE           
*====================================================================           
         TITLE 'ACREPSB02 - PROCESS CASH FROM SAP INPUT FILE'                   
         PRINT NOGEN                                                            
ACSB02   CSECT                                                                  
         NMOD1 0,*ACSB02*                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LARL  RC,ACSBWRK                                                       
         USING ACSBWRK,RC                                                       
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         JE    RNF00                                                            
         CLI   MODE,REQFRST                                                     
         JE    RQF00                                                            
         CLI   MODE,REQLAST                                                     
         JE    RQL00                                                            
         CLI   MODE,RUNLAST                                                     
         JE    RNL00                                                            
         J     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                       *          
***********************************************************************         
                                                                                
RQL00    DS    0H                                                               
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                           *          
***********************************************************************         
                                                                                
RNL00    DS    0H                                                               
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* RUNFIRST                                                           *          
***********************************************************************         
                                                                                
RNF00    DS    0H                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
*                                                                               
         CLI   RCWRITE,C'Y'        TEST WRITE=NO                                
         JE    *+8                                                              
         OI    RNSW,WRTNO                                                       
*                                                                               
*        CLI   MCTSTRUN,X'FF'      TEST RUN=TEST                                
*        JNE   *+8                                                              
*        OI    RNSW,RUNTST                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),WORK                                           
         PACK  DUB(4),WORK(7)                                                   
         MVC   TODAY3,DUB          MOVE 3-BYTE PWOS DATE                        
*                                                                               
         L     RF,=A(DCLIST)                                                    
         GOTO1 ADDICTAT,DMCB,C'L   ',(RF),DSLIST                                
         J     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
                                                                                
RQF00    DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(X'20',TODAY6)                            
         MVC   SVMOS(1),TODAY6+1   BUILD MOS                                    
         MVC   SVMOS+1(1),TODAY6+3                                              
         CLC   TODAY6+2(2),=C'10'                                               
         BNE   *+8                                                              
         MVI   SVMOS+1,C'A'                                                     
         CLC   TODAY6+2(2),=C'11'                                               
         BNE   *+8                                                              
         MVI   SVMOS+1,C'B'                                                     
         CLC   TODAY6+2(2),=C'12'                                               
         BNE   *+8                                                              
         MVI   SVMOS+1,C'C'                                                     
*                                                                               
         OPEN  (SAPIN,(INPUT))                                                  
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         OPEN  (SAPOUT,(OUTPUT))                                                
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         L     RE,=A(DTLBUFF)                                                   
         ST    RE,NEXTDTL                                                       
         XC    0(8,RE),0(RE)       CLEAR FIRST ENTRY                            
*                                                                               
         MVI   ISTPEMPT,C'Y'       INITIALIZE IS TAPE EMPTY                     
         MVI   HDRSET,C'N'         OUTPUT HEADER NOT SET                        
*                                                                               
         LA    R0,INREC            GET AN INPUT RECORD                          
         GET   SAPIN,(0)                                                        
*                                                                               
         MVI   ISTPEMPT,C'N'       TAPE IS NOT EMPTY                            
*                                                                               
         CLC   =C'HDR',INREC+4                                                  
         JE    RQF12                                                            
         DC    H'0'                DIE IF FIRST RECORD IS NOT A HDR             
*                                                                               
RQF10    DS    0H                                                               
         LA    R0,INREC            GET AN INPUT RECORD                          
         GET   SAPIN,(0)                                                        
*                                                                               
RQF11    CLC   =C'HDR',INREC+4                                                  
         JNE   RQF30                                                            
         BRAS  RE,ENDHDR          FINISH PREV BLOCK                             
*                                                                               
*=========================================================                      
* SEE IF HEADER RECORD IS FOR REQUESTED AGENCY                                  
* IF NOT, SKIP TO NEXT HEADER                                                   
*=========================================================                      
                                                                                
RQF12    DS    0H                                                               
         MVI   SAPREC,C' '                                                      
         MVC   SAPREC+1(L'SAPREC-1),SAPREC                                      
         LH    RE,INREC                                                         
         SHI   RE,5                  4 BYTES LENGTH + 1 FOR EX MOVE             
         MVC   SAPREC(0),INREC+4     MOVE FROM INPUT AREA TO HDR AREA           
         EX    RE,*-6                                                           
*                                                                               
         XC    SAPHERR,SAPHERR    CLEAR ERROR                                   
         MVI   CTRY,CTRYCAN       CANADA                                        
         CLC   =C'MEDIAOCEAN_CA_',SAPMSYS                                       
         JE    RQF14                                                            
         MVI   CTRY,CTRYUS        USA                                           
         CLC   =C'MEDIAOCEAN_US_',SAPMSYS                                       
         JE    RQF14                                                            
         OI    SAPHERR,HERRIMID    SET INVALID MEDIAOCEAN ID                    
*                                                                               
RQF14    DS    0H                  MUST MATCH AGY IN REQUEST                    
         ZAP   CHKTOT,=P'0'                                                     
*        CLC   ALPHAID,14(R4)      MUST MATCH AGY IN REQUEST                    
*        JE    RQF20               SHOULD BE TB, SO, OR O0                      
         J     RQF20               MUST MATCH AGY IN REQUEST                    
*                                                                               
RQF16    LA    R0,INREC            READ FOR NEXT RECORD ON TAPE                 
         GET   SAPIN,(0)                                                        
*                                                                               
         CLC   =C'HDR',INREC+4                                                  
         JNE   RQF16               DONE WITH DETAILS                            
         J     RQF12                                                            
         EJECT                                                                  
*=========================================================                      
* PROCESS THIS HEADER                                                           
*=========================================================                      
                                                                                
RQF20    DS    0H                                                               
         BRAS  RE,ENDHDR                                                        
*                                                                               
         L     RE,=A(DTLBUFF)                                                   
         ST    RE,NEXTDTL                                                       
         XC    0(8,RE),0(RE)                                                    
*                                                                               
         BAS   RE,VALHDR                                                        
         J     RQF10                                                            
*                                                                               
RQF30    CLC   =C'DTL',INREC+4                                                  
         JNE   *+2                                                              
*                                                                               
         MVI   SAPDTREC,C' '       CLEAR SAPDTREC                               
         MVC   SAPDTREC+1(L'SAPDTREC-1),SAPDTREC                                
*                                                                               
         LH    RE,INREC                                                         
         SHI   RE,5                  4 BYTES LENGTH + 1 FOR EX MOVE             
         MVC   SAPDTREC(0),INREC+4   MOVE FROM INPUT AREA TO DTL AREA           
         EX    RE,*-6                                                           
         XC    SAPDERR,SAPDERR                                                  
*                                                                               
         BAS   RE,VALDTL           PRINT AND VALIDATE DETAIL REC                
*                                                                               
         L     RE,NEXTDTL           AND SAVE IT IN BUFFER TOO                   
         MVC   0(L'SAPDTREC,RE),SAPDTREC                                        
         LA    RE,L'SAPDTREC(RE)                                                
         L     R0,=A(DTLBUFFX)                                                  
         AHI   R0,-L'SAPDTREC                                                   
         CR    RE,R0                                                            
         JH    *+2                 TOO MANY DETAILS                             
         ST    RE,NEXTDTL                                                       
*                                                                               
         XC    0(8,RE),0(RE)       CLEAR NEXT ENTRY                             
         J     RQF10                                                            
*                                                                               
*=================================                                              
* CLOSE TAPE                                                                    
*=================================                                              
*                                                                               
ENDIN    LARL  R2,SAPIN                                                         
         CLOSE (R2)                                                             
         CLI   ISTPEMPT,C'Y'     IS TAPE EMPTY ?                                
         JE    XIT               TAPE OR DATASET IS EMPTY, EXIT.                
*                                                                               
         BRAS  RE,ENDHDR                                                        
         CLI   SAPHERR,0           TEST ERROR IN HEADER                         
         JNE   ENDIN10             YES - DON'T BOTHER POSTING                   
         CLI   SAPDERR,0           TEST ERROR IN DETAIL                         
         JNE   ENDIN10             YES - DON'T BOTHER POSTING                   
         BRAS  RE,PTOTAL                                                        
ENDIN10  BRAS  RE,CLSWK          CLOSE WORKER FILE.                             
*                                                                               
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================                               
* PRINT AND VALIDATE SAP HEADER RECORD                                          
*================================================                               
                                                                                
VALHDR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   RCSUBPRG,0                                                       
         XC    CKPSTDAT,CKPSTDAT                                                
         XC    CKDATE,CKDATE                                                    
         CLC   SAPCDN,SPACES               DO WE HAVE CLEARING DOC #            
         JH    *+8                                                              
         OI    SAPHERR,HERRCDOC            MISSING CLEARING DOC                 
         CLC   SAPDOCN,SPACES              DO WE HAVE DOCUMENT NUMBER           
         JH    *+8                                                              
         OI    SAPHERR,HERRDOC             MISSING DOCUMENT NUMBER              
         CLC   SAPCUST,SPACES              DO WE HAVE CUSTOMER NUMBER           
         JH    *+8                                                              
         OI    SAPHERR,HERRCUSN            MISSING DOCUMENT NUMBER              
         CLC   SAPCHKN,SPACES              DO WE HAVE CHECK NUMBER              
         JH    *+8                                                              
         OI    SAPHERR,HERRCHKN            MISSING CHECK NUMBER                 
*                                                                               
         MVC   XHEAD2+18(L'SAPCDN),SAPCDN                                       
         MVC   XHEAD3+18(L'SAPDOCN),SAPDOCN                                     
         MVC   XHEAD3+46(L'SAPCPYCD),SAPCPYCD                                   
         MVC   XHEAD3+74(L'SAPFYR),SAPFYR                                       
         MVC   XHEAD4+18(L'SAPCHKN),SAPCHKN                                     
*                                           CHANGE MMDDYYYY ->YYYYMMDD          
         MVC   CKPSTDAT(4),SAPPSTDT+4       YYYY                                
         CLI   CTRY,CTRYUS                  IS IT USA RUN ?                     
         JNE   VALH10                                                           
         MVC   CKPSTDAT+4(2),SAPPSTDT       MM                                  
         MVC   CKPSTDAT+6(2),SAPPSTDT+2     DD                                  
         J     VALH20                                                           
*                                           CHANGE DDMMYYYY ->YYYYMMDD          
VALH10   CLI   CTRY,CTRYCAN                 IS IT CANADIAN RUN ?                
         JNE   *+2                                                              
         MVC   CKPSTDAT+4(2),SAPPSTDT+2     MM                                  
         MVC   CKPSTDAT+6(2),SAPPSTDT       DD                                  
VALH20   GOTO1 DATVAL,DMCB,(0,CKPSTDAT),WORK                                    
         OC    WORK,WORK                                                        
         JNZ   *+8                                                              
         OI    SAPHERR,HERRIPDT             INVALID POSTING DATE                
         GOTO1 DATCON,DMCB,(9,CKPSTDAT),(21,XHEAD4+46)                          
*                                           CHANGE DDMMYYYY ->YYYYMMDD          
         MVC   CKDATE(4),SAPPSTDT+4         YYYY                                
         CLI   CTRY,CTRYUS                  IS IT USA RUN ?                     
         JNE   VALH30                                                           
         MVC   CKDATE+4(2),SAPPSTDT         MM                                  
         MVC   CKDATE+6(2),SAPPSTDT+2       DD                                  
         J     VALH40                                                           
*                                           CHANGE DDMMYYYY ->YYYYMMDD          
VALH30   MVC   CKDATE+4(2),SAPCHKDT+2       MM                                  
         MVC   CKDATE+6(2),SAPCHKDT         DD                                  
VALH40   GOTO1 DATVAL,DMCB,(0,CKDATE),WORK                                      
         OC    WORK,WORK                                                        
         JNZ   *+8                                                              
         OI    SAPHERR,HERRICDT             INVALID CHECK DATE                  
         GOTO1 DATCON,DMCB,(9,CKDATE),(21,XHEAD4+74)                            
*                                                                               
*        MVC   WORK,SPACES          VALIDATE CHECK NUMBER                       
*        MVC   WORK(L'SAPCHKN),SAPCHKN                                          
*        LA    R4,SAPCHKN                                                       
*        LA    R5,L'SAPCHKN                                                     
*                                                                               
*VH04     CLI   0(R4),C'0'                                                      
*        BL    VHERR2                                                           
*        CLI   0(R4),C'9'                                                       
*        BH    VHERR2                                                           
*        LA    R4,1(R4)                                                         
*        BCT   R5,VH04                                                          
*                                                                               
         J     VHERRX                                                           
*                                                                               
VHERRX   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*=======================================================                        
* PRINT AND VALIDATE LINE ITEM DETAILS                                          
*=======================================================                        
                                                                                
VALDTL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PDTLD,R7                                                         
         LA    R7,XP                                                            
         MVC   PDTLD(PDTLLNQ),XSPACES                                           
         MVI   RCSUBPRG,1                                                       
*                                                                               
         LHI   RF,L'SAPNET                                                      
         GOTO1 CASHVAL,DMCB,(X'82',SAPNET),(RF),0                               
         CLI   0(R1),X'FF'                                                      
         JE    *+2                                                              
         AP    CHKTOT,DMCB+4(8)                                                 
*                                                                               
         OC    SAPTAX,SAPTAX                                                    
         JZ    VALDT10                                                          
         LHI   RF,L'SAPNET                                                      
         GOTO1 CASHVAL,DMCB,(X'82',SAPTAX),(RF),0                               
         CLI   0(R1),X'FF'                                                      
         JE    *+2                                                              
*                                                                               
VALDT10  DS    0H                                                               
         MVC   XHEAD6+9(L'SAPITMTX),SAPITMTX                                    
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         MVI   RCSUBPRG,2                                                       
*                                                                               
         CLC   SAPVOUCH,SPACES                                                  
         JH    *+8                                                              
         OI    SAPDERR,DERRVOUC    VOUCHER NUMBER MISSING                       
*                                                                               
         CLC   SAPSLORG,SPACES                                                  
         JH    *+8                                                              
         OI    SAPDERR,DERRSLOR    SALES ORG NUMBER MISSING                     
*                                                                               
         MVC   PDTLVOUC,SAPVOUCH                                                
         MVC   PDTLITEM,SAPITEM                                                 
         MVC   PDTLNET,SAPNET                                                   
         MVC   PDTLTAX,SAPTAX                                                   
         MVC   PDTLVEND,SAPVENDR                                                
         MVC   PDTLSLS,SAPSLORG                                                 
         MVC   PDTLMTY,SAPMTYP                                                  
         MVC   PDTLMSTY,SAPMSTYP                                                
         MVC   PDTLMOS,SAPMOS                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
*                                                                               
         BRAS  RE,RDTRANS          GO READ TRANSACTION RECORD                   
         JNZ   VDX                                                              
*                                                                               
         L     R4,AIO1                                                          
         USING TRNRECD,R4                                                       
         LA    R6,TRNRFST          POINT TO FIRST ELEM                          
         USING TRNELD,R6                                                        
         CLI   0(R6),X'44'                                                      
         JNE   *+2                                                              
         J     VDX                                                              
*                                                                               
VDX      J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* ENDHDR - ALL DETAIL LINES HAVE BEEN PROCESSED                                 
* IF NO ERRORS, UPDATE ACCMST, OUTPUT SAP RECORDS, AND                          
* CREATE POSTINGS FOR THIS HEADER                                               
*===========================================================                    
                                                                                
ENDHDR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SAPHERR,0           TEST ANY ERROR IN HEADER                     
         JNE   EH88                YES - PUT DETAILS W/O PROCESSING             
*                                                                               
         L     R8,=A(DTLBUFF)                                                   
*                                                                               
EH10     CLI   0(R8),0             ANY MORE ENTRIES                             
         BE    EH80                                                             
*                                                                               
         MVC   SAPDTREC,0(R8)      MOVE RECORD                                  
         CLI   SAPDERR,0           TEST ANY DETAIL ERRORS                       
         JNE   EH76                YES - DON'T UPDATE FILE                      
*                                                                               
         BAS   RE,RDTRANS          READ TRANSACTION REC FROM ACCMST             
         JNE   *+2                                                              
                                                                                
*=================================================================              
* SET STATUS BYTES IN RECORD                                                    
*=================================================================              
                                                                                
         L     R4,AIO1                                                          
         USING TRNRECD,R4                                                       
*                                                                               
         LA    R6,TRNRFST          POINT TO FIRST ELEM                          
         USING TRNELD,R6                                                        
         CLI   0(R6),X'44'                                                      
         JNE   *+2                                                              
*                                                                               
         LHI   RF,L'SAPNET                                                      
         GOTO1 CASHVAL,DMCB,(X'82',SAPNET),(RF),0                               
         CLI   0(R1),X'FF'          DMCB+4(8) HAS AMOUNT                        
         JE    *+2                                                              
*                                                                               
         L     R2,AOFFTAB           POST AMOUNT TO OFFICE TABLE                 
         USING OFFTABD,R2                                                       
         LA    R0,OFFTMAX                                                       
EH20     CLI   OFFTABD,OFFTEOTQ    TEST FREE ENTRY                              
         BE    EH30                                                             
         CLC   OFFTOFFC,TRNOFFC    MATCH ON OFFICE CODE                         
         BNE   EH25                                                             
         CLC   OFFTBILL,TRNKREF    DOES BILL/REF # MATCH ?                      
         BNE   EH25                                                             
         AP    OFFTAMNT,DMCB+4(8)  YES - ADD TO OFFICE POSTING                  
         B     EH40                                                             
EH25     LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,EH20             DO FOR NUMBER OF TABLE ENTRIES               
         DC    H'0'                                                             
EH30     MVC   OFFTOFFC,TRNOFFC    CREATE NEW TABLE ENTRY                       
         ZAP   OFFTAMNT,DMCB+4(8)  SAVE OFF AMOUNT IN TABLE                     
         MVC   OFFTBILL,TRNKREF    BILL NUMBER FROM DR                          
*                                                                               
EH40     DS    0H                                                               
         XC    SVMDTELD,SVMDTELD   CLEAR SAVED ELEM VALUES                      
         XC    SVFFTELD,SVFFTELD   CLEAR SAVED ELEM VALUES                      
*                                                                               
EH50     LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    EH75                                                             
         CLI   0(R6),MDTELQ        FIND 1A ELEMENT                              
         JE    EH60                                                             
         CLI   0(R6),FFTELQ        FIND DB ELEMENT                              
         JE    EH70                                                             
         J     EH50                                                             
*                                                                               
EH60     DS    0H                                                               
         LLC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         MVC   SVMDTELD(0),0(R6)     SAVE OFF 1A ELEM                           
         EX    RE,*-6                                                           
         J     EH50                                                             
*                                                                               
EH70     DS    0H                                                               
         USING FFTELD,R6                                                        
         CLI   FFTTYPE,FFTTINVN   INVOICE NUMBER ELEM ?                         
         JNE   EH50                                                             
         LLC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         MVC   SVFFTELD(0),FFTELD    SAVE OFF FFTELD                            
         EX    RE,*-6                                                           
         MVC   OFFTLINV,SVFFTELD                                                
         DROP  R2                                                               
         J     EH50                                                             
         DROP  R6                                                               
*                                                                               
EH75     CLI   WKIDCHK,0           TEST CHECK FILE OPEN                         
         JNE   *+8                                                              
         BRAS  RE,OPNWK                                                         
*                                                                               
         MVC   DFSCACNT,=CL14'SCSCASHIN'       MOVE SC ACCOUNT                  
         BRAS  RE,POST             POST SR TRANSACTIONS                         
*                                                                               
EH76     LA    R8,L'SAPDTREC(R8)                                                
         J     EH10                                                             
                                                                                
*===========================================================                    
* SET UP TO MAKE SC POSTINGS                                                    
*===========================================================                    
                                                                                
EH80     DS    0H                                                               
*                                                                               
         L     R2,AOFFTAB           BUILD OFFICE POSTINGS TO BANK               
         USING OFFTABD,R2          R2=A(OFFICE POSTING TABLE)                   
         LA    R0,OFFTMAX                                                       
EH85     CLI   OFFTABD,OFFTEOTQ    TEST E-O-T                                   
         BE    EH88                                                             
         ZAP   POSTAMNT,OFFTAMNT   SET AMOUNT & OFFICE CODE                     
         MVC   POSTOFFC,OFFTOFFC                                                
         MVC   POSTLINV,OFFTLINV                                                
         BRAS  RE,PSTC             POST TO CASH ACCOUNT                         
         XC    OFFTABD(OFFTABL),OFFTABD                                         
         LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,EH85             DO FOR NUMBER OF TABLE ENTRIES               
         DROP  R2                                                               
*                                                                               
EH88     L     R8,=A(DTLBUFF)                                                   
*                                                                               
EH90     CLI   0(R8),0             HAVE ANOTHER RECORD                          
         JE    EHX                 NO                                           
*                                                                               
         MVC   SAPDTREC,0(R8)      MOVE TO DETAIL REC                           
         BRAS  RE,PUTSAP           PUT RECORDS TO SAPOUT                        
*                                                                               
         LA    R8,L'SAPDTREC(R8)                                                
         J     EH90                                                             
*                                                                               
EHX      DS    0H                                                               
         BRAS  RE,CLRBUFF          CLEAR DETAILS BUFFER                         
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
*============================================================                   
* RDTRANS  - BUILD IOKEY FROM MOKEY IN LINE DETAIL RECORD                       
* AND READ TRANSACTION RECORD FROM ACCFILE                                      
*============================================================                   
                                                                                
RDTRANS  NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING TRNRECD,R4                                                       
*                                                                               
         GOTO1 HEXIN,DMCB,SAPMCPY,TRNKCPY,2                                     
         OC    12(4,R1),12(R1)                                                  
         JNZ   *+8                                                              
         OI    SAPDERR,DERRCPY     BAD COMPANY CODE IN INPUT                    
*                                                                               
         MVC   TRNKULA,SAPMULA    MOVE U/L/ACCOUNT                              
         MVC   TRNKOFF,SAPMOFFC                                                 
*                                                                               
         GOTO1 HEXIN,DMCB,SAPMCCPY,TRNKCCPY,2                                   
         OC    12(4,R1),12(R1)                                                  
         JNZ   *+8                                                              
         OI    SAPDERR,DERRCCPY     BAD CONTRA COMPANY IN INPUT                 
         JZ    NEQXIT                                                           
*                                                                               
         MVC   TRNKULC,SAPMCNAC                                                 
*                                                                               
         GOTO1 DATCON,DMCB,SAPMDATE,WORK                                        
         PACK  DUB(4),WORK(7)                                                   
         MVC   TRNKDATE,DUB        MOVE 3-CHAR PWOS DATE                        
*                                                                               
         MVC   TRNKREF,SAPMREF                                                  
*                                                                               
         GOTO1 HEXIN,DMCB,SAPMSEQ,TRNKSBR,2                                     
         OC    12(4,R1),12(R1)                                                  
         JNZ   *+8                                                              
         OI    SAPDERR,DERRSEQ      BAD TRANSACTION SEQUENCE IN INPUT           
                                                                                
         MVC   IOKEYSV,IOKEY                                                    
         GOTOR DMHIGHDR                READ HIGH                                
         CLC   IOKEYSV,IOKEY                                                    
         JE    *+12                                                             
         OI    SAPDERR,DERRTRNF    TRANSACTION NOT FOUND                        
         J     RDTRNX                                                           
*                                                                               
         GOTOR DMGETREC              GET RECORD                                 
         CLI   8(R1),0                                                          
         JE    RDTRNX                                                           
         DC    H'0'                                                             
*                                                                               
RDTRNX   J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* OUTPUT A RECORD TO SAP DATASET                                                
*===========================================================                    
                                                                                
PUTSAP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   HDRSET,C'Y'                                                      
         BE    PUTSAP10                                                         
         BRAS  RE,PUTHEAD          SET HEADLINES                                
         MVI   HDRSET,C'Y'                                                      
PUTSAP10 BRAS  RE,CLROUT           CLEAR OUTPUT RECORD                          
*                                                                               
         MVC   SRRCLDOC,SAPCDN                                                  
         MVC   SRRDOC,SAPDOCN                                                   
         MVC   SRRCOMP,SAPCPYCD                                                 
         MVC   SRRPOSDT,SAPPSTDT                                                
         MVC   SRCHKDT,SAPCHKDT                                                 
         MVC   SRRRVRS,SAPRVCL                                                  
         MVC   SRRVNDR,SAPVENDR                                                 
         MVC   SRRCHK,SAPCHKXX                                                  
         MVC   SRRMEDIA,SAPMSYS                                                 
         MVC   SRRRVRS,SAPRVCL                                                  
         MVC   SRRNET,SAPNET                                                    
         MVC   SRRCUST,SAPCUST                                                  
         MVC   SRRSALES,SAPSLORG                                                
         MVC   SRRMDTYP,SAPMTYP                                                 
         MVC   SRRMDSUB,SAPMSTYP                                                
         MVC   SRRMOS,SAPMOS                                                    
         MVC   SRACCT,SAPMULA                                                   
*                                                                               
         BRAS  RE,DISPERR          PUT ERROR MESSAGES TO OUTPUT FIELDS          
*                                                                               
         BRAS  RE,INSPIPE          INSERT PIPE DELIMETERS TO OUTPUT             
*                                                                               
         LA    R0,SRRREC                                                        
         PUT   SAPOUT,(0)                                                       
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* OUTPUT A HEADLINE TO SAP DATASET                                              
*===========================================================                    
                                                                                
PUTHEAD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,CLROUT              CLEAR OUTPUT RECORD                       
         MVC   SRRCLDOC,=CL10'SEP=|'  SET SEPARATOR AS PIPE                     
*                                                                               
         LA    R0,SRRREC                                                        
         PUT   SAPOUT,(0)          FIRST LINE OF TAPE IS SEPARATOR              
*                                                                               
         BRAS  RE,CLROUT           CLEAR OUTPUT RECORD                          
*                                                                               
         L     R0,AHEADLIN         ADDRESS OF HEADLINE TO PRINT TO TAPE         
         LA    RE,SRRREC           TAPE RECORD                                  
         LA    RF,SRRMAXLN         LENGTH OF TAPE RECORD                        
         LA    R1,HEADLNQ          LENGTH OF SOURCE                             
         ICM   R1,8,SPACES         PAD IT WITH SPACES                           
         MVCL  RE,R0               MOVE HEADLINE TO TAPE RECORD                 
*                                                                               
         LA    R0,SRRREC                                                        
         PUT   SAPOUT,(0)          PUT HEADLINE TO TAPE                         
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* INSERT PIPES TO OUPUT LINES                                                   
*===========================================================                    
                                                                                
INSPIPE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,PIPETAB                                                       
INSPIP10 ZICM  R1,0(RE),2             GET DISP TO WHERE PIPE IS NEEDED          
         LA    RF,SRRREC                                                        
         AR    RF,R1                                                            
         MVI   0(RF),PIPEEQU       MOVE C'|' DELIMETER                          
         LA    RE,L'PIPETAB(RE)                                                 
         CLC   =C'EOT',0(RE)       HAVE WE REACHED EOT                          
         JNE   INSPIP10                                                         
*                                                                               
INSPIPEX XIT1                                                                   
*                                                                               
PIPETAB  DS    0XL2                                                             
         DC    AL2(SRPIPE1-SRRREC)                                              
         DC    AL2(SRPIPE2-SRRREC)                                              
         DC    AL2(SRPIPE3-SRRREC)                                              
         DC    AL2(SRPIPE4-SRRREC)                                              
         DC    AL2(SRPIPE5-SRRREC)                                              
         DC    AL2(SRPIPE6-SRRREC)                                              
         DC    AL2(SRPIPE7-SRRREC)                                              
         DC    AL2(SRPIPE8-SRRREC)                                              
         DC    AL2(SRPIPE9-SRRREC)                                              
         DC    AL2(SRPIPE10-SRRREC)                                             
         DC    AL2(SRPIPE11-SRRREC)                                             
         DC    AL2(SRPIPE12-SRRREC)                                             
         DC    AL2(SRPIPE13-SRRREC)                                             
         DC    AL2(SRPIPE14-SRRREC)                                             
         DC    AL2(SRPIPE15-SRRREC)                                             
         DC    AL2(SRPIPE16-SRRREC)                                             
         DC    AL2(SRPIPE17-SRRREC)                                             
         DC    AL2(SRPIPE18-SRRREC)                                             
         DC    AL2(SRPIPE19-SRRREC)                                             
         DC    AL2(SRPIPE20-SRRREC)                                             
         DC    AL2(SRPIPE21-SRRREC)                                             
         DC    AL2(SRPIPE22-SRRREC)                                             
         DC    AL2(SRPIPE23-SRRREC)                                             
         DC    AL2(SRPIPE24-SRRREC)                                             
         DC    AL2(SRPIPE25-SRRREC)                                             
         DC    AL2(SRPIPE26-SRRREC)                                             
         DC    AL2(SRPIPE27-SRRREC)                                             
         DC    AL2(SRPIPE28-SRRREC)                                             
         DC    AL2(SRPIPE29-SRRREC)                                             
         DC    C'EOT'                                                           
*                                                                               
PIPEEQU  EQU   C'|'                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
* CLEAR OUTPUT RECORD                                                 *         
*======================================================================         
                                                                                
CLROUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,SRRREC                                                        
         LA    RF,SRRECLNQ                                                      
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         MVCL  RE,R0                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==================================================                             
* CLEAR LINE ITEMS / DETAILS BUFFER                                             
*==================================================                             
                                                                                
CLRBUFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,=A(DTLBUFF)                                                   
         L     R0,=A(DTLBUFFX)                                                  
         AHI   R0,-L'SAPDTREC                                                   
*                                                                               
CLRBUF10 XC    0(L'SAPDTREC,RE),0(RE)                                           
         LA    RE,L'SAPDTREC(RE)                                                
         CR    RE,R0                                                            
         JNH   CLRBUF10                                                         
*                                                                               
CLRBUFFX XIT1                                                                   
         LTORG                                                                  
                                                                                
*==================================================                             
* CLEAR WORKER FILE POSTING BUILD AREA                                          
*==================================================                             
                                                                                
CLRTWRK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,ATWORK           RE=A(TWORK)                                  
         L     R2,ATWORKL          RF=(LENGTH OF TWORK)                         
         LH    RF,0(R2)                                                         
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR POSTING BUILD AREA                     
*                                                                               
CLRTWRKX XIT1                                                                   
         LTORG                                                                  
                                                                                
*============================================                                   
* WRITE TRAILER AND CLOSE WORKER FILE                                 *         
*============================================                                   
                                                                                
CLSWK    NTR1  BASE=*,LABEL=*                                                   
         CLI   RCPOSTNG,C'N'                                                    
         JE    CLSWKX              NO POSTINGS                                  
         CLI   WKIDCHK,0           TEST WORKER FILE OPEN                        
         JE    XIT                                                              
*                                                                               
         L     R5,ATWORKL                                                       
         LA    RF,WKCLOSE                                                       
         GOTO1 WORKER,DMCB,(RF),ACHKBUFF,WKIDCHK,(R5)                           
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CLSWKX   J     XIT                                                              
WKCLOSE  DC    CL8'CLOSE'                                                       
*        DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*============================================                                   
* PUT A RECORD TO THE WORKER FILE                                     *         
*============================================                                   
                                                                                
PWK      NTR1  BASE=*,LABEL=*                                                   
         AP    WRKRCNT,=P'1'       COUNT THE RECORDS                            
*                                                                               
         LA    RE,CHKREC           POINT TO FIRST ELEM                          
PWK2     LLC   R0,1(RE)            FIND THE END OF THE RECORD                   
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         JNE   PWK2                                                             
*                                                                               
         LA    R0,CHKREC                                                        
         LA    RE,1(RE)                                                         
         SR    RE,R0                                                            
         SLL   RE,16                                                            
         ST    RE,CHKREC-4         SET RECORD LENGTH                            
*                                                                               
         GOTO1 WORKER,DMCB,WKADD,ACHKBUFF,WKIDCHK,CHKREC-4                      
         TM    DMCB+8,X'C0'                                                     
         JNZ   *+2                                                              
*                                                                               
PWKX     XIT1                                                                   
WKADD    DC    CL8'ADD'                                                         
         LTORG                                                                  
         EJECT                                                                  
*==================================================                             
* OPEN THE WORKER FILE AND WRITE HEADER RECORDS                                 
*==================================================                             
                                                                                
OPNWK    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   WRKRTOT,=P'0'                                                    
         ZAP   WRKRCNT,=P'0'                                                    
         CLI   RCPOSTNG,C'N'                                                    
         JE    OPNWKX              NO POSTINGS                                  
*                                                                               
         LA    RF,WKIDCHK                                                       
         USING UKRECD,RF                                                        
         MVC   UKUSRID,ORIGINUM    USER ID                                      
         MVC   UKSYSPRG,=C'ASA'    SYSTEM/PROGAM                                
*                                                                               
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   UKDAY,DUB           DAY NUMBER (PWOS)                            
         MVI   UKCLASS,C'P'                                                     
*                                                                               
         OI    UKFLAG,X'10'        SET RETENTION                                
         XC    CHKREC(96),CHKREC                                                
         LA    RE,CHKREC+28                                                     
         USING WKRECD,RE                                                        
         MVC   WKRETN,=H'30'       30 DAYS                                      
*                                                                               
         GOTO1 WORKER,DMCB,WKOPEN,ACHKBUFF,WKIDCHK,CHKREC                       
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
         DROP  RE,RF                                                            
OPNWKX   XIT1                                                                   
WKOPEN   DC    CL8'OPEN'                                                        
         LTORG                                                                  
***********************************************************************         
* BUILD POSTING FILE RECORD (SR CREDITS ONLY)                         *         
***********************************************************************         
                                                                                
POST     NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO1                                                          
         USING TRNRECD,R4                                                       
         BRAS  RE,CLRTWRK                                                       
*                                                                               
TAPE     USING TRNELD,TRNRFST                                                   
         CLI   TAPE.TRNEL,X'44'                                                 
         JNE   *+2                                                              
*                                                                               
POSTD2   L     R5,ATWORK           POST DETAIL ITEM                             
         USING PSHEADD,R5                                                       
         MVI   PSHDEL,PSHDELQ      POSTING HEADER                               
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC,TRNKCULA     MOVE SR ACCOUNT FROM INPUT TAPE KEY         
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,TRNKCULC    CONTRA FROM INPUT TAPE KEY                  
         MVC   PSHDSBNM,SPACES                                                  
*                                                                               
POSTD11  L     R6,ATWORKT                                                       
         USING TRNELD,R6                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
         MVI   TRNSUB,0                                                         
         GOTO1 DATCON,DMCB,SAPMDATE,WORK  CONVERT TO DDS DATE                   
         PACK  DUB(4),WORK(7)      PACKING FBF4F0F8F0F1YX                       
         MVC   TRNDATE,DUB         GIVING B40801XY AND XY IS IGNORED            
         MVC   TRNREF,TRNKREF      REF NUMBER FROM INPUT TAPE KEY               
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNBTCH(2),SVMOS                                                 
         MVC   TRNOFFC,TAPE.TRNOFFC OFFICE FRM INPUT TAPE KEYS TRNELD           
         DROP  TAPE                                                             
*                                                                               
         MVI   TRNNARR,C' '                                                     
         LHI   RF,L'SAPNET                                                      
         GOTO1 CASHVAL,DMCB,(X'82',SAPNET),(RF),0                               
         CLI   0(R1),X'FF'                                                      
         JE    *+2                                                              
         ZAP   TRNAMNT,DMCB+4(8)                                                
         MP    TRNAMNT,=P'-1'     DOLLAR VALUES NEED TO BE REVERSED             
         MVI   TRNTYPE,X'1E'     TRANSACTION TYPE - TYPE 30                     
*                                                                               
         BRAS  RE,BLDNARR                                                       
         LA    RE,L'POSTNARR-1     CALCULATE LENGTH OF NARRATIVE                
         LA    RF,POSTNARR+L'POSTNARR-1                                         
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         JCT   RE,*-10                                                          
         MVC   TRNNARR(0),POSTNARR                                              
         EX    RE,*-6                                                           
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
*                                                                               
         BRAS  RE,ADDC0            ADD APEELD ELEMENT                           
         BRAS  RE,ADDD9            ADD RALELD ELEMENT                           
         BRAS  RE,ADD2C            ADD SPAELD ELEMENT                           
         BRAS  RE,ADD1A            ADD MDTELD ELEMENT                           
         BRAS  RE,ADDDB            ADD FFTELD/FFTTINVN ELEMENT                  
*                                                                               
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0             AT END YET ?                                 
         JNE   *-12                NO                                           
*                                                                               
POSTD26  BAS   RE,PUTAPE                                                        
*                                                                               
POSTX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD POSTING FILE RECORD (SC DEBITS ONLY)                          *         
***********************************************************************         
                                                                                
PSTC     NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO1                                                          
         USING TRNRECD,R4                                                       
*                                                                               
PSTCD2   L     R5,ATWORK           POST DETAIL ITEM                             
         USING PSHEADD,R5                                                       
         MVI   PSHDEL,PSHDELQ      POSTING HEADER                               
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC(1),TRNKCPY                                               
         MVC   PSHDACC+1(14),DFSCACNT DEFAULT SC ACCOUNT                        
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,TRNKCULA    CONTRA FROM INPUT TAPE KEY SR ACC           
         MVC   PSHDSBNM,SPACES                                                  
*                                                                               
*                                                                               
PSTCD11  L     R6,ATWORKT                                                       
         USING TRNELD,R6                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
         MVI   TRNSUB,0                                                         
         GOTO1 DATCON,DMCB,SAPMDATE,WORK  CONVERT TO DDS DATE                   
         PACK  DUB(4),WORK(7)      PACKING FBF4F0F8F0F1YX                       
         MVC   TRNDATE,DUB         GIVING B40801XY AND XY IS IGNORED            
         MVC   TRNREF,SAPCHKN      REF NUMBER = CHECK NUMBER                    
         OI    TRNSTAT,TRNSDR      MARK IT AS DEBIT                             
         OI    TRNSTAT,TRNSAUTH    INVOICE AUTHORISED                           
         MVC   TRNBTCH,SPACES                                                   
         MVC   TRNBTCH(2),SVMOS                                                 
         MVC   TRNOFFC,POSTOFFC    OFFICE FROM SR DR INPUT KEY                  
*                                                                               
         ZAP   TRNAMNT,POSTAMNT  AMOUNT PER EACH SR DEBIT OFFICE                
         MP    TRNAMNT,=P'-1'     DOLLAR VALUES NEED TO BE REVERSED             
         MVI   TRNTYPE,TRNTCALC  TRANSACTION TYPE - TYPE 30                     
*                                                                               
         BRAS  RE,BLDNARR                                                       
         LA    RE,L'POSTNARR-1     CALCULATE LENGTH OF NARRATIVE                
         LA    RF,POSTNARR+L'POSTNARR-1                                         
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         JCT   RE,*-10                                                          
         MVC   TRNNARR(0),POSTNARR                                              
         EX    RE,*-6                                                           
         LA    RE,TRNLN1Q+1(RE)                                                 
         STC   RE,TRNLN            SET ELEMENT LENGTH                           
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
*                                                                               
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         JNE   *-12                                                             
*                                                                               
         USING FFTELD,R6                                                        
         XC    FFTELD(L'SVFFTELD),FFTELD                                        
         MVC   FFTELD(L'SVFFTELD),POSTLINV   X'DB' ELEMENT FFTTINVN             
*                                                                               
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
*                                                                               
*&&DO                                                                           
         L     R6,ATWORKT                                                       
         CP    GST,=P'0'                                                        
         JE    PSTCD21                                                          
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         JNE   *-12                                                             
         USING SCIELD,R6                                                        
         XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        X'50' ELEMENT                                
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTAXP    GST TAX - TYPE 'T'                           
         ZAP   SCIAMNT,GST         GST AMOUNT                                   
         ZAP   SCIBASE,NET                                                      
         AP    SCIBASE,CASHD       BASIS = NET + CD                             
         MVC   SCISUBTY,SPACES     SUB TYPE                                     
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
*                                                                               
PSTCD21  CP    PST,=P'0'           ANY PST PRESENT AT ALL ?                     
         JE    PSTCD24             NO                                           
         MVC   BYTE,SRTGIND        SAVE OFF INDICATOR BYTE                      
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0             AT END YET ?                                 
         JNE   *-12                NO                                           
*                                                                               
         USING SCIELD,R6                                                        
         LA    RE,SRTPST           POINT TO BLOCK OF PST DATA                   
         LA    RF,L'SRTPST/SRTPSTLQ                                             
*                                                                               
         USING SRTPST,RE                                                        
PSTCD23  CLI   SRTPSTYP,C' '       ANY PST TYPE ?                               
         JE    PSTCD24             NO, FINISHED                                 
         XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        X'50' ELEMENT                                
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTQST    PST TAX - TYPE 'Q'                           
         TM    BYTE,X'20'          IS PST RUNNING IN BINARY?                    
         JO    *+14                                                             
         ZAP   DUB,SRTPSTAM                                                     
         J     *+12                                                             
         ICM   R1,15,SRTPSTAM                                                   
         CVD   R1,DUB                                                           
         ZAP   SCIAMNT,DUB                                                      
         ZAP   SCIBASE,NET                                                      
         AP    SCIBASE,CASHD                                                    
         AP    SCIBASE,GST         BASIS = NET+CD+GST                           
         MVC   SCISUBTY,SPACES     SUB TYPE                                     
         MVC   SCISUBPR,SRTPSTPR   PROVINCE CODE                                
         MVC   SCISUBPT,SRTPSTYP   PRIVINCE TAX TYPE                            
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
         LA    RE,SRTPSTLQ(RE)     BUMP TO NEXT PST ENTRY                       
         JCT   RF,PSTCD23          LOOP TO BUILD ANOTHER X'50'                  
         DROP  RE                                                               
*                                                                               
         USING SCIELD,R6                                                        
PSTCD24  CP    SRTPGRS,=P'0'                                                    
         JE    PSTCD25                                                          
*                                                                               
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0             AT END YET ?                                 
         JNE   *-12                NO                                           
*                                                                               
         XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        X'50' ELEMENT                                
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITGRSS    GROSS                                        
         ZAP   SCIAMNT,SRTPGRS                                                  
         ZAP   SCINET,=P'0'                                                     
         TM    SRTGIND,X'80'       SET X'80' IF NEEDED                          
         JZ    *+8                                                              
         OI    SCIINDR,SCIICAL                                                  
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
*                                                                               
         USING PIDEL,R6                                                         
PSTCD25  OC    SRTPID,SRTPID                                                    
         JZ    PSTCD26                                                          
         CLC   SRTPID,SPACES                                                    
         JE    PSTCD26                                                          
*&&                                                                             
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0             AT END YET ?                                 
         JNE   *-12                NO                                           
*                                                                               
PSTCD26  BAS   RE,PUTAPE                                                        
PSTCX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD APEELD TO THE POSTING RECORD                                    *         
***********************************************************************         
                                                                                
ADDC0    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         JNE   *-12                                                             
*                                                                               
         USING APEELD,R6                                                        
         XC    APEELD(256),APEELD                                               
         MVI   APEEL,APEELQ        X'C0' ELEMENT                                
         MVI   APENUM,X'01'        SET NUM OF ANALYSIS SUB ELEM TO 1            
*                                                                               
         MVI   APENSTAT,APENSDR                                                 
*                                                                               
         LA    RE,L'DFSCACNT-1     CALCULATE LENGTH OF NARRATIVE                
         LA    RF,DFSCACNT+L'DFSCACNT-1                                         
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         JCT   RE,*-10                                                          
         MVC   APENACT(0),DFSCACNT                                              
         EX    RE,*-6                                                           
         LA    RE,APELN2Q+1(RE)                                                 
         STC   RE,APENLEN          SET LENGTH OF SUB ELEMENT                    
*                                                                               
         LA    RE,APELN1Q(RE)                                                   
         STC   RE,APELN            SET LENGTH OF ELEMENT                        
*                                                                               
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
ADDC0X   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
***********************************************************************         
* ADD RALELD TO THE POSTING RECORD                                    *         
***********************************************************************         
                                                                                
ADDD9    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         JNE   *-12                                                             
*                                                                               
         USING RALELD,R6                                                        
         XC    RALEL(RALALCLQ),RALEL                                            
         MVI   RALEL,RALELQ        X'D9' ELEMENT                                
         MVI   RALLN,RALALCLQ                                                   
*                                                                               
         MVI   RALTYPE,RALTALC     REGULAR ALLOCATION                           
         MVC   RALAREF,SAPCHKN     CHECK NUMBER FROM HEADER RECORD              
*        GOTO1 DATVAL,DMCB,(0,SAPPSTDT),WORK                                    
*        OC    WORK,WORK                                                        
*        JNZ   *+6                                                              
*        DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(9,CKPSTDAT),(1,RALADEP)   POST DATE HEADER          
*        GOTO1 DATCON,DMCB,(9,SAPPSTDT),(1,RALADEP)   POST DATE HEADER          
*        GOTO1 DATVAL,DMCB,(0,SAPCHKDT),WORK                                    
*        OC    WORK,WORK                                                        
*        JNZ   *+6                                                              
*        DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(9,CKDATE),(1,RALADAT)  CHECK DATE HEADER            
*        GOTO1 DATCON,DMCB,(9,SAPCHKDT),(1,RALADAT)  CHECK DATE HEADER          
*                                                                               
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
ADDD9X   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD SAPELD TO THE POSTING RECORD                                    *         
***********************************************************************         
                                                                                
ADD2C    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         JNE   *-12                                                             
*                                                                               
         USING SPAELD,R6                                                        
         XC    SPAELD(SPALNQ+1),SPAELD                                          
         MVI   SPAEL,SPAELQ        X'2C' ELEMENT                                
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATBANK    BANK ACCOUNT SPAEL                           
         MVC   SPAAULA,DFSCACNT    DEFAULT SC ACCOUNT                           
*                                                                               
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
ADD2CX   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD MDTELD TO THE POSTING RECORD                                    *         
***********************************************************************         
                                                                                
ADD1A    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         JNE   *-12                                                             
*                                                                               
         USING MDTELD,R6                                                        
         XC    MDTELD(MDTLNQ+1),MDTELD                                          
         MVC   MDTELD(MDTPRD-MDTELD),SVMDTELD   X'1A' ELEMENT                   
*                                                                               
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         MVI   0(R6),0             NEW END OF RECORD                            
ADD1AX   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD FFTELD TO THE POSTING RECORD                                    *         
***********************************************************************         
                                                                                
ADDDB    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ATWORKT                                                       
         LLC   R1,1(R6)            FIND END OF RECORD                           
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         JNE   *-12                                                             
*                                                                               
         USING FFTELD,R6                                                        
         XC    FFTELD(L'SVFFTELD),FFTELD                                        
         MVC   FFTELD(L'SVFFTELD),SVFFTELD   X'DB' ELEMENT FFTTINVN             
*                                                                               
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
*                                                                               
                                                                                
         XC    FFTELD(FFTDATA-FFTELD+FFTSPLNQ+1),FFTELD                         
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTDATA-FFTELD+FFTSPLNQ                                    
         MVI   FFTTYPE,FFTTSINT    SAP INTERFACE INFO                           
         MVI   FFTDLEN,FFTSPLNQ                                                 
         MVC   FFTSCDOC,SAPCDN    SAP CLEARING DOCUMENT NUMBER                  
         MVC   FFTSDOCN,SAPDOCN   SAP DOCUMENT NUMBER                           
         MVC   FFTSCUST,SAPCUST   SAP CUSTOMER NUMBER                           
         MVC   FFTSVOUN,SAPVOUCH  SAP VOUCHER NUMBER - DETAIL                   
         MVC   FFTSSORG,SAPSLORG  SAP SALES ORG - DETAIL                        
         MVC   FFTCHKNM,SAPCREF#   SAP CLIENT REF # / CHECK NUMBER              
         MVC   FFTVENDR,SAPVENDR  SAP VENDOR                                    
         MVC   FFTMEDTY,SAPMTYP   SAP MEDIA TYPE                                
         MVC   FFTMDSTY,SAPMSTYP  SAP MEDIA SUB TYPE                            
                                                                                
         LLC   R0,FFTLN                                                         
         AR    R6,R0                                                            
                                                                                
         MVI   0(R6),0             NEW END OF RECORD                            
                                                                                
ADDDBX   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* POST THE TOTAL RECORD                                               *         
***********************************************************************         
                                                                                
PTOTAL   NTR1  BASE=*,LABEL=*                                                   
         L     R5,ATWORK                                                        
         USING PSSUBFD,R5                                                       
         XC    PSSBEL(PSSUBFL),PSSBEL    CLEAR ELEMENT                          
         MVI   PSSBEL,PSSBELQ                                                   
         MVI   PSSBLEN,PSSUBFL                                                  
         MVC   PSSBDESC(8),=C'RECEIVED'                                         
         OC    WRKRCNT,WRKRCNT                                                  
         JZ    PTOTX                                                            
         ZAP   PSSBRECS,WRKRCNT                                                 
         ZAP   PSSBCASH,WRKRTOT                                                 
         CP    PSSBRECS,=P'0'                                                   
         JE    PTOTX                                                            
         MVI   PSSBCASH+6,0                                                     
         BAS   RE,PUTAPE                                                        
PTOTX    XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WRITE AN ACPOST RECORD                                              *         
***********************************************************************         
                                                                                
PUTAPE   NTR1  BASE=*,LABEL=*                                                   
         CLI   RCPOSTNG,C'N'                                                    
         JE    PUTX                NO POSTINGS                                  
*                                                                               
         L     R4,ATWORK                                                        
         SR    R2,R2                                                            
PUT2     CLI   0(R4),0                                                          
         JE    PUT4                                                             
         CLI   0(R4),TRNELQ        GET TOTAL DEBITS                             
         JNE   PUT3                                                             
         TM    TRNSTAT-TRNELD(R4),X'80'                                         
         JNO   *+10                                                             
         AP    WRKRTOT,TRNAMNT-TRNELD(6,R4)                                     
*                                                                               
PUT3     IC    R2,1(R4)                                                         
         AR    R4,R2                                                            
         J     PUT2                                                             
*                                                                               
PUT4     L     R6,ATWORKL               WORK OUT LENGTH FOR WORKER              
         XC    0(4,R6),0(R6)                                                    
         LA    R4,1(R4)                                                         
         SR    R4,R6                                                            
         STH   R4,0(R6)                                                         
         L     R5,ATWORKL                                                       
*                                                                               
         AP    WRKRCNT,=P'1'       COUNT THE RECORDS                            
         GOTO1 WORKER,DMCB,=CL8'ADD',ACHKBUFF,WKIDCHK,(R5)                      
         TM    DMCB+8,X'C0'                                                     
         JNZ   *+2                                                              
*                                                                               
PUTX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE BUILDS TRANSACTION NARRATIVE                                *         
***********************************************************************         
         SPACE 1                                                                
BLDNARR  NTR1  BASE=*,LABEL=*                                                   
         MVI   POSTNARR,C' '                                                    
         MVC   POSTNARR+1(L'POSTNARR-1),POSTNARR FILL IN SPACES                 
         LA    R2,POSTNARR                                                      
         MVC   0(L'AC@CHKC,R2),AC@CHKC                                          
         LA    R2,L'AC@CHKC-1(R2)                                               
         CLI   0(R2),C' '                                                       
         JH    *+8                 GET TO FIRST CHARACTER                       
         JCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(L'TRNREF,R2),SAPCHKN   CHECK NUMBER FROM HEADER                
         LA    R2,L'TRNREF-1(R2)                                                
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@DATED,R2),AC@DATED                                        
         LA    R2,L'AC@DATED-1(R2)                                              
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         GOTO1 DATCON,DMCB,(9,CKDATE),(8,(R2))  CHECK DATE HEADER               
         CLI   0(R2),C' '                                                       
         JNE   *+10                                                             
         MVC   0(9,R2),1(R2)                                                    
         LA    R2,9(R2)                                                         
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(L'AC@DPSON,R2),AC@DPSON                                        
         LA    R2,L'AC@DPSON-1(R2)                                              
         CLI   0(R2),C' '                                                       
         JH    *+8                                                              
         JCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         GOTO1 DATCON,DMCB,(9,CKPSTDAT),(8,(R2))  CHECK DATE HEADER             
         CLI   0(R2),C' '                                                       
         JNE   *+10                                                             
         MVC   0(9,R2),1(R2)                                                    
BLDNARX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*======================================================================         
* DISPERR - FILL IN ERROR MESSAGE TO OUTPUT TAPE FIELD                *         
* LOOP THROUGH ERROR TABLE AND PUT OUT APPROPRIATE ERR TO TAPE FIELDS *         
*======================================================================         
                                                                                
DISPERR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RF,EMSGTAB          MESSAGE TEXT FIELD DISPLACEMENT TAB          
         LA    RE,HERRTAB          LOOK UP ERROR TEXT                           
DERR10   DS    0H                                                               
         LLC   R1,0(RE)            GET THE ERROR BYTE IN R1                     
         LLC   R3,SAPHERR          GET HEADER ERROR                             
         NR    R1,R3                                                            
         JZ    DERR20              NOT ZERO MEANS ERROR BYTES MATCH             
*                                                                               
         CLC   =C'EOT',0(RF)                                                    
         JE    *+2                 ERROR MESSAGE OUTPUT FULL                    
         ZICM  R1,0(RF),2          GET DISPLACEMENT OF THE OUTPUT FIELD         
         LA    R5,SRRREC                                                        
         AR    R5,R1                                                            
         MVC   0(L'ERRMSG,R5),1(RE) MOVE MESSAGE FROM TABLE TO OUTPUT           
         LA    RF,L'EMSGTAB(RF)    BUMP DISPLACEMENT TABLE                      
DERR20   LA    RE,L'HERRTAB(RE)                                                 
         CLC   =C'EOT',0(RE)                                                    
         JNE   DERR10                                                           
*                                                                               
         LA    RE,DERRTAB          LOOK UP DTL ERROR TEXT                       
DERR30   DS    0H                                                               
         LLC   R1,0(RE)            GET THE ERROR BYTE IN R1                     
         LLC   R3,SAPDERR          GET DETAIL ERROR                             
         NR    R1,R3                                                            
         JZ    DERR40                                                           
*                                                                               
         CLC   =C'EOT',0(RF)       KEEP RF INTACT FROM ABOVE                    
         JE    *+2                 ERROR MESSAGE OUTPUT FULL                    
         ZICM  R1,0(RF),2          GET DISPLACEMENT OF THE OUTPUT FIELD         
         LA    R5,SRRREC                                                        
         AR    R5,R1                                                            
         MVC   0(L'ERRMSG,R5),1(RE) MOVE MESSAGE FROM TABLE TO OUTPUT           
         LA    RF,L'EMSGTAB(RF)    BUMP DISPLACEMENT TABLE                      
DERR40   LA    RE,L'DERRTAB(RE)                                                 
         CLC   =C'EOT',0(RE)                                                    
         JNE   DERR30                                                           
*                                                                               
DISPERRX J     XIT                                                              
*                                                                               
HERRTAB  DS    0XL26                                                            
         DC    X'80'                                                            
ERRMSG   DC    CL25'MISSING CLEARING DOC #'                                     
         DC    X'40'                                                            
         DC    CL25'MISSING DOCUMENT #'                                         
         DC    X'20'                                                            
         DC    CL25'MISSING CUSTOMER #'                                         
         DC    X'10'                                                            
         DC    CL25'INVALID MEDIAOCEAN ID'                                      
         DC    X'08'                                                            
         DC    CL25'INVALID POSTING DATE'                                       
         DC    X'04'                                                            
         DC    CL25'INVALID CHECK DATE'                                         
         DC    X'02'                                                            
         DC    CL25'MISSING CHECK NUMBER'                                       
         DC    X'01'                                                            
         DC    CL25'SPARE'                                                      
         DC    C'EOT'                                                           
*                                                                               
HERRCDOC EQU   X'80'               MISSING CLEARING DOCUMENT #                  
HERRDOC  EQU   X'40'               MISSING DOCUMENT #                           
HERRCUSN EQU   X'20'               MISSING CUSTOMER #                           
HERRIMID EQU   X'10'               INVALID MEDIAOCEAN ID                        
HERRIPDT EQU   X'08'               INVALID POSTING DATE                         
HERRICDT EQU   X'04'               INVALID CHECK DATE                           
HERRCHKN EQU   X'02'               MISSING CHECK NUMBER                         
HERRSPR2 EQU   X'01'               SPARE 2                                      
*                                                                               
DERRTAB  DS    0XL26                                                            
         DC    X'80'                                                            
         DC    CL25'MISSING VOUCHER #'                                          
         DC    X'40'                                                            
         DC    CL25'MISSING SALES ORG #'                                        
         DC    X'20'                                                            
         DC    CL25'MISSING/BAD COMPANY CODE'                                   
         DC    X'10'                                                            
         DC    CL25'BAD CONTRA COMPANY CODE '                                   
         DC    X'08'                                                            
         DC    CL25'BAD TRANSACTION SEQ #'                                      
         DC    X'04'                                                            
         DC    CL25'TRANSACTION NOT FOUND'                                      
         DC    X'02'                                                            
         DC    CL25'SPARE'                                                      
         DC    X'01'                                                            
         DC    CL25'SPARE'                                                      
         DC    C'EOT'                                                           
*                                                                               
DERRVOUC EQU   X'80'               MISSING VOUCHER NUMBER                       
DERRSLOR EQU   X'40'               MISSING SALES ORG #                          
DERRCPY  EQU   X'20'               MISSING/BAD COMPANY CODE                     
DERRCCPY EQU   X'10'               MISSING/BAD CONTRA COMPANY                   
DERRSEQ  EQU   X'08'               MISSING/BAD SEQUENCE #                       
DERRTRNF EQU   X'04'               TRANSACTION NOT FOUND                        
DERRSPR3 EQU   X'02'               SPARE 3                                      
DERRSPR4 EQU   X'01'               SPARE 4                                      
*                                                                               
EMSGTAB  DS    0XL2                                                             
         DC    AL2(SRERR1-SRRREC)                                               
         DC    AL2(SRERR2-SRRREC)                                               
         DC    AL2(SRERR3-SRRREC)                                               
         DC    AL2(SRERR4-SRRREC)                                               
         DC    AL2(SRERR5-SRRREC)                                               
         DC    AL2(SRERR6-SRRREC)                                               
         DC    AL2(SRERR7-SRRREC)                                               
         DC    AL2(SRERR8-SRRREC)                                               
         DC    AL2(SRERR9-SRRREC)                                               
         DC    AL2(SRERR10-SRRREC)                                              
*        DC    AL2(SRERR11-SRRREC)                                              
*        DC    AL2(SRERR12-SRRREC)                                              
*        DC    AL2(SRERR13-SRRREC)                                              
*        DC    AL2(SRERR14-SRRREC)                                              
         DC    C'EOT'                                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
DMCTFIL  NTR1 BASE=*,LABEL=*       READ SEQUENTIAL                              
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',IOKEYSV,IOKEY                
         J     DMX                                                              
         LTORG                                                                  
*                                                                               
DMWRTDR  NTR1 BASE=*,LABEL=*       WRITE BACK TO DIR                            
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',IOKEY,IOKEY            
         J     DMX                                                              
         LTORG                                                                  
*                                                                               
DMADDDR  NTR1 BASE=*,LABEL=*       ADD KEY TO DIR                               
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',IOKEY,IOKEY            
         J     DMX                                                              
         LTORG                                                                  
*                                                                               
DMSEQDR  NTR1 BASE=*,LABEL=*       READ SEQUENTIAL                              
         GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),=C'ACCDIR ',IOKEYSV,IOKEY,0          
         J     DMX                                                              
         LTORG                                                                  
*                                                                               
DMHIGHDR NTR1 BASE=*,LABEL=*       READ HIGH                                    
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'ACCDIR ',IOKEYSV,IOKEY,0          
         J     DMX                                                              
         LTORG                                                                  
*                                                                               
DMREADDR NTR1 BASE=*,LABEL=*       READ                                         
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),=C'ACCDIR ',IOKEYSV,IOKEY,0          
         J     DMX                                                              
         LTORG                                                                  
*                                                                               
DMGETREC NTR1 BASE=*,LABEL=*       GET RECORD                                   
         USING ACCRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   SVDA,ACCKDA         SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,AIO1,DMWORK                  
         B     DMX                                                              
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
DMPUTREC NTR1 BASE=*,LABEL=*       PUT RECORD                                   
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,AIO1,DMWORK            
         B     DMX                                                              
         LTORG                                                                  
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
*=================================                                              
* LITERALS                                                                      
*=================================                                              
                                                                                
         LTORG                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
*ERRTAB   DC    100XL1'00'          ROOM FOR 100 ERROR                          
*ERRTABNT EQU   (*-ERRTAB)/L'ERRTAB                                             
*         DC    X'FF'                                                           
*                                                                               
ACCOUNT  DC    C'ACCOUNT'                                                       
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
ACCARC   DC    C'ACCARC '                                                       
ACCFIL   DC    C'ACCFIL '                                                       
ACCRCV   DC    C'ACCRCV '                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
*                                                                               
WKFILE   DC    CL8'WKFILE'                                                      
WKKEEP   DC    CL8'KEEP'                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
*        DS    CL700                                                            
ACSBWRK  DS    0D                                                               
*                                                                               
DATVAL   DC    V(DATVAL)                                                        
HEXIN    DC    V(HEXIN)                                                         
CASHVAL  DC    V(CASHVAL)                                                       
PUBEDIT  DC    V(PUBEDIT)                                                       
PQPROF   DC    V(PQPROF)                                                        
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
ACHKBUFF DC    A(CHKBUFF)                                                       
ATWORKL  DC    A(TWORKL)           A(LENGTH OF POSTING)                         
ATWORK   DC    A(TWORK)            A(POSTING FILE HEADER)                       
ATWORKT  DC    A(TWORKT)           A(POSTING FILE TRANSACTION)                  
APOST    DC    A(POST)             POSTING ROUTINE                              
AOFFTAB  DC    A(OFFTAB)           OFFICE TABLE BUFFER                          
AHEADLIN DC    A(HEADLINE)         OUTPUT HEADLINE                              
NEXTDTL  DS    A                                                                
ADBOX    DS    A                   ADDRESS OF bOX                               
*                                                                               
         DS    0D                                                               
RNSW     DS    XL1                 RUN CONTROL                                  
WRTNO    EQU   X'80'               RCWRITE=NO                                   
RUNTST   EQU   X'40'               RUN=TEST                                     
SVREMPQK DS    CL7                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'WKIDPOST'                                                    
WKIDPOST DS    XL(L'WKINDEX)       POSTING WORKER FILE ID FROM AC56             
         DS    0D                                                               
         DS    F                                                                
POSTREC  DS    XL256                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*WKIDCHK'                                                    
WKIDCHK  DS    XL(L'WKINDEX)       CHECK WORKER FILE ID                         
         DS    0D                                                               
         DS    F                   WORKER FILE LENGTH                           
CHKREC   DS    XL256                                                            
*                                                                               
RUNTOT   DS    PL6                 TOTAL CHECKS                                 
WRKRCNT  DS    PL6                 WORKER RECORDS                               
WRKRTOT  DS    PL6                 WORKER CASH                                  
CHOKS    DS    PL6                 NUMBER OF GOOD CHECKS                        
TOTTRNS  DS    PL6                 TOTAL TRANSACTIONS                           
CHKTOT   DS    PL6                                                              
SVDA     DS    F                                                                
DFSCACNT DS    CL14                                                             
SVMDTELD DS    CL255               SAVED OFF 1A ELEM FROM INP KEY READ          
SVFFTELD DS    CL255               SAVED OFF DB ELEM FROM INP KEY READ          
SVOFF    DS    CL2                 SAVED OFFICE                                 
CKPSTDAT DS    CL(L'SAPPSTDT)      DATE MMDDYYYY                                
CKDATE   DS    CL(L'SAPCHKDT)      DATE MMDDYYYY                                
CTRY     DS    CL1                 COUNTRY CODE                                 
CTRYUS   EQU   1                   USA                                          
CTRYCAN  EQU   2                   CANADA                                       
*                                                                               
POSTVALS DS    0X                  ** TRANSACTION POSTING VALUES **             
POSTACT  DS    XL(L'TRNKCULA-1)    ACCOUNT CODE                                 
POSTACTN DS    CL(L'NAMEREC)       ACCOUNT NAME                                 
POSTOFFC DS    CL(L'TRNOFFC)       OFFICE CODE                                  
POSTLINV DS    XL(L'OFFTLINV)      FFTELD FULL ELEM FOR POSTING                 
POSTDATE DS    PL(L'TRNKDATE)      DATE                                         
POSTREF  DS    CL(L'TRNKREF)       BILL NUMBER/REFERENCE                        
POSTAMNT DS    PL6                 AMOUNT                                       
POSTAM2  DS    PL6                 AMOUNT (CURRENCY)                            
POSTSTAT DS    XL(L'TRNSTAT)       TRANSACTION STATUS                           
POSTCAC  DS    XL(L'TRNKCULC)      CONTRA-ACCOUNT                               
POSTCACN DS    CL(L'NAMEREC)       CONTRA-ACCOUNT NAME                          
POSTNARR DS    CL(255-TRNLN1Q)                                                  
         DS    XL32                                                             
*                                                                               
*                                                                               
* FOR DATES 1=PWOS, 2=COMPRESSED, 3=BINARY                                      
*                                                                               
SVPOSDT2 DS    XL2                 FORMAT IS E14F                               
SVCLRDT2 DS    XL2                                                              
SVMOS    DS    XL2                 MONTH OF SERVICE                             
SVTRNMOS DS    PL2                                                              
*                                                                               
POSTC    DS    XL1                 POSTING CONTROL                              
PSTDTL   EQU   1                   POST CREDIT TO PAYABLE ACCOUNT               
PSTCASH  EQU   2                   POST MEDIA TOTAL (DEBIT TO SZ)               
PSTTOT   EQU   3                   POST THE TOTAL RECORD                        
*                                                                               
ODDKEY   DC    XL16'00'            INDEX KEY FOR ODD FILE                       
*                                                                               
TODAY3   DS    XL3                                                              
TODAY6   DS    CL6              TODAY CHARACTER YYMMDD                          
*                                                                               
ELCODE   DS    XL1                                                              
HDRSET   DS    C                                                                
ISTPEMPT DS    C                   IS TAPE EMPTY SWITCH                         
         DS    0D                                                               
ELEM     DS    XL64                                                             
         DS    0D                                                               
IOKEY    DS    XL64                                                             
IOKEYSV  DS    XL42                                                             
*                                                                               
         DS    0D                                                               
INREC    DS    XL256                                                            
*                                                                               
*                                                                               
*=====================================                                          
* SAP INPUT HEADER RECORD                                                       
* '*' DENOTES VALIDATED FIELDS                                                  
*=====================================                                          
                                                                                
         DS    0D                                                               
SAPREC   DS    XL200                                                            
         ORG   SAPREC                                                           
SAPRECTY DS    CL3                                                              
SAPHDRQ  EQU   C'HDR'              HEADER RECORD                                
*                                  Header record                                
SAPCDN   DS    CL10                SAPClearingDocumnetNumber                    
SAPDOCN  DS    CL10                SAPDocumentNumber                            
SAPCPYCD DS    CL4                 SAPCompanyCode                               
SAPCUST  DS    CL10                SAPCustomer                                  
SAPCREF# DS    CL16                                                             
         ORG   SAPCREF#                                                         
SAPCHKXX DS    CL10                client reference number check num.           
SAPCHKN  DS    CL6                 SAP Check number                             
SAPFYR   DS    CL35                FiscalYear                                   
SAPPSTDT DS    CL8                 PostingDate DDMMYYYY                         
SAPCHKDT DS    CL8                 EntryDate/Check Date/Ref# DDMMYYYY           
SAPCURR  DS    CL5                 Currency                                     
SAPMSYS  DS    CL25                MediaSystem                                  
SAPRVCL  DS    CL1                 ReverseClearing                              
SAPHERR  DS    XL1                 ERROR CODE (APPENDED BY THIS PROG)           
         DS    CL(L'SAPREC-(*-SAPREC))                                          
         ORG                                                                    
                                                                                
*=====================================                                          
* SAP INPUT DETAIL RECORD                                                       
* '*' DENOTES VALIDATED FIELDS                                                  
*=====================================                                          
                                                                                
         DS    0D                                                               
SAPDTREC DS    XL200                                                            
         ORG   SAPDTREC                                                         
SAPDTRTY DS    CL3                                                              
SAPDTLQ  EQU   C'DTL'              DETAIL LINE ITEM                             
*                                  Detail record                                
SAPITEM  DS    CL4                 ItemNumber                                   
SAPVOUCH DS    CL16                VoucherNumber                                
SAPLCUST DS    CL10                SAP LINE ITEM CUSTOMER                       
SAPLDOCN DS    CL10                SAP LINE ITEM DOCUMENT NUMBER                
SAPNET   DS    CL11                NetAmount                                    
SAPTAX   DS    CL11                TaxAmount                                    
SAPVENDR DS    CL10                SAPVendor                                    
SAPSLORG DS    CL4                 SAPSalesOrg                                  
SAPMTYP  DS    CL2                 MediaType                                    
SAPMSTYP DS    CL2                 MediaSubType                                 
SAPMOS   DS    CL6                 MonthOfService                               
SAPINSNO DS    CL10                InsertionNumber                              
SAPITMTX DS    CL50                SR DEBIT KEY ITEM TEXT                       
         ORG   SAPITMTX                                                         
SAPMCPY  DS    CL2                 COMPANY CODE                                 
SAPMULA  DS    CL14                ACCOUNT                                      
SAPMOFFC DS    CL2                 2-BYTE OFFICE                                
SAPMCCPY DS    XL2                 CONTRA COMPANY                               
SAPMCNAC DS    CL14                CONTRA ACCOUNT                               
SAPMDATE DS    CL6                 YYMMDD                                       
SAPMREF  DS    CL6                 TRANS REF                                    
SAPMSEQ  DS    XL2                 SEQ NUMBER                                   
SAPDERR  DS    XL1                 ERROR CODE (APPENDED BY THIS PROG)           
SAPSPR   DS    XL(*-SAPITMTX)                                                   
         DS    CL(L'SAPDTREC-(*-SAPDTREC))                                      
*                                                                               
         ORG                                                                    
                                                                                
*===============================================                                
* SAP REPORT RECORDS (SAPOUT)                                                   
*===============================================                                
                                                                                
SRRREC   DS    0CL500                                                           
SRRCLDOC DS    CL(L'SAPCDN)        CLEARING DOCNUM                              
SRPIPE1  DS    CL1                                                              
SRRDOC   DS    CL(L'SAPDOCN)       DOCNUM                                       
SRPIPE2  DS    CL1                                                              
SRRCOMP  DS    CL(L'SAPCPYCD)      COMPANY CODE                                 
SRPIPE3  DS    CL1                                                              
SRRPOSDT DS    CL(L'SAPPSTDT)      POSTING DATE                                 
SRPIPE4  DS    CL1                                                              
SRCHKDT  DS    CL(L'SAPCHKDT)      CHECK DATE                                   
SRPIPE5  DS    CL1                                                              
SRRVOUCH DS    CL(L'SAPVOUCH)      VOUCHER NUMBER                               
SRPIPE6  DS    CL1                                                              
SRRCHK   DS    CL(L'SAPCHKXX+L'SAPCHKN) CHECK NUMBER 16 CHARACTERS              
SRPIPE7  DS    CL1                                                              
SRRMEDIA DS    CL(L'SAPMSYS)       MEDIA SYSTEM                                 
SRPIPE8  DS    CL1                                                              
SRRRVRS  DS    CL(L'SAPRVCL)       REVERSE CLEARING                             
SRPIPE9  DS    CL1                                                              
SRRNET   DS    CL(L'SAPNET)        NET AMOUNT                                   
SRPIPE10 DS    CL1                                                              
SRRCUST  DS    CL(L'SAPCUST)       SAP CUSTOMER                                 
SRPIPE11 DS    CL1                                                              
SRRSALES DS    CL(L'SAPSLORG)      SALES ORG                                    
SRPIPE12 DS    CL1                                                              
SRRMDTYP DS    CL(L'SAPMTYP)       MEDIA TYPE                                   
SRPIPE13 DS    CL1                                                              
SRRMDSUB DS    CL(L'SAPMSTYP)      MEDIA SUBTYPE                                
SRPIPE14 DS    CL1                                                              
SRRMOS   DS    CL(L'SAPMOS)        MONTH OF SERVICE                             
SRPIPE15 DS    CL1                                                              
SRRVNDR  DS    CL(L'SAPVENDR)      VENDOR CODE                                  
SRPIPE16 DS    CL1                                                              
SRACCT   DS    CL(L'SAPMULA)       RECEIVABLE ACCOUNT                           
SRPIPE17 DS    CL1                                                              
SRERR1   DS    CL25                                                             
SRPIPE18 DS    CL1                                                              
SRERR2   DS    CL25                                                             
SRPIPE19 DS    CL1                                                              
SRERR3   DS    CL25                                                             
SRPIPE20 DS    CL1                                                              
SRERR4   DS    CL25                                                             
SRPIPE21 DS    CL1                                                              
SRERR5   DS    CL25                                                             
SRPIPE22 DS    CL1                                                              
SRERR6   DS    CL25                                                             
SRPIPE23 DS    CL1                                                              
SRERR7   DS    CL25                                                             
SRPIPE24 DS    CL1                                                              
SRERR8   DS    CL25                                                             
SRPIPE25 DS    CL1                                                              
SRERR9   DS    CL25                                                             
SRPIPE26 DS    CL1                                                              
SRERR10  DS    CL25                                                             
SRPIPE27 DS    CL1                                                              
SRERR11  DS    CL25                                                             
SRPIPE28 DS    CL1                                                              
SRERR12  DS    CL25                                                             
SRPIPE29 DS    CL1                                                              
         DS    CL(SRRMAXLN-(*-SRRREC))                                          
SRRECLNQ EQU   *-SRRREC                                                         
SRRMAXLN EQU   500                                                              
*                                                                               
*****************************************************                           
* DICTIONARY LIST                                                               
*****************************************************                           
DCLIST   DS    0C                                                               
         DCDDL AC#CHKC,13                                                       
         DCDDL AC#DATED,5                                                       
         DCDDL AC#DPSON,12                                                      
DCLISTX  DS    X'00'                                                            
*                                                                               
DSLIST   DS    0C                                                               
         DSDDL PRINT=YES                                                        
*                                                                               
SAPIN    DCB   DDNAME=SAPIN,RECFM=VB,DSORG=PS,EODAD=ENDIN,MACRF=GM              
*                                                                               
SAPOUT   DCB   DDNAME=SAPOUT,DSORG=PS,MACRF=(PM),RECFM=FB,LRECL=500             
*                                                                               
P1HDR    DC    C'==>  SAP CLRDOC XXXXXXXXXX  SAP DOCNUM XXXXXXXXXX'             
         DC    C' SAP COMPANY XXXX   FISCAL YR XXXX POSTED XXXXXXXX'            
P1HDRX   EQU   *                                                                
*                                                                               
P2HDR    DC    C'==>  CLEARED    XXXXXXXX    SAP VENDOR XXXXXXXXXX'             
         DC    C'  CHECK NUM XXXXXX   MEDIA SYS'                                
P2HDRX   EQU   *                                                                
*                                                                               
MIDDTL   DC    C'ITEM  NET AMOUNT TAX AMT  SAP CUST   SAPSLS  MONSVC'           
         DC    C'  MEDIA OCEAN KEY'                                             
MIDTTLX  EQU   *                                                                
*                                                                               
HEADLINE DC    C'CLEARING DOC #|DOCUMENT #|COMPANY CODE|POSTING DATE|'          
         DC    C'CHECK DATE|VOUCHER NUMBER|CHECK NUMBER|MEDIA SYSTEM|'          
         DC    C'REV CLEARING|NET AMOUNT|CUSTOMER NUMBER|SALES ORG|'            
         DC    C'MEDIA TYPE|MEDIA SUBTYPE|MOS|VENDOR CODE|'                     
         DC    C'RECEIVABLE ACCOUNT|ERROR 1|'                                   
         DC    C'ERROR 2|ERROR 3|ERROR 4|ERROR 5|ERROR 6|ERROR 7|'              
         DC    C'ERROR 8|ERROR 9|ERROR 10|ERROR 11|ERROR 12'                    
HEADLNQ  EQU   *-HEADLINE                                                       
HEADLINX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO**'                                                      
IO       DS    2000C                                                            
IO1      EQU   IO                                                               
IOLNQ    EQU   *-IO                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO2*'                                                      
IO2      DS    2000C                                                            
IO2LNQ   EQU   *-IO2                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*DTLBUFF'                                                    
DTLBUFF  DS    0D                                                               
         DS    100000C                                                          
DTLBUFFX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*CHKBUFF*'                                                   
CHKBUFF  DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*TWORK**'       TRANSACTON POSTING BLOCK                     
TWORKL   DC    F'0'                                                             
TWORK    DC    XL(PSHEADL)'00'                                                  
TWORKT   DC    5000X'00'                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'*OFFBUF*'       OFFICE AMOUNT TABLE BUFFER                   
OFFTAB   DS    (OFFTMAX)XL(OFFTABL)                                             
*                                                                               
ACDCB    DS    A                   A(DCB)                                       
*                                                                               
         EJECT                                                                  
ACSBDX   EQU   *                                                                
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
OFFTABD  DSECT                     DSECT COVERS OFFICE AMOUNT TABLE             
OFFTOFFC DS    CL(L'TRNOFFC)       OFFICE CODE                                  
OFFTEOTQ EQU   0                   END OF TABLE INDICATOR                       
OFFTAMNT DS    PL(L'TRNAMNT)       AMOUNT FOR BANK POSTING                      
*OFFTBILL DS    CL1                 BILL NUMBER FOR BANK POSTING                
*OFFTLINV DS    XL1                 FULL DB LONG INV # ELEMENT                  
OFFTBILL DS    CL(L'TRNKREF)       BILL NUMBER FOR BANK POSTING                 
OFFTLINV DS    XL14                FULL DB LONG INV # ELEMENT                   
OFFTMAX  EQU   500                 MAXIMUM NUMBER OF ENTRIES IN TABLE           
OFFTABL  EQU   *-OFFTABD           LENGTH OF TABLE ENTRY                        
         SPACE 2                                                                
*===============================================                                
* PRINT LINE DSECTS FOR HEADERS AND DETAIL LINES                                
*===============================================                                
                                                                                
P1D       DSECT                                                                 
P1EYEBALL DS   CL3                                                              
          DS   CL13                                                             
P1CLDOC   DS   CL10                                                             
          DS   CL13                                                             
P1DOC     DS   CL10                                                             
          DS   CL13                                                             
P1COMP    DS   CL4                                                              
          DS   CL13                                                             
P1FSCLYR  DS   CL4                                                              
          DS   CL8                                                              
P1POSTED  DS   CL8                                                              
*                                                                               
          DS   CL16                                                             
P2CHKD    DS   CL8                                                              
          DS   CL15                                                             
P2VNDR    DS   CL10                                                             
          DS   CL12                                                             
P2CHKNUM  DS   CL6                                                              
          DS   CL14                                                             
P2MDSYS   DS   CL25                                                             
*                                                                               
PDTLD    DSECT                                                                  
         DS    CL2                                                              
PDTLVOUC DS    CL16                                                             
         DS    CL1                                                              
PDTLITEM DS    CL4                                                              
         DS    CL4                                                              
PDTLNET  DS    CL11                                                             
         DS    CL1                                                              
PDTLTAX  DS    CL11                                                             
         DS    CL1                                                              
PDTLVEND DS    CL10          SAP VENDOR                                         
         DS    CL1                                                              
PDTLSLS  DS    CL4           SAP SALES ORG                                      
         DS    CL9                                                              
PDTLMTY  DS    CL2           SAP MEDIA TYPE                                     
         DS    CL9                                                              
PDTLMSTY DS    CL2           SAP SUB MEDIA TYPE                                 
         DS    CL11                                                             
PDTLMOS  DS    CL6                                                              
         DS    CL2                                                              
PDTLMOKEY DS   CL55                                                             
PDTLLNQ  EQU   *-PDTLD                                                          
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
                                                                                
         PRINT OFF                                                              
* ACREPWORKD                                                                    
       ++INCLUDE ACREPWORKD                                                     
* ACGENFILE                                                                     
       ++INCLUDE ACGENFILE                                                      
* ACGENMODES                                                                    
       ++INCLUDE ACGENMODES                                                     
* ACGENPOST                                                                     
       ++INCLUDE ACGENPOST                                                      
* ACMASTD                                                                       
       ++INCLUDE ACMASTD                                                        
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
* DDMASTD                                                                       
       ++INCLUDE DDMASTD                                                        
* DMWRKRD                                                                       
       ++INCLUDE DMWRKRD                                                        
* DMWRKRK                                                                       
       ++INCLUDE DMWRKRK                                                        
* FASSBOFF                                                                      
       ++INCLUDE FASSBOFF                                                       
* DMPRTQL                                                                       
       ++INCLUDE DMPRTQL                                                        
* ACQD (REQUEST CARD LAYOUT)                                                    
       ++INCLUDE ACQD                                                           
*                                                                               
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
*                                                                               
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPSB02 04/09/19'                                      
         END                                                                    
