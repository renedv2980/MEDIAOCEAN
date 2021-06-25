*          DATA SET ACCLB53    AT LEVEL 093 AS OF 08/16/00                      
*PHASE T62153A                                                                  
CLB53    TITLE '- PC COMMS - CLIENT/PRODUCT/JOB LISTS'                          
CLB53    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB53**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
*                                                                               
         CLI   LINKMODE,ROUTSN                                                  
         BNL   EXITY                                                            
         XR    RF,RF                                                            
         IC    RF,LINKMODE                                                      
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     SETMAP              SET A(MAP TABLE)                             
         B     RCVFST              FIRST FOR RECEIVE                            
         B     RCVHDRF             FIRST FOR MAP HEADER RECEIVE                 
         B     RCVDATA             DATA RECEIVE                                 
         B     RCVHDRL             LAST FOR MAP HEADER RECEIVE                  
         B     RCVLST              LAST FOR RECEIVE                             
         B     SND                 SEND                                         
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SET A(MAP TABLE)                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMAP   DS    0H                                                               
         LA    RF,MAPTAB           SET A(MAP TABLE)                             
         ST    RF,AMAPTAB                                                       
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR RECEIVE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RCVFST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR MAP HEADER RECEIVE                                        *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRF  DS    0H                                                               
         MVI   JOBSTAT,0                                                        
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ELEMENT DATA RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVDATA  DS    0H                                                               
         ICM   RF,15,ORCVDATA                                                   
         BNZR  RF                                                               
         B     EXITY                                                            
*                                                                               
***********************************************************************         
* CLI/PRO CODE RECEIVE                                                *         
***********************************************************************         
         SPACE 1                                                                
RCVCLI   DS    0H                  RECEIVE CLIENT CODE                          
         MVC   THISCLI,DATA                                                     
         B     EXITY                                                            
*                                                                               
RCVPRO   DS    0H                  RECEIVE PRODUCT CODE                         
         MVC   THISPRO,DATA                                                     
         B     EXITY                                                            
*                                                                               
RCVSTA   DS    0H                  RECEIVE JOB STATUS                           
         MVC   JOBSTAT,DATA                                                     
         B     EXITY                                                            
*                                                                               
         SPACE 1                                                                
***********************************************************************         
* JOB RECORD DISK ADDRESS RECIEVE                                    *          
***********************************************************************         
         SPACE 1                                                                
RCVJOBDA MVC   JOBDA,DATA                                                       
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* FORMAT NUMBER RECEIVE                                              *          
***********************************************************************         
         SPACE 1                                                                
RCVFMT   DS    0H                                                               
P        USING PBCRECD,IOKEY       READ FORMAT CONTROL RECORD                   
         XC    P.PBCKEY,P.PBCKEY                                                
         MVI   P.PBCKTYP,PBCKTYPQ                                               
         MVC   P.PBCKCPY,CUABIN                                                 
         MVI   P.PBCKSUB,PBCKCONQ                                               
         MVC   P.PBCKFMT,DATA                                                   
         MVC   P.PBCKLANG,CSFMLANG                                              
         GOTO1 AIO,IO3+IOACCDIR+IORD                                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITN                                                            
         MVC   CSFORMAT,DATA                                                    
         B     EXITY                                                            
         DROP  P                                                                
         SPACE 1                                                                
***********************************************************************         
* CURRENCY CODE RECEIVE                                              *          
***********************************************************************         
         SPACE 1                                                                
RCVJOB   DS    0H                                                               
         MVC   THISJOB,DATA                                                     
         B     EXITY                                                            
                                                                                
RCVCUR   DS    0H                                                               
         GOTO1 ASETUP,BOPARM,(X'38',THISJOB),DATA,0                             
         BNE   EXITN                                                            
         TM    BCJOBSTA,BCJOBPEN                                                
         BO    EXITN                                                            
*                                                                               
A        USING ACTRECD,IOKEY                                                    
         MVC   A.ACTKEY,BCSPACES                                                
         MVC   A.ACTKCPY,CUABIN                                                 
         MVC   A.ACTKUNT(L'BCCPYPRD),BCCPYPRD                                   
         MVC   A.ACTKACT,THISJOB                                                
         DROP  A                                                                
         GOTO1 AIO,IO1+IOACCDIR+IORD                                            
         BE    *+6                                                              
         DC    H'0'                WHERE'S THE JOB GONE?                        
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R2,ACTRFST-ACTRECD(R2)                                           
         XR    R0,R0                                                            
         USING ABLELD,R2                                                        
RCVCUR02 CLI   ABLEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO BALANCE !                                 
         CLI   ABLEL,ABLELQ                                                     
         BE    *+14                                                             
         IC    R0,ABLLN                                                         
         AR    R2,R0                                                            
         B     RCVCUR02                                                         
                                                                                
         ZAP   PL8,ABLFRWD                                                      
         AP    PL8,ABLDR                                                        
         SP    PL8,ABLCR                                                        
                                                                                
*&&UK                                                                           
         CLC   CSBILCUR,CSCPYCUR   NEED CONVERTING?                             
         BE    EXITY                                                            
         LA    R2,BOWORK1                                                       
         USING EURKBLKD,R2                                                      
         XC    0(EURKBLKL,R2),0(R2)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         ZAP   BODUB1,PL8                                                       
         GOTO1 VEUREKA,BODMCB,('APPLYQ',BOWORK1),BODUB1,PL8                     
         DROP  R2                                                               
*&&                                                                             
         B     EXITY                                                            
                                                                                
         SPACE 1                                                                
***********************************************************************         
* LAST FOR MAP HEADER RECEIVE                                         *         
***********************************************************************         
         SPACE 1                                                                
RCVHDRL  DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST FOR RECEIVE                                                    *         
***********************************************************************         
         SPACE 1                                                                
RCVLST   DS    0H                                                               
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* SEND                                                                *         
***********************************************************************         
         SPACE 1                                                                
SND      DS    0H                                                               
         ICM   RF,15,OSND                                                       
         BNZR  RF                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND CLIENT AND PRODUCT LIST INFORMATION                            *         
***********************************************************************         
                                                                                
SNDCPR   EQU   *                                                                
         PUSH  USING                                                            
A        USING ACTRECD,IOKEY       CLIENTS AND PRODUCTS                         
         MVC   A.ACTKEY,BCSPACES                                                
         MVC   A.ACTKCPY,CUABIN                                                 
         MVC   A.ACTKUNT(L'BCCPYPRD),BCCPYPRD                                   
         MVI   A.ACTKACT,X'41'       GET FIRST ACCOUNT                          
         CLC   THISCLI,BCSPACES    TEST CLIENT SENT                             
         BNH   CPR04                                                            
         SR    RF,RF                                                            
         IC    RF,BCCLILEN                                                      
         EX    RF,*+4                                                           
         MVC   A.ACTKACT(0),THISCLI                                             
         LA    RF,A.ACTKACT(RF)                                                 
         MVI   0(RF),X'41'       GET FIRST PRODUCT FOR GIVEN CLIENT             
                                                                                
CPR04    GOTO1 AIO,IO1+IOACCDIR+IOHI                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BCCPYPRD,A.ACTKUNT  TEST STILL IN THE PROD LEDGER                
         BNE   CPR20               DONE                                         
         CLC   THISCLI,BCSPACES    TEST ANY CLIENT SENT                         
         BNH   CPR10                                                            
         SR    RF,RF                                                            
         IC    RF,BCCLILEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8              CHECK SAME CLIENT                            
         BH    CPR20               NO,DONE                                      
         CLC   A.ACTKACT(0),THISCLI                                             
                                                                                
CPR10    GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         GOTO1 AGETELS,BODMCB,(RF),0                                            
         L     RF,AOFFBLK                                                       
         OI    OFFACTRL-OFFALD(RF),OFFACCNV                                     
         GOTO1 ATSTSEC                                                          
         BNE   CPR18               GET NEXT RECORD                              
*&&UK                                                                           
         XR    R1,R1                                                            
         ICM   R1,15,ACAPPR                                                     
         MVC   BOELEM(L'IOKEY),IOKEY  ATSTOFF USES IOKEY NOW                    
         GOTO1 ATSTOFF,PPRGAOFF-PPREL(R1)                                       
         MVC   IOKEY,BOELEM                                                     
         BNE   CPR18               GET NEXT RECORD                              
*&&                                                                             
                                                                                
         SR    R2,R2               TEST CLIENT RECORD                           
         IC    R2,BCCLILEN         R2=L'CLI                                     
         LA    RF,A.ACTKACT(R2)    RF=A(PRO)                                    
         CLI   0(RF),C' '                                                       
         BH    CPR14               MUST BE PRODUCT                              
         GOTO1 ASNDHDR,BODMCB,07                                                
         GOTO1 ASNDDATA,BODMCB,1,((R2),A.ACTKACT)                               
         GOTO1 ASNDDATA,BODMCB,2,ACNAME                                         
         B     CPR18               GET NEXT RECORD                              
                                                                                
CPR14    GOTO1 ASNDHDR,BODMCB,08   SEND PRODUCT CODE AND NAME                   
         SR    RF,RF                                                            
         IC    RF,BCCLILEN                                                      
         GOTO1 ASNDDATA,BODMCB,1,A.ACTKACT(RF)                                  
         GOTO1 ASNDDATA,BODMCB,2,ACNAME                                         
                                                                                
CPR18    SR    RE,RE                                                            
         IC    RE,BCCLILEN         RF=L'CLI                                     
         CLC   THISCLI,BCSPACES    IF CLIENT SENT                               
         BNH   *+8                                                              
         IC    RE,BCPROLEN         RE=L'CLIPRO                                  
         LA    RE,A.ACTKACT(RE)                                                 
         LR    RF,RE               RF=RE=A(END OF CLI OR PRO)                   
         LA    R1,A.ACTKACT                                                     
         SR    RE,R1               RE=L'CUL.ACT                                 
         LA    R1,L'ACTKEY                                                      
         SR    R1,RE               R1=L'BACKOF KEY                              
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),BCSPACES    CLEAR BACK OF KEY                            
         MVI   0(RF),FF            FORCE TO NEXT RECORD                         
         B     CPR04                                                            
                                                                                
CPR20    DS    0H                                                               
         B     EXITY                                                            
         DROP  A                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* SEND JOB LIST INFORMATION                                           *         
***********************************************************************         
                                                                                
SNDJBK   EQU   *                                                                
         PUSH  USING                                                            
         L     R1,AGOPBLK                                                       
         USING GOBLOCK,R1                                                       
         XC    GOBLOCK+8(GOADM-(GOBLOCK+8)),GOBLOCK+8                           
         DROP  R1                                                               
                                                                                
A        USING ACTRECD,IOKEY                                                    
         MVC   A.ACTKEY,BCSPACES                                                
         MVC   A.ACTKCPY,CUABIN                                                 
         MVC   A.ACTKUNT(L'BCCPYPRD),BCCPYPRD                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RF,BCCLILEN                                                      
         IC    RE,BCPROLEN                                                      
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   A.ACTKACT(0),THISCLI  SET CLIENT CODE                            
         LA    RF,A.ACTKACT+1(RF)                                               
         EX    RE,*+4                                                           
         MVC   0(0,RF),THISPRO     SET PRODUCT CODE                             
         LA    R2,1(RE,RF)         RF=A(FIRST JOB POSITION)                     
         MVI   0(R2),X'41'                                                      
                                                                                
JOB10    GOTO1 AIO,IO1+IOACCDIR+IOHI                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         LA    RF,ACTKACT-ACTKEY(RF)                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   IOKEY(0),IOKEYSAV   TEST SAME CLIENT PRODUCT                     
         BNE   JOB100              FINISH OFF                                   
         MVC   JOBDA,ACCKDA-ACCRECD+IOKEY                                       
                                                                                
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         GOTO1 AGETELS,BODMCB,(RF),0                                            
         L     RF,AOFFBLK                                                       
         OI    OFFACTRL-OFFALD(RF),OFFACCNV                                     
         GOTO1 ATSTSEC                                                          
         BNE   JOB62                                                            
*&&UK                                                                           
         XR    R1,R1                                                            
         ICM   R1,15,ACAPPR                                                     
         MVC   BOELEM(L'IOKEY),IOKEY  ATSTOFF USES IOKEY NOW                    
         GOTO1 ATSTOFF,PPRGAOFF-PPREL(R1)                                       
         MVC   IOKEY,BOELEM                                                     
         BNE   JOB62                                                            
*&&                                                                             
                                                                                
         MVC   CSBILCUR,BCSPACES                                                
         XC    CSEXCVAL,CSEXCVAL                                                
         XC    CSFORMAT,CSFORMAT                                                
         XC    BCJOBSTA,BCJOBSTA                                                
         MVC   KEYSAVE,IOKEY                                                    
         GOTO1 ASETUP,BOPARM,(0,A.ACTKACT),0,0                                  
         MVC   IOKEY,KEYSAVE                                                    
         BNE   JOB62               IGNORE CLOSED/LOCKED/                        
         CLI   JOBSTAT,0                                                        
         BNE   *+12                INCLUDE NON CBILL JOBS ELSE                  
         TM    BCJOBSTA,BCJOBSCB                                                
         BNO   JOB62               IGNORE NON CLIENT BILLING JOBS               
                                                                                
         GOTO1 ASNDHDR,BODMCB,09   SEND JOB INFO                                
         GOTO1 ASNDDATA,BODMCB,1,JOBDA                                          
         GOTO1 (RF),(R1),2,(L'ACTKACT,A.ACTKACT)                                
         GOTO1 (RF),(R1),3,(L'ACNAME,ACNAME)                                    
         GOTO1 (RF),(R1),10,CSCURBIL+(CURTDECP-CURTABD)                         
                                                                                
         L     R2,AIO1                                                          
         LA    R2,ACTRFST-ACTRECD(R2)                                           
         XR    R0,R0                                                            
         USING SCIELD,R2                                                        
JOB20    CLI   SCIEL,0                                                          
         BE    JOB50                                                            
         CLI   SCIEL,SCIELQ                                                     
         BE    JOB30                                                            
         CLI   SCIEL,ABLELQ                                                     
         BE    JOB40                                                            
JOB22    IC    R0,SCILN                                                         
         AR    R2,R0                                                            
         B     JOB20                                                            
                                                                                
JOB30    CLI   SCITYPE,SCITCBAP                                                 
         BNE   JOB32                                                            
         GOTO1 ASNDDATA,BODMCB,7,SCIAMNT NET ALLOCATION PENDING                 
         GOTO1 ASNDDATA,BODMCB,8,SCIADMN COMMISSION PENDING                     
         B     JOB22                                                            
JOB32    CLI   SCITYPE,SCITCBWP                                                 
         BNE   JOB34                                                            
         GOTO1 ASNDDATA,BODMCB,12,SCIADMN WRITE OFFS PENDING                    
         B     JOB22                                                            
JOB34    CLI   SCITYPE,SCITCBTP                                                 
         BNE   JOB22                                                            
         GOTO1 ASNDDATA,BODMCB,13,SCIAMNT TRANSFERS PENDING                     
         B     JOB22                                                            
         DROP  R2                                                               
                                                                                
         USING ABLELD,R2                                                        
JOB40    ZAP   PL8,ABLFRWD                                                      
         AP    PL8,ABLDR                                                        
         SP    PL8,ABLCR                                                        
         B     JOB22                                                            
         DROP  R2                                                               
                                                                                
JOB50    L     R4,AGOPBLK          GET BILL TYPE AND FORMAT                     
         USING GOBLOCK,R4                                                       
         GOTO1 ASNDDATA,BODMCB,14,GOBILTYP                                      
         L     R3,GOABEXT                                                       
         USING GOBBLOCK,R3                                                      
         CLI   GOPCBFRM,0                                                       
         BNE   *+8                                                              
         MVI   GOPCBFRM,1                                                       
         GOTO1 ASNDDATA,BODMCB,6,GOPCBFRM                                       
         DROP  R3,R4                                                            
                                                                                
         TM    BCCPYST6,CPYSFBIL   TEST FOREIGN CURRENCY BILLING                
         BZ    JOB60                                                            
         CLC   CSBILCUR,BCSPACES                                                
         BNH   JOB60                                                            
         CLC   CSBILCUR,CSCPYCUR                                                
         BE    JOB60                                                            
         GOTO1 ASNDDATA,BODMCB,4,CSBILCUR                                       
         OC    CSEXCVAL,CSEXCVAL   TEST EXCHANGE RATE SET                       
         BZ    JOB60                                                            
         GOTO1 ASNDDATA,BODMCB,5,CSEXCRAT                                       
         CLI   CSEXCSHF,0                                                       
         BE    JOB60                                                            
         GOTO1 ASNDDATA,BODMCB,11,CSEXCSHF                                      
                                                                                
JOB60    DS    0H                                                               
*&&UK                                                                           
         CLC   CSBILCUR,CSCPYCUR                                                
         BE    JOB61                                                            
         LA    R2,BOWORK1                                                       
         USING EURKBLKD,R2                                                      
         XC    0(EURKBLKL,R2),0(R2)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         ZAP   BODUB1,PL8                                                       
         GOTO1 VEUREKA,BODMCB,('APPLYQ',BOWORK1),BODUB1,PL8                     
         DROP  R2                                                               
*&&                                                                             
                                                                                
JOB61    GOTO1 ASNDDATA,BODMCB,9,PL8                                            
                                                                                
JOB62    MVC   IOKEY,KEYSAVE                                                    
         SR    RF,RF                                                            
         IC    RF,BCJOBLEN                                                      
         LA    RF,A.ACTKACT(RF)      ALWAYS BUMP TO NEXT JOB                    
         MVI   0(RF),FF                                                         
         B     JOB10                                                            
                                                                                
JOB100   DS    0H                                                               
         B     EXITY                                                            
         DROP  A                                                                
         POP   USING                                                            
***********************************************************************         
* SEND EXCHANGE RATE & BALANCE AFTER CURRENCY CODE RECEIVE            *         
***********************************************************************         
                                                                                
SNDRATE  EQU   *                                                                
         GOTO1 ASNDHDR,BODMCB,09              SENDING JOB INFO                  
         GOTO1 ASNDDATA,BODMCB,5,CSEXCRAT     SEND NEW RATE                     
         GOTO1 (RF),(R1),9,PL8                NEW BALANCE AND DEC PL            
         GOTO1 (RF),(R1),10,CSCURBIL+(CURTDECP-CURTABD)                         
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* MAP TABLE                                                           *         
***********************************************************************         
         SPACE 1                                                                
MAPTAB   DS    0X                                                               
         SPACE 1                                                                
*                                  ** CLIENT PRODUCT LIST **                    
CPREL    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#CPL)         ELEMENT CODE                                 
         DC    AL2(CPRELX+1-CPREL) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDCPR-CLB53)   SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR CLIENT CODE                 
         DC    CL5'CLINT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'THISCLI)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVCLI-CLB53)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
CPRELX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                  ** JOB LIST **                               
JBKEL    DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#JBL)         ELEMENT CODE                                 
         DC    AL2(JBKELX+1-JBKEL) DISP TO NEXT ELEMENT HEADER                  
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDJBK-CLB53)   SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE FOR CLIENT CODE                 
         DC    CL5'CLINT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'THISCLI)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVCLI-CLB53)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE FOR PRODUCT CODE                
         DC    CL5'PROD.'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'THISPRO)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVPRO-CLB53)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE FOR PRODUCT CODE                
         DC    CL5'STAT '          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVSTA-CLB53)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
JBKELX   DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
CLIELN   DC    AL1(MHELDL)         HEADER LENGTH                                
         DC    AL2(07)             ELEMENT CODE                                 
         DC    AL2(CLIELNX+1-CLIELN) DISP TO NEXT ELEMENT HEADER                
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'CLINT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'CLNAM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ACNAME)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
CLIELNX  DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
PROELN   DC    AL1(MHELDL)         HEADER LENGTH                                
         DC    AL2(08)             ELEMENT CODE                                 
         DC    AL2(PROELNX+1-PROELN) DISP TO NEXT ELEMENT HEADER                
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'PRODT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(2)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'PRNAM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ACNAME)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
PROELNX  DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
JOBELN   DC    AL1(MHELDL2)        HEADER LENGTH                                
         DC    AL2(MH#JDT)         ELEMENT CODE                                 
         DC    AL2(JOBELNX+1-JOBELN) DISP TO NEXT ELEMENT HEADER                
         DC    AL1(0)              INDICATORS                                   
         DC    AL1(0,0)            ELEMENT CODE/LENGTH                          
         DC    AL2(0)              FIRST FOR ELEMENT RECEIVE                    
         DC    AL2(0)              LAST FOR ELEMENT RECEIVE                     
         DC    AL2(SNDRATE-CLB53)  SEND ROUTINE                                 
         DC    AL2(0)              SEND ELEMENT ROUTINE                         
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(01)             MAPPING CODE                                 
         DC    CL5'JOBDA'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'ACCKDA)       DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOBDA-CLB53) RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(02)             MAPPING CODE                                 
         DC    CL5'JOB'            TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVJOB-CLB53)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(03)             MAPPING CODE                                 
         DC    CL5'JOBNM'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(MDLENV)         DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(04)             MAPPING CODE                                 
         DC    CL5'CRNCY'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CSBILCUR)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVCUR-CLB53)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(05)             MAPPING CODE                                 
         DC    CL5'EXCHR'          TEXT IDENTIFIER                              
         DC    AL1(MDTHXQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CSEXCRAT)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(06)             MAPPING CODE                                 
         DC    CL5'FRMAT'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PBCKFMT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(RCVFMT-CLB53)   RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(07)             MAPPING CODE                                 
         DC    CL5'NET  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'SCIAMNT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(08)             MAPPING CODE                                 
         DC    CL5'COMM '          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'SCIAMNT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(09)             MAPPING CODE                                 
         DC    CL5'BAL  '          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'PL8)          DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(10)             MAPPING CODE                                 
         DC    CL5'CURDP'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CURTDECP)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(11)             MAPPING CODE                                 
         DC    CL5'CURSH'          TEXT IDENTIFIER                              
         DC    AL1(MDTBIQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'CSEXCSHF)     DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(0)              INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(12)             MAPPING CODE                                 
         DC    CL5'WOPEN'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'SCIAMNT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(13)             MAPPING CODE                                 
         DC    CL5'XFRPN'          TEXT IDENTIFIER                              
         DC    AL1(MDTCAQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(L'SCIAMNT)      DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
         DC    AL1(MDELDL2)        ITEM LENGTH                                  
         DC    AL2(14)             MAPPING CODE                                 
         DC    CL5'BSTAT'          TEXT IDENTIFIER                              
         DC    AL1(MDTCHQ)         DATA TYPE (CHARACTER)                        
         DC    AL1(1)              DATA LENGTH                                  
         DC    AL1(0)              DATA DISPLACEMENT                            
         DC    AL1(MDINNULL)       INDICATORS                                   
         DC    AL2(0)              RECEIVE ROUTINE                              
         DC    AL2(0)              SEND ROUTINE                                 
         DC    AL1(0,0,0)                                                       
*                                                                               
JOBELNX  DC    XL1'00'             END-OF-ELEMENT-FIELDS                        
*                                                                               
MAPTABX  DC    X'00'               E-O-T                                        
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
         DS    (L'OSVALS-(*-OSVALS))X                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
         SPACE 1                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
LINKD    DSECT                                                                  
         ORG   OVRWS                                                            
SAVERF   DS    F                                                                
THISCLI  DS    CL6                                                              
THISPRO  DS    CL6                                                              
THISJOB  DS    CL(L'TRNKACT)                                                    
KEYSAVE  DS    XL(L'IOKEY)                                                      
PL8      DS    PL8                                                              
JOBDA    DS    XL(L'ACCKDA)                                                     
JOBSTAT  DS    XL1                                                              
         DS    (L'OVRWS-(*-OVRWS))X                                             
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093ACCLB53   08/16/00'                                      
         END                                                                    
