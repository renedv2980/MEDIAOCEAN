*          DATA SET SPREPXA02  AT LEVEL 096 AS OF 03/24/15                      
*PHASE SPXA02A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'SPXA02 - ATT FILE DATA TRANSFER'                                
         SPACE 1                                                                
*=================================================================*             
* PROBLEMS REQUIRING ATTENTION                                    *             
* THERE MAY BE SOME PRODUCTS THAT HAVE NO AOR                     *             
* MAYBE WANT TO SET AOR = FFFFFF TO SUPPRESS AOR ENTRY            *             
*=================================================================*             
         SPACE 2                                                                
SPXA02   CSECT                                                                  
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPXA02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         LA    R8,2048(RC)                                                      
         LA    R8,2048(R8)                                                      
         USING SPXA02,RB,RC,R8                                                  
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    XA10                                                             
         CLI   MODE,CLTFRST                                                     
         BE    XA30                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=========================================================*                     
* RUNFRST PROCESSING                                      *                     
*=========================================================*                     
         SPACE 1                                                                
XA10     DS    0H                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         OPEN  (FILEOUT,(OUTPUT))                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         B     XA11                                                             
SORTCARD DC    CL80'SORT FIELDS=(5,13,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2004'                                  
*                                                                               
XA11     L     R4,=A(AGYTAB)       GET AGENCY TABLE ADDRESS                     
         USING AGYTABD,R4                                                       
*                                                                               
XA12     DS    0H                                                               
         GOTO1 CLPACK,DMCB,AGYATCLT,AGYATCLB                                    
         GOTO1 (RF),(R1),AGYAGCLT,AGYAGCLB                                      
         LA    R4,AGYTABL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   XA12                                                             
         DROP  R4                                                               
*                                                                               
         L     R4,=A(AORTAB)                                                    
         USING AORTABD,R4                                                       
XA14     DS    0H                                                               
         GOTO1 CLPACK,DMCB,AORATCLT,AORATCLB                                    
         GOTO1 (RF),(R1),AORAGCLT,AORAGCLB                                      
         LA    R4,AORTABL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   XA14                                                             
         DROP  R4                                                               
*                                                                               
         L     R4,=A(CLTTAB)                                                    
         USING CLTTABD,R4                                                       
XA16     DS    0H                                                               
         GOTO1 CLPACK,DMCB,CLTATCLT,CLTATCLB                                    
*                                                                               
         LA    R5,CLTATLST                        LIST OF RELATED CLTS          
         LA    R6,(CLTATLSX-CLTATLST)/L'CLTATLST  MAX ENTRIES                   
XA18     DS    0H                                                               
         GOTO1 CLPACK,DMCB,(R5),3(R5)                                           
         LA    R5,L'CLTATLST(R5)                                                
         CLI   0(R5),0                                                          
         BE    XA20                                                             
         BCT   R6,XA18                                                          
*                                                                               
XA20     LA    R4,CLTTABL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   XA16                                                             
*                                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*=======================================================*                       
* CLTFRST PROCESSSING                                   *                       
*=======================================================*                       
         SPACE 1                                                                
XA30     DS    0H                                                               
         BAS   RE,BLDSTA           BUILD ATT STATION BY MARKET LIST             
*                                                                               
         BAS   RE,BLDMKT           BUILD ATT MKT TO RTGSVC MKT LIST             
*                                                                               
         BAS   RE,BLDATT           READ ALL ATT CLT/PRD/EST/CML RECS            
         SPACE 1                                                                
*================================================================*              
* VERIFY THAT EVERY PRODUCT IN ALL ATT CLTHDRS APPEARS IN        *              
* AORTAB. IF NOT PRINT ERROR MESSAGE AND CONTINUE                *              
*================================================================*              
         SPACE 1                                                                
* SORT AORTAB ON ATT CLT/ATT PRDB                                               
*                                                                               
         GOTO1 XSORT,DMCB,AORTAB,AORTABCT,AORTABL,3,11                          
         XC    WORK,WORK                                                        
*                                                                               
         L     R4,=A(CLTTAB)                                                    
         USING CLTTABD,R4                                                       
*                                                                               
XA32     L     R7,CLTCLTAD         POINT TO CLTHDR                              
         LA    R7,CLIST-CLTHDR(R7)                                              
*                                                                               
XA34     MVC   WORK+11(2),CLTATCLB   SET UP SEARCH CLT                          
         MVC   WORK+13(1),3(R7)      AND PRODUCT                                
         GOTO1 BINSRCH,AORPARMS,WORK                                            
         CLI   0(R1),1               TEST NOT FOUND                             
         BNE   XA36                                                             
*                                                                               
         MVC   ERRMSG(40),=C'** ERROR ** NO A-O-R TABLE ENTRY FOR PRD'          
         MVC   ERRMSG+41(3),0(R7)                                               
         GOTO1 REPORT                                                           
*                                                                               
XA36     LA    R7,4(R7)                                                         
         CLI   3(R7),X'FF'         IGNORE POL                                   
         BE    *-8                                                              
         CLI   0(R7),C'A'                                                       
         BNL   XA34                                                             
*                                                                               
         LA    R4,CLTTABL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   XA32                                                             
         DROP  R4                                                               
         EJECT                                                                  
*============================================================*                  
* PROCESS INPUT TAPE                                         *                  
*============================================================*                  
         SPACE 1                                                                
XA40     DS    0H                                                               
*                                                                               
         MVI   TOTIND,C'-'         SET FLAG FOR DELETING RECORDS                
*                                                                               
XA41     L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         GET   FILEIN,(R0)                                                      
         AP    INCNT,=P'1'                                                      
* SET 2X'00' AT EOR                                                             
         L     RE,ADBUY                                                         
         SH    RE,=H'4'            POINT TO RECLEN                              
         AH    RE,0(RE)            POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       CLEAR ELCODE AND LEN                         
*                                                                               
         L     R2,ADBUY                                                         
         USING GOALRECD,R2                                                      
*                                                                               
         CLI   0(R2),2             TEST GOALREC                                 
         BNE   XA42                                                             
         CLC   1(1,R2),BAGYMD      TEST AGENCY ATT                              
         BNE   XA48                NO - KEEP                                    
         MVI   ELCODE,X'21'        FIND REGULAR GOAL ELEMENTS                   
         LA    R6,GDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   XA46                DELETE (LOCKIN)                              
*                                                                               
         CLI   GDXFRAGY,0          DID I CREATE IT                              
         BE    XA48                NO - DEFINITELY KEEP                         
         LA    R1,2(R2)            POINT TO CLIENT                              
         BAS   RE,TESTCLT                                                       
         BE    XA48                CC EQ - CAME FROM OLD PROGRAM                
         B     XA46                ELSE DELETE                                  
         EJECT                                                                  
         USING BUYRECD,R2                                                       
XA42     OC    0(1,R2),0(R2)       TEST HEADER RECORD                           
         BZ    XA44                YES                                          
         CLC   0(1,R2),BAGYMD      TEST AGENCY ATT BUY                          
         BNE   XA48                NO                                           
         TM    15(R2),X'20'        TEST 'CLONED' FLAG                           
         BNO   XA48                NO - KEEP IT                                 
         BAS   RE,XATOT            ELSE EXTRACT DOLLAR VALUE                    
         B     XA46                AND DELETE IT                                
         SPACE 1                                                                
*=================================================================*             
* TEST TO DELETE HDR RECORDS CREATED BY THIS PROGRAM              *             
*=================================================================*             
         SPACE 1                                                                
XA44     TM    15(R2),X'20'        HDR REC - TEST 'CLONED' FLAG                 
         BO    XA46                YES - GET RID OF IT                          
         CLC   =X'FFFFFFFF',0(R2)  TEST FILE TRAILER REC                        
         BE    XA41                YES - SKIP                                   
         BAS   RE,SETCLT           IF CLT HEADER - SET INFO                     
         B     XA48                PUT TO OUTPUT                                
*                                                                               
XA46     AP    DELCNT,=P'1'        BUMP DELETE COUNTER                          
         CLI   QOPT2,C'Y'          TRACE DELETED RECORDS                        
         BNE   XA41                                                             
         GOTO1 MYTRACE,DMCB,ADBUY,0,=C'***DELETED RECORD**',19                  
         B     XA41                AND READ NEXT RECORD                         
*                                                                               
XA48     DS    0H                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
*                                                                               
XA49     DS    0H                                                               
         PUT   FILEOUT,(R0)        PUT ALL NON-ATT BUYS TO OUTPUT               
         AP    OUTCNT,=P'1'                                                     
         B     XA41                                                             
         EJECT                                                                  
*=========================================================*                     
* THIS CODE DISTINGUISHES NEW RECORDS FROM OLD RECORDS    *                     
* CREATED BY OLD VERSION OF TRANSFER                      &                     
*=========================================================*                     
         SPACE 2                                                                
TESTCLT  OC    0(2,R1),0(R1)       TEST MISSING CLIENT CODE                     
         BNZ   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         CLC   0(2,R1),=X'8273'    ATT COMES FIRST !                            
         BER   RE                                                               
         CLC   0(2,R1),=X'B05F'    MC                                           
         BER   RE                                                               
         CLC   0(2,R1),=X'B6DF'    NW                                           
         BER   RE                                                               
         CLC   0(2,R1),=X'B99F'    OM                                           
         BER   RE                                                               
         CLC   0(2,R1),=X'E23F'    YR                                           
         BER   RE                                                               
         BR    RE                  EXIT WITH CC NEQ                             
         EJECT                                                                  
*                                                                               
* IF THIS IS A CLIENT HEADER - GET INFO FROM MASTER CLIENT      *               
* AND COPY IT INTO THE NEW CLIENT HEADER                                        
*        R2 - NEW CLIENT HEADER                                                 
*                                                                               
         SPACE 1                                                                
SETCLT   NTR1                                                                   
         CLC   1(1,R2),BAGYMD      TEST AGENCY ATT BUY                          
         BNE   SCX                 YES                                          
         OC    4(9,R2),4(R2)       MAKE SURE THIS IS A CLT HEADER               
         BNZ   SCX                                                              
         L     R1,=A(CLTTAB)       FIND CLTTAB ENTRY                            
         USING CLTTABD,R1                                                       
*                                                                               
SC10     LA    R5,CLTATLST                                                      
         LA    R6,(CLTATLSX-CLTATLST)/L'CLTATLST                                
*                                                                               
SC20     CLC   2(2,R2),3(R5)       FIND CLIENT CODE ENTRY                       
         BE    SC40                                                             
         LA    R5,L'CLTATLST(R5)   BUMP TO NEXT CLT                             
         CLC   0(3,R5),SPACES                                                   
         BE    SC30                                                             
         BCT   R6,SC20                                                          
*                                                                               
SC30     LA    R1,CLTTABL(R1)      BUMP TO NEXT MASTER CLIENT                   
         CLI   0(R1),X'FF'         IF CLIENT NOT IN TABLE                       
         BNE   SC10                                                             
         B     SCX                 KEEP IT                                      
*                                                                               
SC40     L     R1,CLTCLTAD         A(MASTER CLIENT HEADER)                      
         LA    R0,60(R2)           SET 'TO' ADDR                                
         LA    RE,60(R1)           SET 'FROM' ADDR                              
         LA    R1,940              SET 'TO' LEN                                 
         LR    RF,R1               SET 'FROM' LEN                               
         MVCL  R0,RE                                                            
         CLI   QOPT3,C'Y'          TRACE OUTPUT TO TAPE                         
         BNE   SCX                                                              
         GOTO1 MYTRACE,DMCB,ADBUY,0,=C'**AOR CLIENT HDR***',19                  
*                                                                               
SCX      B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
*===============================================================*               
* NOW PROCESS DATA FROM OTHER SPTFILES                          *               
* EACH BUY RECORD WILL GENERATE TWO RECORDS ON THE ATT FILE     *               
* ONE RECORD UNDER AN ATT CLIENT CODE                           *               
* AND ONE UNDER THE CLIENT CODE FOR THE AGENCY OF RECORD        *               
*===============================================================*               
         SPACE 1                                                                
XA50     DS    0H                                                               
*                                                                               
         BAS   RE,XAAGYTOT          PRINT AGENCY TOTALS                         
         CLOSE FILEIN               AND CLOSE INPUT TAPE                        
*                                                                               
         L     R2,ADBUY                                                         
         MVI   0(R2),X'FF'         SET EOF REC                                  
         BAS   RE,XATOT            FORCE LAST CLT TO BUFFER                     
         EJECT                                                                  
*===============================================================*               
* PROCESS ALL THE AGENCIES/CLIENTS IN THE AGYTAB LIST           *               
*===============================================================*               
         SPACE 1                                                                
XA60     DS    0H                  CLOSE PREVIOUS SPOT SYSTEM                   
         L     R4,=A(AGYTAB)       POINT TO AGY/CLT LIST                        
         USING AGYTABD,R4                                                       
*                                                                               
XA61     L     RF,=A(CLTTAB)       FIND CLTTAB ENTRY                            
         USING CLTTABD,RF                                                       
*                                                                               
XA62     CLC   AGYATCLT,CLTATCLT   MATCH ATT CLIENT CODES                       
         BE    XA64                                                             
         LA    RF,CLTTABL(RF)                                                   
         CLI   0(RF),X'FF'                                                      
         BNE   XA62                                                             
         DC    H'0'                INCONSISTENT TABLES                          
*                                                                               
XA64     ST    R4,SVAGYTAB         SAVE AGYTAB ENTRY ADDRESS                    
         ST    RF,SVCLTTAB         SAVE CLTTAB ENTRY ADDRESS                    
         MVC   SVCLTB,CLTATCLB     SAVE CLIENT CODE ON ATT FILE                 
         MVC   SVPOLEST,CLTATPOL   SAVE POL EST TABLE ADDRESS                   
         MVC   SVPRDEST,CLTATEST   SAVE PRD EST TABLE ADDRESS                   
         MVC   SVCMLTAB,CLTCMLAD   SET COMMERCIAL TABLE ADDRESS                 
         DROP  RF                                                               
*                                                                               
XA65     DS    0H                                                               
         CLC   AGYAGY,SVAGYA       TEST SAME AGY AS PREVIOUS                    
         BE    XA70                YES - SKIP OPEN/CLOSE                        
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT'                                 
*                                                                               
         XC    CNDATA,CNDATA                                                    
         LA    R5,CNDATA                                                        
         USING CND,R5                                                           
         MVC   CNAGY(2),AGYAGY     MOVE ALPHA AGENCY CODE                       
*                                                                               
         GOTO1 CONFID,DMCB,(R5),(1,FULL)                                        
         OC    FULL,FULL           TEST ID FOUND                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,UTL                                                           
         MVC   4(1,RE),CNSSE       MOVE SPOT SYSTEM NUMBER                      
         MVC   SVAGYB,CNSCD        SAVE SPOT AGENCY NUMBER                      
         MVC   SVAGYA,AGYAGY       SAVE ALPHA AGENCY CODE                       
         DROP  R5                                                               
*                                                                               
* NOW TEST TO OPEN NEW TRAFFIC FILES                                            
* IF THEY EXIST OPEN THEM. IF THEY DON'T, GO AWAY                               
*                                                                               
         MVI   FTRFDIR,C' '        DEFAULT TO OPEN NEW TRAFFIC                  
         MVI   FTRFFIL,C' '                                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'SYSFLES',0                            
         ICM   RE,15,DMCB+12       A(FILE LIST FOR THIS SPOT SYSTEM)            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         ICM   R1,3,2(RE)          NUMBER OF FILES IN SYSTEM                    
         LA    RE,4(RE)            BUMP TO FIRST FILE IN LIST                   
*                                                                               
XA66     CLI   3(RE),X'33'         STRFDR?                                      
         BE    XA67                YES -- SEE IF IT'S NOP                       
         LA    RE,8(RE)            NO, TRY NEXT FILE                            
         BCT   R1,XA66                                                          
         B     XA68                NO TRAFFIC FILES -- REMOVE FROM LIST         
*                                                                               
XA67     TM    0(RE),X'80'         ARE TRAFFIC FILES NOP                        
         BZ    XA69                NO                                           
*                                                                               
XA68     MVI   FTRFDIR,C'Z'        YES -- TAKE THEM OUT OF THE LIST             
         MVI   FTRFFIL,C'Z'                                                     
*                                                                               
XA69     DS    0H                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLIST,ADBUY                     
*                                                                               
         XC    LASTMKT,LASTMKT                                                  
         BAS   RE,BLDAGMKT         BUILD AGENCY TO RTGSVC MARKET TAB            
*                                                                               
         EJECT                                                                  
XA70     XC    KEY,KEY                                                          
         IC    R0,SVAGYB                                                        
         STC   R0,KEY+1            SET AGENCY IN KEY (LEFT ALIGN)               
         OI    KEY+1,X'01'         SET MEDIA = SPOT TV                          
         MVC   KEY+2(2),AGYAGCLB                                                
         GOTO1 HIGH                READ CLTHDR ON AGY FILE                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,ADCLT                                                         
         ST    R5,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         BAS   RE,BLDPRD           BUILD PRODUCT TRANSLATE TABLE                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),1(R5)      A-M/CLT                                      
         MVC   SVCMLTAB,=A(AGCMLTB)                                             
         BAS   RE,BLDCML           BUILD COMMERCIAL TRANSLATE TABLE             
*                                                                               
* FOLLOWING DROP MEANS NO MORE AGYTAB REFERENCES                                
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*==============================================================*                
* READ GOAL RECORDS FROM AGENCY FILE                           *                
*==============================================================*                
         SPACE 1                                                                
XA80     OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         NI    DMOUTBTS,X'FD'      AND IGNORE ERROR IF DELETED                  
*                                                                               
         L     R5,ADCLT                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+1(3),1(R5)      SET A-M/CLT                                  
         GOTO1 HIGH                                                             
         B     XA84                                                             
*                                                                               
XA82     GOTO1 SEQ                                                              
*                                                                               
XA84     CLC   KEY(4),KEYSAVE      SAME TY/A-M/CLT                              
         BNE   XA90                                                             
*                                                                               
         L     R2,ADGOAL                                                        
         USING GOALRECD,R2                                                      
*                                                                               
         ST    R2,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         TM    15(R2),X'80'        TEST DELETED                                 
         BO    XA82                                                             
*                                                                               
         ZIC   R0,GOALREC+7        GET ESTIMATE NUMBER                          
         LA    R7,GOALREC+4        POINT TO PRD 1                               
         BAS   RE,XAEST            VALIDATE POL EST OPEN                        
         BNE   XA82                IF NO ATT POL EST, SKIP REC                  
*                                                                               
         MVI   ELCODE,X'21'        FIND REGULAR GOAL ELEMENTS                   
         LA    R6,GDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    XA84A               IF REGELS, KEEP RECORD                       
         B     XA82                ELSE, SKIP RECORD                            
*                                                                               
XA84A    BAS   RE,XAGLMKT          TRANSLATE MKT NUMBER                         
*                                                                               
         LA    R7,GOALREC+4        POINT TO PRD 1                               
         BAS   RE,FINDPRD                                                       
         BNE   XA82                SKIP RECORD ON ERROR                         
*                                                                               
         LA    R7,GOALREC+12       POINT TO PRD 2                               
         CLI   0(R7),0                                                          
         BE    *+12                                                             
         BAS   RE,FINDPRD                                                       
         BNE   XA82                SKIP RECORD ON ERROR                         
         EJECT                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,13(R2)         GET REC LENGTH                               
         AH    R0,=H'4'                                                         
         L     R4,ADGOAL                                                        
         SH    R4,=H'4'                                                         
         SLL   R0,16               LEFT ALIGN                                   
         ST    R0,0(R4)            SET REC LENGTH FOR SORT                      
         SPACE 1                                                                
*============================================================*                  
* GENERATE ATT CLIENT RECORD                                 *                  
*============================================================*                  
         SPACE 1                                                                
         CLI   QOPT4,C'Y'          TRACE BEFORE & AFTER RECORDS                 
         BNE   XA85                                                             
         GOTO1 MYTRACE,DMCB,4(R4),0,=C'OLD ATT CLIENT RECORD',21                
*                                                                               
XA85     BAS   RE,BLDCLONE         BUILD CLONED ELEMENT                         
         OI    15(R2),X'20'        MARK REC CLONED                              
         BAS   RE,ADELEM           ADD CLONED ELEMENT                           
*                                                                               
         MVC   1(1,R2),BAGYMD        MOVE ATT AGY/MED TO REC                    
         MVC   2(2,R2),SVCLTB        SET ATT CLIENT CODE                        
         MVC   GOALREC+20(2),=C'AT'  AGYALPHA BECOMES ATT                       
*                                                                               
         MVI   ELCODE,X'21'        FIND REGULAR GOAL ELEMENTS                   
         LA    R6,GDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   XA85A                                                            
         MVC   GDXFRAGY,XFRAGY       SET TRANSFER AGY NUMBER                    
*                                                                               
XA85A    CLI   QOPT4,C'Y'            TRACE BEFORE & AFTER RECORDS               
         BNE   XA86                                                             
         GOTO1 MYTRACE,DMCB,4(R4),0,=C'NEW ATT CLIENT RECORD',21                
*                                                                               
XA86     BAS   RE,PUTSORT                                                       
         SPACE 1                                                                
*============================================================*                  
* GENERATE AOR CLIENT RECORD                                 *                  
*============================================================*                  
         SPACE 1                                                                
         MVC   WORK+11(3),GOALREC+2  KEY IS CLT(2)/PRD(1)                       
         GOTO1 BINSRCH,AORPARMS,WORK                                            
         CLI   0(R1),1               TEST NOT FOUND                             
         BE    XA82   <============  DO NOT BLOW UP !                           
*                                                                               
         L     RE,0(R1)                                                         
         USING AORTABD,RE                                                       
         CLC   AORAGCLB,AORATCLB   IF THE CLIENT CODE IS THE SAME               
         BE    XA82                DON'T PUT OUT AOR RECORD                     
         MVC   GOALREC+2(2),AORAGCLB-AORTABD(RE)  MOVE AOR CLT                  
         DROP  RE                                                               
*                                                                               
         CLI   QOPT4,C'Y'            TRACE BEFORE & AFTER RECORDS               
         BNE   XA88                                                             
         GOTO1 MYTRACE,DMCB,4(R4),0,=C'NEW AOR CLIENT RECORD',21                
*                                                                               
XA88     BAS   RE,PUTSORT                                                       
         CLI   QOPT1,C'Y'          SEE IF TEST RUN                              
         BNE   XA82                                                             
         CP    AGYCNT,=P'100'                                                   
         BL    XA82                                                             
         EJECT                                                                  
*=============================================================*                 
* READ POL BUY RECORDS FOR THIS ATT CLIENT CODE               *                 
*=============================================================*                 
         SPACE 1                                                                
XA90     XC    KEY,KEY                                                          
         MVC   KEY(3),1(R5)        SET A-M/CLT                                  
         MVI   KEY+3,X'FF'         SET PRD=POL                                  
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BE    XA94                                                             
         B     XA96                SKIP IF NO BUYS                              
*                                                                               
XA92     GOTO1 SEQ                                                              
*                                                                               
XA94     CLC   KEY(3),KEYSAVE      SAME A-M/CLT                                 
         BNE   XA96                                                             
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         ST    R2,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         TM    15(R2),X'80'        TEST DELETED                                 
         BO    XA92                YES - IGNORE                                 
         CLC   KEY+4(2),BUYREC+4   TEST SPILL POINTER                           
         BNE   XA92                                                             
*                                                                               
         ZIC   R0,BUYREC+9          GET ESTIMATE NUMBER                         
*****    LA    R7,BUYREC+3          POINT TO PRD *****                          
         LA    R7,BDMASPRD                                                      
         BAS   RE,XAEST             VALIDATE POL EST OPEN                       
         BNE   XA92                                                             
*                                                                               
         CLI   QOPT4,C'Y'          TRACE BEFORE & AFTER RECORDS                 
         BNE   XA95                                                             
         L     R4,ADBUY                                                         
         SH    R4,=H'4'                                                         
         GOTO1 MYTRACE,DMCB,4(R4),0,=C'OLD RECORD FOR ATT CLIENT',25            
*                                                                               
XA95     BAS   RE,XAMKT            TRANSLATE MKT NUMBER                         
*                                                                               
         BAS   RE,XAPRD            TRANSLATE PRD CODES                          
         TM    ERRSW,X'80'         TEST PRD ERR - IGNORE EST ERROR              
         BNZ   XA92                YES - SKIP RECORD                            
*                                                                               
         BAS   RE,XACML            TRANSLATE CMML CODES                         
*                                                                               
         BAS   RE,BLDCLONE         BUILD CLONED ELEMENT                         
         OI    15(R2),X'20'        MARK BUY CLONED                              
*                                                                               
         MVC   BDXFRAGY,XFRAGY     SET TRANSFER FLAG IN BUYREC                  
         MVC   BUYALPHA,=C'AT'     AGYALPHA BECOMES ATT                         
         EJECT                                                                  
*=============================================================*                 
* GENERATE RECORD FOR ATT CLIENT                              *                 
*=============================================================*                 
         SPACE 1                                                                
         MVC   0(1,R2),BAGYMD      MOVE ATT AGY/MED TO REC                      
         MVC   1(2,R2),SVCLTB      SET ATT CLIENT CODE                          
*                                                                               
         BAS   RE,ADELEM           ADD CLONED ELEMENT                           
*                                                                               
         CLI   QOPT4,C'Y'          TRACE BEFORE & AFTER RECORDS                 
         BNE   XA95A                                                            
         GOTO1 MYTRACE,DMCB,4(R4),0,=C'NEW RECORD FOR ATT CLIENT',25            
*                                                                               
XA95A    BAS   RE,PUTSORT                                                       
         SPACE 1                                                                
*=============================================================*                 
* GENERATE RECORD FOR AOR CLIENT                              *                 
* SINCE BUYING IS BRAND POL, LAST PRD GIVES AOR               *                 
*=============================================================*                 
         SPACE 1                                                                
         MVC   WORK+11(2),BUYREC+1    KEY IS CLIENT(2)/PRD(1)                   
         MVC   WORK+13(1),SVPRDB      USE AOR OF LAST PRD TRANSLATED            
         GOTO1 BINSRCH,AORPARMS,WORK                                            
         CLI   0(R1),1                TEST NOT FOUND                            
         BE    XA92   <===========    DO NOT BLOW UP !!                         
*                                                                               
         L     RE,0(R1)                                                         
         USING AORTABD,RE                                                       
         CLC   AORAGCLB,AORATCLB   IF THE CLIENT CODE IS THE SAME               
         BE    XA92                DON'T PUT OUT AOR RECORD                     
         MVC   BUYREC+1(2),AORAGCLB-AORTABD(RE)  MOVE AOR CLT                   
         DROP  RE                                                               
*                                                                               
         CLI   QOPT4,C'Y'          TRACE BEFORE & AFTER RECORDS                 
         BNE   XA95C                                                            
         GOTO1 MYTRACE,DMCB,4(R4),0,=C'NEW RECORD FOR AOR CLIENT',25            
*                                                                               
XA95C    BAS   RE,PUTSORT                                                       
         CLI   QOPT1,C'Y'          SEE IF TEST RUN                              
         BNE   XA92                                                             
         CP    AGYCNT,=P'200'                                                   
         BNH   XA92                                                             
*                                                                               
XA96     DS    0H                                                               
         BAS   RE,XAAGYTOT                                                      
*                                                                               
XA97     L     R4,SVAGYTAB                                                      
         LA    R4,AGYTABL(R4)      NEXT ENTRY                                   
         CLI   0(R4),X'FF'         TEST EOL                                     
         BNE   XA61                                                             
         EJECT                                                                  
*==============================================================*                
* ALL AGENCY FILES HAVE BEEN PROCESSED                         *                
* SORT RECORDS AND WRITE TO TAPE                               *                
*==============================================================*                
         SPACE 1                                                                
XA100    MVI   TOTIND,C'+'         INDICATE ADDING RECORDS                      
*                                                                               
XA102    DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BZ    XA122                                                            
*                                                                               
         LR    R0,R2                                                            
         PUT   FILEOUT,(R0)                                                     
         AP    OUTCNT,=P'1'                                                     
         AP    ADDCNT,=P'1'                                                     
* MOVE RECORD *                                                                 
         L     R1,ADBUY                                                         
         SH    R1,=H'4'                                                         
         LH    RE,0(R2)            GET LENGTH                                   
XA105    CH    RE,=H'256'                                                       
         BNH   XA106                                                            
         MVC   0(256,R1),0(R2)                                                  
         LA    R1,256(R1)                                                       
         LA    R2,256(R2)                                                       
         SH    RE,=H'256'                                                       
         B     XA105                                                            
XA106    BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)  *EXECUTED*                                        
*                                                                               
         LA    R1,1(RE,R1)         POINT TO END OF REC                          
         XC    0(2,R1),0(R1)       CLEAR NEXT ELEM CODE/LENGTH                  
*                                                                               
         BAS   RE,XATOT                                                         
         B     XA102                                                            
*                                                                               
XA122    DS    0H                  PRINT FINAL TOTALS                           
         CLOSE FILEOUT                                                          
         EJECT                                                                  
         L     R2,ADBUY                                                         
         MVI   0(R2),X'FF'         PASS EOF TO TOT ROUTINES                     
         BAS   RE,XATOT                                                         
*                                                                               
         BAS   RE,XATOTPRT                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,CTRS                                                          
*                                                                               
XA124    MVC   P(20),4(R4)                                                      
         EDIT  (P4,0(R4)),(8,P+22)                                              
         GOTO1 REPORT                                                           
         LA    R4,L'CTRS(R4)                                                    
         LA    R0,CTRX                                                          
         CR    R4,R0                                                            
         BL    XA124                                                            
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* READ CLIENT, PRODUCT, AND ESTIMATE HEADERS FOR EACH ATT CLIENT  *             
* CLIENT HEADERS ARE SAVED IN CLTHDR SAVE AREA                    *             
* CLT/PRD/EST HDRS ARE COPIED TO RELATED AOR CLIENT CODES         *             
*                                                                 *             
* TWO ESTIMATE TABLES ARE CONSTRUCTED FOR EACH CLIENT             *             
* CLTATPOL CONTAINS THE POL ESTIMATE DATES                        *             
* CLTATEST CONTAINS A FLAG FOR EACH OPEN BRAND ESTIMATE           *             
*                                                                               
* AORTAB ENTRIES ARE COMPLETED WITH BINARY PRODUCT CODES          *             
*=================================================================*             
         SPACE 1                                                                
BLDATT   NTR1                                                                   
         L     R4,=A(CLTTAB)                                                    
         USING CLTTABD,R4                                                       
*                                                                               
BLDATT2  DS    0H                                                               
         L     R1,CLTATPOL               CLEAR THE POL EST TABLE                
         L     R0,=A(CCSPOLTX-CCSPOLTB)  USE LENGTH OF FIRST                    
         BAS   RE,CLEAR                                                         
*                                                                               
         L     R1,CLTATEST               CLEAR THE BRAND EST TABLE              
         L     R0,=A(CCSESTTX-CCSESTTB)                                         
         BAS   RE,CLEAR                                                         
*                                                                               
         L     R1,CLTCMLAD               GET ADDRESS OF CML TABLE               
         L     R0,=A(CCSCMLTX-CCSCMLTB)  USE LENGTH OF CCS TABLE                
         BAS   RE,CLEAR                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD     A-M                                          
         MVC   KEY+2(2),CLTATCLB   CLT                                          
         GOTO1 HIGH                READ CLTHDR                                  
         MVC   AREC,ADCLT          READ TO EXPECTED LOCATION                    
         GOTO1 GET                                                              
* COPY CLTHDR TO SAVE AREA                                                      
         L     R0,CLTCLTAD         SET 'TO' ADDR                                
         LA    R1,1280             SET 'TO' LEN                                 
         L     RE,AREC             SET 'FROM' ADDR                              
         LR    RF,R1               SET 'FROM' LEN                               
         MVCL  R0,RE                                                            
*                                                                               
*****    L     R0,=A(XAIO)         SET 'TO' ADDR                                
*****    LA    R1,1280             SET 'TO' LEN                                 
*****    L     RE,AREC             SET 'FROM' ADDR                              
*****    LR    RF,R1               SET 'FROM' LEN                               
*****    MVCL  R0,RE                                                            
*****    BAS   RE,PUTHDR                                                        
         EJECT                                                                  
BLDATT10 GOTO1 SEQ                                                              
*                                                                               
BLDATT12 CLC   KEY(4),KEYSAVE      SAME A-M/CLT                                 
         BNE   BLDATT30                                                         
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   BLDATT10                                                         
*                                                                               
BLDATT14 DS    0H                                                               
         L     RE,=A(XAIO)                                                      
         ST    RE,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         BAS   RE,PUTHDR                                                        
*                                                                               
         OC    KEY+7(6),KEY+7      TEST PRODUCT HEADER                          
         BZ    BLDATT10            YES                                          
*                                                                               
         CLC   =C'POL',KEY+4       TEST POL ESTIMATE                            
         BE    BLDATT20                                                         
*                                                                               
         L     R1,ADCLT                                                         
         LA    R1,CLIST-CLTHDRD(R1)                                             
*                                                                               
BLDATT16 CLC   KEY+4(3),0(R1)      TRANSLATE PRODUCT CODE                       
         BE    BLDATT18                                                         
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   BLDATT16                                                         
         DC    H'0'                                                             
*                                                                               
BLDATT18 ZIC   RE,3(R1)            GET PRD NUMBER                               
         SLL   RE,8                X 256                                        
         A     RE,CLTATEST         ADD A(BRAND EST TABLE)                       
         ZIC   R0,KEY+7            GET EST NUMBER                               
         AR    RE,R0               INDEX TO CORRECT BYTE                        
         MVC   0(1,RE),KEY+7       MOVE EST NUMBER TO TABLE                     
         B     BLDATT10                                                         
         EJECT                                                                  
*========================================================*                      
* READ POL ESTHDR TO GET DATES                           *                      
*========================================================*                      
         SPACE 1                                                                
BLDATT20 L     R6,AREC                                                          
         USING ESTHDRD,R6                                                       
         GOTO1 DATCON,DMCB,ESTART,(2,FULL)                                      
         GOTO1 (RF),(R1),EEND,(2,FULL+2)                                        
         ZIC   RE,KEY+7            GET ESTIMATE NUMBER                          
         SLL   RE,2                X 4                                          
         A     RE,CLTATPOL         ADD A(POL EST TABLE)                         
         MVC   0(4,RE),FULL        MOVE DATES                                   
         B     BLDATT10                                                         
         SPACE 1                                                                
*=============================================================*                 
* LOOK-UP BINARY PRODUCT CODES FOR THIS CLIENT TO COMPLETE    *                 
* AOR TABLE ENTRIES                                           *                 
*=============================================================*                 
         SPACE 1                                                                
BLDATT30 L     R1,=A(AORTAB)                                                    
         USING AORTABD,R1                                                       
*                                                                               
BLDATT32 CLC   AORATCLT,CLTATCLT   TEST RIGHT CLIENT                            
         BNE   BLDATT38                                                         
*                                                                               
         L     RE,CLTCLTAD           POINT TO CLTHDR                            
         LA    RE,CLIST-CLTHDRD(RE)  POINT TO PRD LIST                          
*                                                                               
BLDATT34 CLC   AORATPRD,0(RE)        FIND PRODUCT                               
         BE    BLDATT36                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   BLDATT34                                                         
         DC    H'0'                TABLE ERROR                                  
*                                                                               
BLDATT36 MVC   AORATPRB,3(RE)      MOVE BINARY CODE TO TABLE                    
*                                                                               
BLDATT38 LA    R1,AORTABL(R1)                                                   
         CLI   0(R1),X'FF'                                                      
         BNE   BLDATT32                                                         
         DROP  R1                                                               
         EJECT                                                                  
*===========================================================*                   
* BUILD LIST OF COMMERCIALS FOR THIS CLIENT                 *                   
*===========================================================*                   
         SPACE 1                                                                
BLDATT40 MVC   KEY(2),=X'0A21'                                                  
         L     RE,CLTCLTAD                                                      
         MVC   KEY+2(11),0(RE)     SET AGY/MED/CLT                              
         MVC   SVCMLTAB,CLTCMLAD                                                
         BAS   RE,BLDCML                                                        
         SPACE 1                                                                
*===========================================================*                   
* NEXT CLTTAB ENTRY                                         *                   
*===========================================================*                   
         SPACE 1                                                                
BLDATT42 LA    R4,CLTTABL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   BLDATT2                                                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*============================================================*                  
* COPY THE HEADER RECORD TO THE RELATED AOR CLIENT CODES     *                  
* THE RELATED CLIENT LIST IS IN THE CLT TABLE                *                  
* ALL RECORDS WRITTEN HERE HAVE X'20' STATUS BYTE ON AS A    *                  
* 'CLONED' FLAG. THIS BIT IS TESTED TO REMOVE THEM FROM THE  *                  
* INPUT TAPE THE FOLLOWING WEEK                              *                  
*============================================================*                  
         SPACE 1                                                                
PUTHDR   NTR1                                                                   
         LA    R5,CLTATLST         GET LIST OF RELATED CLIENTS                  
         LA    R6,(CLTATLSX-CLTATLST)/L'CLTATLST                                
*                                                                               
PUTHDR2  CLC   0(3,R5),SPACES                                                   
         BE    PUTHDRX                                                          
         L     R1,AREC                                                          
         OI    15(R1),X'20'        SET 'CLONED' STATUS BIT                      
         SR    R0,R0                                                            
         ICM   R0,3,13(R1)         GET RECORD LENGTH FROM RECORD                
         AH    R0,=H'4'                                                         
         SH    R1,=H'4'                                                         
         SLL   R0,16                                                            
         ST    R0,0(R1)                                                         
         LR    R0,R1               POINT R0 TO RECORD                           
*                                                                               
         L     RE,AREC                                                          
         MVC   2(2,RE),3(R5)       MOVE PACKED CLIENT TO RECORD                 
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   PUTHDR10                                                         
         GOTO1 MYTRACE,DMCB,AREC,0,=C'***HEADER TO TAPE**',19                   
*                                                                               
PUTHDR10 PUT   FILEOUT,(R0)                                                     
         AP    OUTCNT,=P'1'                                                     
*                                                                               
         LA    R5,L'CLTATLST(R5)                                                
         BCT   R6,PUTHDR2                                                       
*                                                                               
PUTHDRX  XIT1                                                                   
         EJECT                                                                  
*=============================================================*                 
* BUILD A TABLE IN CALL LETTER SEQUENCE OF 6 BYTE MKT/STA/AFF *                 
*=============================================================*                 
         SPACE 1                                                                
BLDSTA   NTR1                                                                   
         L     R4,=A(ATSTATAB)                                                  
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'ST'                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,ADSTAT                   
*                                                                               
         L     R6,ADSTAT                                                        
         USING STAMASTD,R6                                                      
         B     BLDSTA4                                                          
*                                                                               
BLDSTA2  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'STATION'                              
*                                                                               
BLDSTA4  CLC   =C'ST',0(R6)        STILL IN TV MASTER RECORDS                   
         BNE   BLDSTA10                                                         
         CLC   =C'AT',7(R6)        TEST AGENCY AT                               
         BNE   BLDSTA2                                                          
         L     R0,=A(ATSTATBX)                                                  
         CR    R4,R0               TEST PAST END OF TABLE                       
         BNL   STATBERR                                                         
*                                                                               
         MVC   DUB(5),2(R6)        MOVE STATION                                 
         MVC   DUB+5(3),=C'   '    AND FORCE CABLE NET TO SPACES                
         GOTO1 MSPACK,DMCB,SMKT,DUB,(R4)                                        
*                                                                               
         MVI   5(R4),C'A'          SET AFFILIATE IND                            
         CLC   =C'ABC',SNETWRK                                                  
         BE    BLDSTA6                                                          
         CLC   =C'CBS',SNETWRK                                                  
         BE    BLDSTA6                                                          
         CLC   =C'NBC',SNETWRK                                                  
         BE    BLDSTA6                                                          
         MVI   5(R4),C'I'          SET INDEPENDENT IND                          
*                                                                               
BLDSTA6  LA    R4,6(R4)                                                         
         BCTR  R5,0                BUMP COUNTER                                 
         B     BLDSTA2                                                          
*                                                                               
BLDSTA10 LPR   R5,R5                                                            
         ST    R5,ATSTACNT         SAVE STATION COUNT                           
         SPACE 1                                                                
         GOTO1 XSORT,DMCB,A(ATSTATAB),(R5),6,3,2                                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*================================================================*              
* BUILD A TABLE TO EQUATE ATT MARKET NUMBER TO ARB MARKET NUMBER *              
* ENTRY IS RTGSVC MKT/ATT MKT                                    *              
*================================================================*              
         SPACE 1                                                                
BLDMKT   NTR1                                                                   
         L     R4,=A(ATMKTTAB)                                                  
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'MT'                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,ADSTAT                   
*                                                                               
         L     R6,ADSTAT                                                        
         USING MKTRECD,R6                                                       
         B     BLDMKT4                                                          
*                                                                               
BLDMKT2  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'STATION'                              
*                                                                               
BLDMKT4  CLC   =C'MT',0(R6)        STILL IN TV MARKET RECORDS                   
         BNE   BLDMKT10                                                         
         CLC   =C'AT',6(R6)        TEST AGENCY AT                               
         BNE   BLDMKT2                                                          
         L     R0,=A(ATMKTTBX)                                                  
         CR    R4,R0               TEST PAST END OF TABLE                       
         BNL   MKTTBERR                                                         
*                                                                               
         PACK  DUB,MKTKMKT                                                      
         CVB   R0,DUB                                                           
         STH   R0,2(R4)                                                         
*                                                                               
         LA    R1,MKTRS1                                                        
         CLI   0(R1),C'1'          TEST ARB                                     
         BE    BLDMKT6                                                          
         LA    R1,MKTRS2                                                        
         CLI   0(R1),C'1'          TEST ARB                                     
         BNE   BLDMKT2                                                          
*                                                                               
BLDMKT6  OC    1(2,R1),1(R1)                                                    
         BZ    BLDMKT2             IGNORE IF 0                                  
         MVC   0(2,R4),1(R1)       MOVE RATING SVC MKT NUMBER                   
*                                                                               
         LA    R4,4(R4)                                                         
         BCTR  R5,0                BUMP COUNTER                                 
         B     BLDMKT2                                                          
*                                                                               
BLDMKT10 LPR   R5,R5                                                            
         ST    R5,ATMKTCNT         SAVE MARKET COUNT                            
         BZ    EXIT                                                             
         GOTO1 XSORT,DMCB,A(ATMKTTAB),ATMKTCNT,4,2,0                            
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*================================================================*              
* BUILD A TABLE TO EQUATE AGENCY MKT NUMBER TO ARB MARKET NUMBER *              
* ENTRY IS AGYMKT/RTGSVC MKT                                     *              
* SVAGYA CONTAINS ALPHA AGENCY CODE                              *              
*================================================================*              
         SPACE 1                                                                
BLDAGMKT NTR1                                                                   
         L     R4,=A(AGMKTTAB)                                                  
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'MT'                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,ADSTAT                   
*                                                                               
         L     R6,ADSTAT                                                        
         USING MKTRECD,R6                                                       
         B     BLDAGMK4                                                         
*                                                                               
BLDAGMK2 GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'STATION'                              
*                                                                               
BLDAGMK4 CLC   =C'MT',0(R6)        STILL IN TV MARKET RECORDS                   
         BNE   BLDAGMKX                                                         
         CLC   SVAGYA,6(R6)        TEST RIGHT AGENCY                            
         BNE   BLDAGMK2                                                         
         L     R0,=A(AGMKTTBX)                                                  
         CR    R4,R0               TEST PAST END OF TABLE                       
         BNL   MKTTBERR                                                         
*                                                                               
         PACK  DUB,MKTKMKT                                                      
         CVB   R0,DUB                                                           
         STH   R0,0(R4)                                                         
*                                                                               
         LA    R1,MKTRS1                                                        
         CLI   0(R1),C'1'          TEST ARB                                     
         BE    BLDAGMK6                                                         
         LA    R1,MKTRS2                                                        
         CLI   0(R1),C'1'          TEST ARB                                     
         BNE   BLDAGMK2                                                         
*                                                                               
BLDAGMK6 OC    1(2,R1),1(R1)                                                    
         BZ    BLDAGMK2                                                         
         MVC   2(2,R4),1(R1)       MOVE RATING SVC MKT NUMBER                   
         LA    R4,4(R4)                                                         
         BCTR  R5,0                BUMP COUNTER                                 
         B     BLDAGMK2                                                         
*                                                                               
BLDAGMKX LPR   R5,R5                                                            
         ST    R5,AGMKTCNT         SAVE MARKET COUNT                            
         BZ    EXIT                                                             
         GOTO1 XSORT,DMCB,A(AGMKTTAB),AGMKTCNT,4,2,0                            
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*====================================================================*          
* SET UP TRANSLATE TABLE BETWEEN PRODUCT CODES FOR                   *          
* CLTHDR ON AGENCY FILE (READ INTO ADCLT)                            *          
* AND CORRESPONDING ATT CLIENT CODE.                                 *          
* REMEMBER THAT SINCE THE AOR CLIENT CODES ARE CLONED FROM THE       *          
*  ATT CLIENT, ONLY ONE PRDTAB IS REQUIRED.                          *          
*====================================================================*          
         SPACE 1                                                                
BLDPRD   NTR1                                                                   
         L     R4,SVCLTTAB         GET CURRENT CLTTAB ENTRY ADDRESS             
         USING CLTTABD,R4                                                       
*                                                                               
         L     R1,=A(PRDTAB)       POINT TO PRD TRANSLATE TABLE                 
         XC    0(256,R1),0(R1)                                                  
*                                                                               
         L     RE,ADCLT            POINT TO AGY CLTHDR                          
         LA    RE,CLIST-CLTHDRD(RE)                                             
*                                                                               
BLDPRD2  L     RF,CLTCLTAD         POINT TO ATT CLTHDR                          
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
BLDPRD4  CLC   0(3,RE),0(RF)       MATCH ALPHA PRD CODE                         
         BE    BLDPRD6                                                          
         LA    RF,4(RF)                                                         
         CLI   0(RF),C'A'                                                       
         BNL   BLDPRD4                                                          
         B     BLDPRD10            IGNORE PRD NOT FOUND                         
*                                                                               
BLDPRD6  ZIC   R1,3(RE)            GET PRD NUM                                  
         A     R1,=A(PRDTAB)       POINT TO PROPER SLOT                         
         MVC   0(1,R1),3(RF)       MOVE ATT PRD CODE TO SLOT                    
*                                                                               
BLDPRD10 LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   BLDPRD2             GO BACK TO TOP IN CASE NOT FOUND             
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*==================================================================*            
* READ AND SAVE ALL COMMERCIALS AND SEQ NUMBERS FOR CLIENT IN 'KEY'*            
* ON ENTRY SVCMLTAB CONTAINS ADDRESS OF TABLE                      *            
* ON EXIT, SVCMLCNT CONTAINS NUMBER OF ENTRIES IN TABLE            *            
*==================================================================*            
         SPACE 1                                                                
BLDCML   NTR1                                                                   
         L     R4,SVCMLTAB                                                      
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         GOTO1 HIGH                                                             
         B     BLDCML4                                                          
*                                                                               
BLDCML2  GOTO1 SEQ                                                              
*                                                                               
BLDCML4  CLC   KEY(5),KEYSAVE      0A21/A-M/CLT                                 
         BNE   BLDCML10                                                         
*                                                                               
         L     R6,=A(XAIO)                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING CMLRECD,R6                                                       
*                                                                               
         MVC   0(8,R4),KEY+5       SAVE COMMERCIAL CODE                         
         MVC   8(2,R4),CMLSEQ+1    AND 2 BYTES OF CMML SEQ                      
         LA    R5,1(R5)            BUMP COUNTER                                 
         LA    R4,10(R4)           NEXT ENTRY                                   
         C     R5,=A(MAXCMLS)                                                   
         BL    BLDCML2                                                          
         B     CMLTBERR                                                         
*                                                                               
BLDCML10 ST    R5,SVCMLCNT                                                      
         B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
*====================================================================*          
* TRANSLATE ALL PRODUCT CODES IN BUYREC TO ATT PRD CODES             *          
* APRDTAB CONTAINS APPROPRIATE PRDTAB ADDRESS                        *          
* NON-ZERO VALUE IN ERRSW ON EXIT MEANS TRANSLATION ERROR OCCURRED   *          
* X'80' = UNABLE TO TRANSLATE PRODUCT CODE                           *          
* X'40' = SPOT OUTSIDE ATT EST PERIOD                                *          
* X'20' = NO ATT ESTIMATE OPEN                                       *          
*====================================================================*          
         SPACE 1                                                                
XAPRD    NTR1                                                                   
         MVI   ERRSW,0             RESET ERROR FLAG                             
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R7,BDMASPRD                                                      
         CLI   0(R7),0                                                          
         BE    XAPRD10                                                          
         BAS   RE,FINDPRD                                                       
         BNE   NEQXIT                                                           
*                                                                               
         LA    R7,1(R7)                                                         
         CLI   0(R7),0                                                          
         BE    XAPRD10                                                          
         BAS   RE,FINDPRD                                                       
         BNE   NEQXIT                                                           
*                                                                               
XAPRD10  SR    R0,R0                                                            
         LA    R6,BDELEM                                                        
*                                                                               
XAPRD12  ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    XAPRD30                                                          
         CLI   0(R6),6                                                          
         BL    XAPRD12                                                          
         CLI   0(R6),13                                                         
         BH    XAPRD12                                                          
*                                                                               
         CLC   2(2,R6),POLESDTS    TEST ELEM PRIOR TO ATT EST START             
         BNL   *+8                                                              
         OI    ERRSW,X'40'                                                      
         CLC   2(2,R6),POLESDTS+2    OR AFTER ATT EST END                       
         BNH   *+8                                                              
         OI    ERRSW,X'40'                                                      
*                                                                               
         ZIC   R0,1(R6)                                                         
         SH    R0,=H'10'                                                        
         BNP   XAPRD12                                                          
         SRL   R0,2                SET FOR BCT                                  
         LA    R7,10(R6)                                                        
*                                                                               
XAPRD20  BAS   RE,FINDPRD                                                       
         BNE   NEQXIT                                                           
*                                                                               
         ZIC   RF,0(R7)            GET PRD NUM                                  
         SLL   RF,8                PRD NUM X 256                                
         ZIC   RE,BUYREC+9         ESTIMATE NUMBER                              
         AR    RF,RE                                                            
         A     RF,SVPRDEST         POINT TO EST TAB ENTRY                       
         CLI   0(RF),0             TEST ATT EST OPEN                            
         BNE   XAPRD24                                                          
         OI    ERRSW,X'20'         SET NO EST ERROR                             
         B     XAPRD30                                                          
***      MVC   ERRMSG+38(3),1(R7)                                               
*                                                                               
XAPRD24  LA    R7,4(R7)            NEXT PRD CODE                                
         CLI   ERRSW,0             TEST FOR ERRORS *****                        
         BNE   XAPRD30             IF SO GIVE ERROR *****                       
         BCT   R0,XAPRD20          CONTINUE                                     
         B     XAPRD12                                                          
*                                                                               
XAPRD30  CLI   ERRSW,0             TEST FOR ERRORS                              
         BE    EQXIT               NONE - EXIT WITH CC EQ                       
         TM    ERRSW,X'40'                                                      
         BO    XAPERERR                                                         
         TM    ERRSW,X'20'                                                      
         BO    XAESTER2                                                         
         DC    H'0'                                                             
         EJECT                                                                  
FINDPRD  DS    0H                                                               
         ZIC   RF,0(R7)            GET PRD CODE                                 
         A     RF,=A(PRDTAB)                                                    
         CLI   0(RF),0                                                          
         BE    FINDPRD2                                                         
         MVC   0(1,R7),0(RF)                                                    
         MVC   SVPRDB,0(RF)        SAVE LAST PRD TRANSLATED                     
         CR    RE,RE               SET CC EQUAL                                 
         BR    RE                                                               
*                                                                               
FINDPRD2 NTR1 ,                     SET UP TO 'ENTER' ERROR ROUTINES            
         OI    ERRSW,X'80'                                                      
         B     XAPRDERR                                                         
         SPACE 1                                                                
*=========================================================*                     
* TEST AGENCY ESTIMATE NUMBER IS VALID ON ATT CLIENT      *                     
* ON ENTRY R0 CONTAINS EST NUMBER                         *                     
*=========================================================*                     
         SPACE 1                                                                
XAEST    DS    0H                                                               
         LR    RF,R0                                                            
         SLL   RF,2                X 4                                          
         A     RF,SVPOLEST                                                      
         MVC   POLESDTS,0(RF)      MOVE TO SAVE AREA                            
         OC    0(4,RF),0(RF)       TEST POL EST OPEN                            
         BZ    XAEST2                                                           
         CR    RE,RE               FOUND - SET CC EQ AND RETURN                 
         BR    RE                                                               
*                                                                               
XAEST2   NTR1 ,                     SET UP TO 'ENTER' ERROR ROUTINES            
         ZIC   RF,0(R7)            GET PRD CODE                                 
         A     RF,=A(PRDTAB)                                                    
         B     XAESTERR                                                         
         EJECT                                                                  
*=================================================================*             
* SEARCH ATT MKT-STA LIST FOR CORRECT MARKET NUMBER               *             
* REMEMBER THAT TABLE IS IN STATION ORDER BUT ENTRIES ARE MKT/STA *             
*=================================================================*             
         SPACE 1                                                                
XAMKT    NTR1                                                                   
         LA    R4,BUYREC+4         POINT TO MARKET CODE                         
         GOTO1 BINSRCH,DMCB,(R4),A(ATSTATAB),ATSTACNT,6,(2,3),ATSTACNT          
         CLI   0(R1),1             TEST MARKET NOT FOUND                        
         BE    MKTERR                                                           
         L     RE,0(R1)            GET ENTRY ADDRESS                            
         MVC   BUYREC+4(2),0(RE)   MOVE NEW MARKET NUMBER                       
         MVC   LASTAFF,5(RE)       SET NEW AFFILIATE IND                        
         SPACE 1                                                                
* OVERWRITE SPILL DEMO ELEMENT CODES *                                          
         SPACE 1                                                                
         LA    R6,BUYREC+24                                                     
         MVI   ELCODE,3                                                         
XAMKT2   BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         OI    0(R6),X'80'         SET ELCODE = X'83'                           
         B     XAMKT2                                                           
         EJECT                                                                  
*=================================================================*             
* SEARCH AGENCY MKT LIST TO DETERMINE RTGSVC MARKET NUMBER        *             
* THEN SEARCH ATT MKT LIST TO GET ATT MARKET NUMBER               *             
*=================================================================*             
         SPACE 1                                                                
XAGLMKT  NTR1                                                                   
*                                                                               
         L     R2,ADGOAL                                                        
         USING GOALRECD,R2                                                      
*                                                                               
         CLC   LASTMKT(2),GOALREC+5                                             
         BNE   XAGLMKT2                                                         
         MVC   GOALREC+5(2),LASTMKT+2                                           
         B     EXIT                                                             
*                                                                               
XAGLMKT2 LA    R4,GOALREC+5        POINT TO MARKET CODE                         
         GOTO1 BINSRCH,DMCB,(R4),A(AGMKTTAB),AGMKTCNT,4,(0,2),AGMKTCNT          
         CLI   0(R1),1             TEST MARKET NOT FOUND                        
         BE    GLMKTERR                                                         
         L     RE,0(R1)            GET ENTRY ADDRESS                            
         MVC   HALF,2(RE)          SAVE RTGSVC MARKET NUMBER                    
         SPACE 1                                                                
* NOW CONVERT TO ATT MARKET NUMBER *                                            
         SPACE 1                                                                
         GOTO1 BINSRCH,DMCB,HALF,A(ATMKTTAB),ATMKTCNT,4,(0,2),ATMKTCNT          
         CLI   0(R1),1                                                          
         BE    GLMKTERR                                                         
         L     RE,0(R1)               GET ENTRY ADDRESS                         
         MVC   GOALREC+5(2),2(RE)     MOVE ATT MARKET NUMBER                    
         MVC   LASTMKT(2),GOALREC+5   SAVE THE AGY MARKET NUMBER                
         MVC   LASTMKT+2(2),2(RE)     SAVE THE ATT MARKET NUMBER                
         B     EXIT                                                             
         SPACE 1                                                                
GLMKTERR MVC   ERRMSG(25),=C'ATT GOAL MARKET NOT FOUND'                         
         LA    R4,ERRKEY                                                        
*                                                                               
         CLC   SVBUYERR(8),GOALREC  SAME A-M/CLT/PRD/MKT/EST                    
         BE    GMKTER5              DON'T PRINT ERROR                           
         EJECT                                                                  
         LA    R1,GOALREC+2        POINT TO CLIENT                              
         BAS   RE,TSTMC            IF MCCAN CLT                                 
         BNE   GMKTER10                                                         
*                                                                               
GMKTER5  MVC   ERRMSG,SPACES       DON'T PRINT ERROR                            
         B     EXIT                                                             
*                                                                               
GMKTER10 MVC   SVBUYERR,GOALREC                                                 
         MVC   0(3,R4),SVAGYA                                                   
         LA    R4,4(R4)                                                         
         GOTO1 CLUNPK,DMCB,GOALREC+2,(R4)                                       
         LA    R4,4(R4)                                                         
* GET ATT PRD CODE                                                              
         L     RE,ADCLT            PRD CODE NOT TRANSLATED YET                  
         LA    RE,CLIST-CLTHDRD(RE)                                             
*                                                                               
GLMKTER2 CLC   GOALREC+4(1),3(RE)                                               
         BE    GLMKTER4                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   GLMKTER2                                                         
         ZIC   R0,GOALREC+4        IF NOT FOUND, PRINT PRD NUM                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(3),DUB                                                      
         LA    RE,FULL                                                          
GLMKTER4 MVC   0(3,R4),0(RE)                                                    
         LA    R4,4(R4)                                                         
*                                                                               
         SR    R0,R0               MARKET                                       
         ICM   R0,3,GOALREC+5                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R4),DUB                                                      
*                                                                               
         LA    R4,5(R4)                                                         
         ZIC   R0,GOALREC+7        ESTIMATE                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         LA    R4,4(R4)                                                         
*                                                                               
         MVC   0(1,R4),GOALREC+8   DPT                                          
         ZIC   R0,GOALREC+9        SLN                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R4),DUB                                                      
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* TRANSLATE BUYREC CMML CODES TO ATT CMML CODES                   *             
*=================================================================*             
         SPACE 1                                                                
XACML    NTR1                                                                   
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
XACML2   ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXIT                                                             
         CLI   0(R6),X'12'         TEST FILM ELEMENT                            
         BNE   XACML2                                                           
*                                                                               
         ZIC   R0,1(R6)                                                         
         SH    R0,=H'3'                                                         
         SRL   R0,1                DIVIDE BY 2                                  
         LA    R1,3(R6)                                                         
*                                                                               
XACML4   BAS   RE,GETCML                                                        
         BE    XACML6                                                           
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'4'            3 BYTES OVHD/1 FOR EX                        
         EX    RE,*+8                                                           
         B     XACML2                                                           
         XC    2(0,R6),2(R6)       WIPE OUT CML DATA                            
*                                                                               
XACML6   BAS   RE,GETCMLAT                                                      
         LA    R1,2(R1)                                                         
         BCT   R0,XACML4                                                        
         B     XACML2                                                           
*                                                                               
CMLERR1  DC    H'0'                CMML NOT FOUND IN AGYLIST                    
         EJECT                                                                  
*================================================================*              
* FIND ALPHA EQUIVALENT OF CMLSEQ IN AGCMLTB AND SAVE IN AGCMML *               
*================================================================*              
         SPACE 1                                                                
GETCML   L     R4,=A(AGCMLTB)      GET ADDRESS OF CURRENT TABLE                 
         LR    R5,R4                                                            
         SH    R5,=H'4'            BACK UP TO GET COUNT                         
         ICM   R5,15,0(R5)                                                      
         BNZ   GETCML2                                                          
         LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
*                                                                               
GETCML2  CLC   8(2,R4),0(R1)       MATCH CMML SEQ                               
         BE    GETCML4                                                          
         LA    R4,10(R4)                                                        
         BCT   R5,GETCML2                                                       
         LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
*                                                                               
GETCML4  MVC   AGCMML,0(R4)        SAVE CMLTAB ENTRY                            
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
* FIND ALPHA CMML ENTRY IN ATTTAB                                 *             
*=================================================================*             
         SPACE 1                                                                
GETCMLAT L     R4,SVCMLTAB         GET ADDRESS OF CURRENT TABLE                 
         LR    R5,R4                                                            
         SH    R5,=H'4'            BACK UP TO GET COUNT                         
         ICM   R5,15,0(R5)                                                      
         BNZ   CMLAT2                                                           
         XC    0(2,R1),0(R1)       CLEAR OUT THE MISSING ONE                    
         BR    RE                                                               
*                                                                               
CMLAT2   CLC   0(8,R4),AGCMML      MATCH CMML CODE                              
         BE    CMLAT4                                                           
         LA    R4,10(R4)                                                        
         BCT   R5,CMLAT2                                                        
         XC    0(2,R1),0(R1)       CLEAR MISSING CMML SEQ                       
         B     CMLATERR            AND PRINT ERROR MESSAGE                      
*                                                                               
CMLAT4   MVC   0(2,R1),8(R4)       MOVE ATT CML SEQ NUMBER                      
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
*                                                                               
CMLATERR NTR1                                                                   
         MVC   ERRMSG(33),=C'** ERROR ** MISSING ATT COMMERCIAL'                
         MVC   ERRMSG+34(8),AGCMML                                              
         GOTO1 REPORT                                                           
         B     EXIT                RETURNS TO SUBR CALLER                       
         EJECT                                                                  
*                                                                               
MKTERR   MVC   ERRMSG(32),=C'** ERROR ** ATT MARKET NOT FOUND'                  
         B     XAERR                                                            
*                                                                               
XAPERERR MVC   ERRMSG(37),=C'** ERROR ** BUY NOT IN ATT EST PERIOD'             
         B     XAERR                                                            
*                                                                               
XAPRDERR MVC   ERRMSG(36),=C'** ERROR ** MISSING ATT PRODUCT CODE'              
*                                                                               
* NEED TO PUT ALPHA CODE IN MESSAGE                                             
*                                                                               
XAPRERR0 L     RE,ADCLT                                                         
         LA    RE,CLIST-CLTHDRD(RE)                                             
*                                                                               
XAPRERR2 CLC   0(1,R7),3(RE)       MATCH PRD NUMBERS                            
         BE    XAPRERR4                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   XAPRERR2                                                         
         ZIC   R0,0(R7)            ANYTHING IS BETTER THAN BLOWING UP           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(3),DUB                                                      
         LA    RE,FULL                                                          
XAPRERR4 MVC   ERRMSG+38(3),0(RE)                                               
         B     XAERR                                                            
*                                                                               
XAESTERR MVC   ERRMSG(37),=C'** ERROR ** NO ATT BRAND ESTIMATE PRD='            
         B     XAPRERR0                                                         
*                                                                               
XAESTER2 GOTO1 CLUNPK,DMCB,BUYREC+1,WORK                                        
         L     R4,=A(CLTTAB)                                                    
         USING CLTTABD,R4                                                       
*                                                                               
XAESTER3 CLC   CLTATCLT,WORK       MATCH ATT CLIENT CODES                       
         BE    XAESTER4                                                         
         LA    R4,CLTTABL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   XAESTER3                                                         
         DC    H'0'                INCONSISTENT TABLES                          
*                                                                               
XAESTER4 L     RE,CLTCLTAD                                                      
         LA    RE,CLIST-CLTHDRD(RE)                                             
         MVC   ERRMSG(37),=C'** ERROR ** NO ATT BRAND ESTIMATE PRD='            
         B     XAPRERR2                                                         
         DROP  R4                                                               
         EJECT                                                                  
XAERR    LA    R4,ERRKEY                                                        
*                                                                               
         LA    R1,BUYREC+2         POINT TO CLIENT                              
         CLI   BUYREC,2            IS THIS A GOAL RECORD                        
         BE    *+8                                                              
         LA    R1,BUYREC+1         POINT TO CLIENT                              
         BAS   RE,TSTMC            IF MCCAN CLT                                 
         BE    XAERR1                                                           
*                                                                               
         LA    R1,9                                                             
         CLI   SVBUYERR,X'02'      IS THIS A GOAL RECORD                        
         BNE   *+8                                                              
         LA    R1,7                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVBUYERR(0),BUYREC                                               
         BNE   XAERR2                                                           
*                                                                               
XAERR1   MVC   ERRMSG,SPACES                                                    
         B     NEQXIT                                                           
*                                                                               
XAERR2   MVC   SVBUYERR,BUYREC                                                  
         MVC   0(3,R4),SVAGYA                                                   
         LA    R4,4(R4)                                                         
*                                                                               
         CLI   BUYREC,2            TEST GOAL RECORD ERROR                       
         BE    XAERR10                                                          
*                                                                               
         GOTO1 CLUNPK,DMCB,BUYKEY+1,(R4)                                        
         LA    R4,4(R4)                                                         
*                                                                               
         GOTO1 MSUNPK,DMCB,BUYKEY+4,(R4),5(R4)                                  
         LA    R4,11(R4)                                                        
*                                                                               
         ZIC   R0,BUYKEY+9         EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         MVI   3(R4),C'-'                                                       
         ZIC   R0,BUYREC+10        LINE                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R4),DUB                                                      
         B     XAERRX                                                           
         EJECT                                                                  
*========================================================*                      
* GOAL RECORD ERROR                                      *                      
*========================================================*                      
         SPACE 1                                                                
XAERR10  DS    0H                                                               
         GOTO1 CLUNPK,DMCB,BUYKEY+2,(R4)                                        
         LA    R4,4(R4)                                                         
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BUYKEY+5       MKT                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R4),DUB                                                      
         LA    R4,11(R4)                                                        
*                                                                               
         ZIC   R0,BUYKEY+7         EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         LA    R4,4(R4)                                                         
*                                                                               
         MVC   0(1,R4),BUYKEY+8    DPT                                          
         ZIC   R0,BUYREC+9         SLN                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R4),DUB                                                      
*                                                                               
XAERRX   GOTO1 REPORT                                                           
         B     NEQXIT                                                           
         EJECT                                                                  
* IF MCCAN CLIENT - DON'T PRINT ERROR                                           
*                                                                               
TSTMC    CLC   =X'8452',0(R1)      CLT = BCS                                    
         BNE   *+8                                                              
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
* PRINT TOTALS FOR AN AGENCY                                                    
         SPACE 1                                                                
XAAGYTOT NTR1                                                                   
         CLI   XFRAGY,0            TEST FIRST TIME                              
         BE    AGYTOTX                                                          
*                                                                               
         MVC   P(3),SVAGYA         AGENCY ALPHA                                 
         GOTO1 CLUNPK,DMCB,SVCLTB,P+3                                           
         MVI   P+7,C'='                                                         
         ZIC   R0,XFRAGY                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+8(2),DUB                                                       
         OI    AGYCNT+3,X'0F'                                                   
         UNPK  P+14(7),AGYCNT                                                   
         MVC   P+22(7),=C'RECORDS'                                              
         GOTO1 REPORT                                                           
*                                                                               
AGYTOTX  IC    RE,XFRAGY           BUMP XFR AGENCY NUMBER                       
         LA    RE,1(RE)                                                         
         STC   RE,XFRAGY                                                        
         ZAP   AGYCNT,=P'0'        RESET AGENCY RECORD COUNTER                  
         B     EXIT                                                             
         EJECT                                                                  
*=============================================================*                 
* SUBROUTINE TO CLEAR FROM R1 TO R0. RF IS DESTROYED.         *                 
*=============================================================*                 
         SPACE 1                                                                
CLEAR    LA    RF,256                                                           
*                                                                               
CLEAR2   CR    R0,RF                                                            
         BNH   CLEAR4                                                           
         XC    0(256,R1),0(R1)                                                  
         AR    R1,RF                                                            
         SR    R0,RF                                                            
         BZR   RE                                                               
         B     CLEAR2                                                           
CLEAR4   LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,CLEARXC                                                       
         BR    RE                                                               
CLEARXC  XC    0(0,R1),0(R1)  ** EXECUTED **                                    
         EJECT                                                                  
**************************************************                              
* ACCUMULATE ORDERED/PAID TOTALS FOR BUY RECORDS *                              
* R2 POINTS TO BUY RECORD                        *                              
* TOTIND = C'-' TO POST TO REMOVED               *                              
* TOTIND = C'+' TO POST TO ADDED                 *                              
**************************************************                              
         SPACE 1                                                                
XATOT    NTR1                                                                   
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         CLI   0(R2),X'10'         TEST BUYREC                                  
         BNH   EXIT                                                             
*                                                                               
         CLI   0(R2),X'FF'         TEST EOF                                     
         BE    XATOT10                                                          
*                                                                               
         TM    15(R2),X'80'        TEST DELETED                                 
         BNZ   EXIT                                                             
*                                                                               
         CLI   OLDCLT,0            TEST FIRST TIME                              
         BE    XATOT2                                                           
*                                                                               
         CLC   OLDCLT,1(R2)        TEST SAME CLIENT                             
         BE    XATOT4              YES                                          
         L     R5,NEXTBUF          NO - MOVE CLT TOT TO BUFFER                  
         MVC   0(1,R5),TOTIND      MOVE +/-                                     
         MVC   1(2,R5),OLDCLT                                                   
         MVC   4(24,R5),CTOTS                                                   
         LA    R5,28(R5)                                                        
         ST    R5,NEXTBUF                                                       
         MVI   0(R5),0             SET END OF BUFFER FLAG                       
*                                                                               
XATOT2   LA    R1,CTOTS                                                         
         BAS   RE,XACLR            CLEAR CTOTS                                  
*                                                                               
         MVC   OLDCLT,1(R2)        MOVE CLIENT CODE                             
*                                                                               
XATOT4   SR    R4,R4               GROSS ORD                                    
         SR    R5,R5               GROSS PAID                                   
         SR    R6,R6               NET PAID                                     
         IC    R7,38(R2)           SAVE BDTIME BYTE                             
         MVI   38(R2),0            MAKE ZERO FOR GETRATE (PB'S)                 
*                                                                               
         L     RF,GETRATE                                                       
*                                                                               
         LA    R3,24(R2)           PROCESS BUYREC                               
         SR    R0,R0                                                            
XATOT6   ICM   R0,1,1(R3)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    XATOT8                                                           
         CLI   0(R3),6                                                          
         BL    XATOT6                                                           
         CLI   0(R3),14                                                         
         BH    XATOT6                                                           
         LA    R1,SPOTS                                                         
         STM   R1,R3,DMCB                                                       
         LA    R1,DMCB                                                          
         MVC   0(1,R1),3(R2)       MOVE PRD CODE                                
         BASR  RE,RF                                                            
*                                                                               
         A     R4,GROSS                                                         
         OC    4(2,R3),4(R3)       TEST PAID                                    
         BZ    XATOT6                                                           
         A     R5,GROSS                                                         
         A     R6,NET                                                           
         B     XATOT6                                                           
*                                                                               
XATOT8   STC   R7,38(R2)           RESTORE BDTIME                               
         CVD   R4,DUB                                                           
         AP    CTOTS(8),DUB                                                     
         CVD   R5,DUB                                                           
         AP    CTOTS+8(8),DUB                                                   
         CVD   R6,DUB                                                           
         AP    CTOTS+16(8),DUB                                                  
         B     EXIT                                                             
*                                                                               
XATOT10  L     R5,NEXTBUF          MOVE LAST CLT TOT TO BUFFER                  
         MVC   0(1,R5),TOTIND                                                   
         MVC   1(2,R5),OLDCLT                                                   
         MVC   4(24,R5),CTOTS                                                   
         MVI   28(R5),0            SET END OF BUFFER FLAG                       
         LA    R5,28(R5)           AND ADVANCE POINTER                          
         ST    R5,NEXTBUF                                                       
         XC    OLDCLT,OLDCLT       SET FIRST TIME FLAG                          
         B     EXIT                                                             
         EJECT                                                                  
* PRINT DOLLAR TOTALS FROM BUFFER *                                             
         SPACE 1                                                                
XATOTPRT NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   P(21),=C'** DELETED RECORDS **'                                  
         GOTO1 REPORT                                                           
*                                                                               
         L     R5,=A(TOTBUFF)                                                   
         ST    R5,NEXTBUF                                                       
*                                                                               
         CLI   0(R5),C'-'                                                       
         BNE   XATP8                                                            
*                                                                               
XATP2    DS    0H                                                               
         GOTO1 CLUNPK,DMCB,1(R5),P+27                                           
*                                                                               
XATP4    LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,4(R5)                                                         
         LA    R6,AGYTOTS                                                       
*                                                                               
XATP6    AP    0(8,R6),0(8,R5)     BUMP AGY TOTALS                              
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XATP6                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),C'-'                                                       
         BE    XATP2                                                            
         ST    R5,NEXTBUF          SAVE BUFFER POINTER                          
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
         SPACE 1                                                                
* PRINT THE - TOTALS *                                                          
         SPACE 1                                                                
XATP8    MVC   P+27(5),=C'TOTAL'                                                
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,AGYTOTS                                                       
         LA    R6,FILTOTS                                                       
*                                                                               
XATP10   SP    0(8,R6),0(8,R5)     SUBTRACT FROM AATTUMS                        
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XATP10                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R1,AGYTOTS                                                       
         BAS   RE,XACLR                                                         
         SPACE 2                                                                
* PRINT THE TOTALS OF INSERTED RECORDS *                                        
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         L     R5,NEXTBUF          RESTORE BUFFER POINTER                       
*                                                                               
         MVC   P(21),=C'** INSERTED RECORDS **'                                 
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),0             TEST END OF BUFFER                           
         BE    XATP20                                                           
*                                                                               
XATP12   DS    0H                                                               
         GOTO1 CLUNPK,DMCB,1(R5),P+27                                           
*                                                                               
XATP14   LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,4(R5)                                                         
         LA    R6,AGYTOTS                                                       
*                                                                               
XATP16   AP    0(8,R6),0(8,R5)     BUMP AGY TOTALS                              
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XATP16                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),0                                                          
         BNE   XATP12                                                           
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         MVC   P+27(5),=C'TOTAL'                                                
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,AGYTOTS                                                       
         LA    R6,FILTOTS                                                       
*                                                                               
XATP18   AP    0(8,R6),0(8,R5)     BUMP FILE TOTALS                             
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XATP18                                                        
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         LA    R1,AGYTOTS                                                       
         BAS   RE,XACLR                                                         
         SPACE 1                                                                
XATP20   DS    0H                                                               
         MVC   P(22),=C'* GRAND TOTALS (NET) *'                                 
*                                                                               
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,FILTOTS                                                       
*                                                                               
XATP22   MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR PENNIES                                
         MVI   15(R3),C'+'         SET SIGN                                     
         CP    0(8,R5),=P'0'                                                    
         BNM   *+8                                                              
         MVI   15(R3),C'-'                                                      
         LA    R5,8(R5)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XATP22                                                        
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
XACLR    LA    R0,3                                                             
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
*=================================================================*             
*        ADD A CLONED ELEMENT TO RECORD                                         
*=================================================================*             
         SPACE 1                                                                
BLDCLONE NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING CRELEM,R4                                                        
         MVI   CRCODE,CRCODEQ      ELEMENT CODE                                 
         MVI   CRLEN,CRLENQ        LENGTH                                       
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         MVC   CRCLKEY,0(R2)       SET CLONED FROM RECORD'S KEY                 
         MVC   CRALPHA,BUYALPHA        AGY ALPHA                                
*                                                                               
BCLX     B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
*=================================================================*             
*                                                                               
*        ADD CLONED ELEMENT AFTER LAST ELEMENT FOUND                            
*            & UPDATE LENGTH OF RECORD                                          
*        R2 - A(RECORD)                                                         
*                                                                               
*=================================================================*             
ADELEM   NTR1                                                                   
         USING BUYRECD,R2                                                       
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL           FIND LAST ELEMENT                            
         BE    *-4                                                              
*                                  ADD CLONED ELEMENT AFTER LAST ELEM           
         GOTO1 =V(RECUP),DMCB,(0,(R2)),ELEMENT,(C'R',(R6))                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,13(R2)         GET REC LENGTH                               
         AH    R0,=H'4'                                                         
         L     R4,ADBUY                                                         
         SH    R4,=H'4'                                                         
         SLL   R0,16               LEFT ALIGN                                   
         ST    R0,0(R4)            SET REC LENGTH FOR SORT                      
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*=================================================================*             
*        PUT A RECORD TO SORTER                                                 
*=================================================================*             
         SPACE                                                                  
PUTSORT  NTR1                                                                   
         AP    AGYCNT,=P'1'                                                     
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)    AND PUT RECORD 1                 
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* TRACE DATA BLOCK                                                              
*                                                                               
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA) OR ZERO FOR RECORD                               
*        PARAMETER 3 - A(LABEL) OR ZERO FOR NO LABEL                            
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*                                                                               
*=================================================================*             
MYTRACE  NTR1                                                                   
*                                                                               
         LM    R2,R5,0(R1)         R2 = A(DATA)                                 
*                                  R3 = L(DATA)                                 
*                                  R4 = A(LABEL)                                
*                                  R5 = L(LABEL)                                
*                                                                               
         LTR   R4,R4               IF CALLER SUPPLIED A LABEL                   
         BZ    TR10                                                             
*                                                                               
         MVI   P,C'-'              THEN FILL PRINT LINE WITH '-'S               
         MVC   P+1(131),P                                                       
*                                                                               
         LR    RE,R5               RF = A(PLACE TO CENTER LABEL)                
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P(RF)                                                         
*                                                                               
         BCTR  R5,0                MOVE LABEL TO CENTER OF PRINT LINE           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
         GOTO1 REPORT              PRINT LABEL LINE                             
*                                                                               
TR10     LTR   R3,R3               IF DATA IS A RECORD                          
         BNZ   TR50                                                             
         OC    DATADISP,DATADISP   IF THERE IS A KEY                            
         BZ    TR15                                                             
*                                  PRINT OUT ITS KEY                            
         LH    R3,DATADISP                                                      
         GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR15     LR    R6,R2               A(RECORD)                                    
         AH    R6,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R4,1(R6)            PRINT ELEMENT                                
         GOTO1 PRNTBL,DMCB,0,(R6),C'DUMP',(R4),=X'01C4'                         
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
         B     TR100                                                            
*                                  ELSE PRINT ENTIRE DATA BLOCK                 
TR50     GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR100    DS    0H                                                               
*                                                                               
TRX      B     EXIT                                                             
         EJECT                                                                  
NEXTBUF  DC    A(TOTBUFF)                                                       
         DS    0D                                                               
         DC    CL8'**SAVE**'                                                    
SVAGYTAB DS    A                                                                
SVCLTTAB DS    A                                                                
SVCMLTAB DS    A                                                                
SVCMLCNT DS    F                                                                
SVPOLEST DS    A                                                                
SVPRDEST DS    A                                                                
SVAGYA   DS    XL2                                                              
SVAGYB   DS    XL1                                                              
SVCLTB   DS    XL2                                                              
SVPRDB   DS    XL1                                                              
SVXFRAGY DS    XL1                                                              
*                                                                               
TOTIND   DC    X'00'                                                            
OLDCLT   DC    XL2'00'                                                          
CTOTS    DS    0CL24                                                            
         DC    3PL8'0'             GROSS ORD/GROSS PAID/NET PAID                
AGYTOTS  DC    3PL8'0'                                                          
FILTOTS  DC    3PL8'0'                                                          
POLESDTS DS    F                                                                
BUYSTART DS    XL2                                                              
BUYEND   DS    XL2                                                              
LASTMKT  DS    XL4                                                              
LASTAFF  DS    XL1                                                              
XFRAGY   DC    X'00'                                                            
AGCMML   DS    CL10                                                             
CNDATA   DS    XL14                                                             
MKTCD    DS    CL3                 NSI MARKET CODE                              
SVBUYERR DS    CL13                                                             
ELCODE   DS    XL1                                                              
ELEMENT  DS    XL256                                                            
ERRSW DC       X'00'                                                            
*                                                                               
SVPROF   DS    CL16                                                             
*                                                                               
STATBERR DC    H'0'                                                             
MKTTBERR DC    H'0'                                                             
CMLTBERR DC    H'0'                                                             
*                                                                               
AGYCNT   DC    PL4'0'                                                           
*                                                                               
ATSTACNT DC    A(0)                                                             
ATMKTCNT DC    A(0)                                                             
AGMKTCNT DC    A(0)                                                             
ATGLCNT  DC    A(0)                                                             
*                                                                               
CTRS     DS    0CL24                                                            
INCNT    DC    PL4'0',CL20'RECORDS IN'                                          
OUTCNT   DC    PL4'0',CL20'RECORDS OUT'                                         
DELCNT   DC    PL4'0',CL20'RECORDS DELETED'                                     
ADDCNT   DC    PL4'0',CL20'RECORDS INSERTED'                                    
CTRX     EQU   *-1                                                              
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=XA50                                                       
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760                                         
         SPACE 1                                                                
         EJECT                                                                  
* THIS PARAMETER LIST USED FOR BINSRCH ON AORTAB *                              
         SPACE 1                                                                
AORPARMS DC    A(0)                A(KEYARG)                                    
         DC    A(AORTAB)           A(TABLE)                                     
         DC    A(AORTABCT)         TABLE COUNT                                  
         DC    A(AORTABL)          ENTRY LENGTH                                 
         DC    AL1(11),AL3(3)      KEY DSPL, KEY LEN (CLT/PRD)                  
         DC    A(AORTABCT)         MAX TABLE COUNT                              
         SPACE 2                                                                
FLIST    DC    CL8' SPTFILE'                                                    
         DC    CL8' SPTDIR '                                                    
         DC    CL8' STAFILE'                                                    
         DC    CL8' CTFILE '                                                    
FTRFDIR  DC    CL8'ZSTRFDR '                                                    
FTRFFIL  DC    CL8'ZSTRFFL '                                                    
         DC    CL8'X       '                                                    
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
* THIS TABLE DEFINES THE LIST OF CLIENT CODES ON THE ATT FILE THAT  *           
* ARE TO BE CREATED.                                                *           
*===================================================================*           
         SPACE 1                                                                
CLTTABD  DSECT                                                                  
CLTATCLT DS    CL3                 EBCDIC CLIENT CODE                           
CLTATCLB DS    XL2                 PACKED CLIENT CODE                           
         DS    XL1                 SPARE (FOR ALIGNMENT)                        
*                                                                               
CLTATLST DS    6XL5                LIST OF RELATED AOR CLIENT CODES             
CLTATLSX EQU   *                   EBCDIC(3)/BINARY(2)                          
*                                                                               
CLTCLTAD DS    AL4                 CLIENT HEADER ADDRESS                        
CLTCMLAD DS    AL4                 COMMERCIAL TABLE ADDRESS                     
CLTATPOL DS    AL4                 POL ESTIMATE TABLE ADDRESS                   
CLTATEST DS    AL4                 BRAND ESTIMATE TABLE ADDRESS                 
CLTTABL  EQU   *-CLTTABD           ENTRY LENGTH                                 
         SPACE 1                                                                
SPXA02   CSECT                                                                  
         DS    0D                                                               
         DC    C'*CLTTAB*'                                                      
CLTTAB   DS    0C                                                               
         SPACE 1                                                                
* BUSINESS COMMUNICATION SERVICE                                                
         DC    C'BCS',XL2'00',X'00'                                             
         DC    CL5'BCN',CL5'BCY',CL5'BCO',CL5' ',CL5' ',CL5' '                  
         DC    AL4(BCSCLT),AL4(BCSCMLTB),AL4(BCSPOLTB),AL4(BCSESTTB)            
* BUSINESS COMMUNICATION SERVICE - 1993                                         
         DC    C'BC3',XL2'00',X'00'                                             
         DC    CL5'BCW',CL5'BCR',CL5'BCM',CL5' ',CL5' ',CL5' '                  
         DC    AL4(BC3CLT),AL4(BC3CMLTB),AL4(BC3POLTB),AL4(BC3ESTTB)            
* CONSUMER SERVICES                                                             
         DC    C'CCS',XL2'00',X'00'                                             
         DC    CL5'CCN',CL5'CCY',CL5'CCO',CL5' ',CL5' ',CL5' '                  
         DC    AL4(CCSCLT),AL4(CCSCMLTB),AL4(CCSPOLTB),AL4(CCSESTTB)            
* CONSUMER SERVICES 1993                                                        
         DC    C'CC3',XL2'00',X'00'                                             
         DC    CL5'CCW',CL5'CCR',CL5'CCM',CL5' ',CL5' ',CL5' '                  
         DC    AL4(CC3CLT),AL4(CC3CMLTB),AL4(CC3POLTB),AL4(CC3ESTTB)            
* CONSUMER SERVICES 1994                                                        
         DC    C'CC4',XL2'00',X'00'                                             
         DC    CL5'CCI',CL5'CCU',CL5' ',CL5' ',CL5' ',CL5' '                    
         DC    AL4(CC4CLT),AL4(CC4CMLTB),AL4(CC4POLTB),AL4(CC4ESTTB)            
* CONSUMER PRODUCT DIVISION                                                     
         DC    C'CPD',XL2'00',X'00'                                             
         DC    CL5'CPN',CL5'CPY',CL5'CPO',CL5' ',CL5' ',CL5' '                  
         DC    AL4(CPDCLT),AL4(CPDCMLTB),AL4(CPDPOLTB),AL4(CPDESTTB)            
* CONSUMER PRODUCT DIVISION - 1993                                              
         DC    C'CP3',XL2'00',X'00'                                             
         DC    CL5'CPW',CL5'CPR',CL5'CPM',CL5' ',CL5' ',CL5' '                  
         DC    AL4(CP3CLT),AL4(CP3CMLTB),AL4(CP3POLTB),AL4(CP3ESTTB)            
* GENERAL BUSINESS SYSTEMS                                                      
         DC    C'GBS',XL2'00',X'00'                                             
         DC    CL5'GBN',CL5'GBY',CL5'GBO',CL5' ',CL5' ',CL5' '                  
         DC    AL4(GBSCLT),AL4(GBSCMLTB),AL4(GBSPOLTB),AL4(GBSESTTB)            
* INTERNATIONAL BUSINESS SERVICES                                               
         DC    C'IBS',XL2'00',X'00'                                             
         DC    CL5'IBN',CL5'IBY',CL5'IBO',CL5' ',CL5' ',CL5' '                  
         DC    AL4(IBSCLT),AL4(IBSCMLTB),AL4(IBSPOLTB),AL4(IBSESTTB)            
* INTERNATIONAL BUSINESS SERVICES - 1993                                        
         DC    C'IB3',XL2'00',X'00'                                             
         DC    CL5'IBW',CL5'IBR',CL5'IBM',CL5' ',CL5' ',CL5' '                  
         DC    AL4(IB3CLT),AL4(IB3CMLTB),AL4(IB3POLTB),AL4(IB3ESTTB)            
* INTERNATIONAL BUSINESS SERVICES - 1994                                        
         DC    C'IB4',XL2'00',X'00'                                             
         DC    CL5'IBU',CL5' ',CL5' ',CL5' ',CL5' ',CL5' '                      
         DC    AL4(IB4CLT),AL4(IB4CMLTB),AL4(IB4POLTB),AL4(IB4ESTTB)            
* INTERNATIONAL CONSUMER SERVICES - BRAVO                                       
         DC    C'ICB',XL2'00',X'00'                                             
         DC    CL5' ',CL5' ',CL5' ',CL5' ',CL5' ',CL5' '                        
         DC    AL4(ICBCLT),AL4(ICBCMLTB),AL4(ICBPOLTB),AL4(ICBESTTB)            
* INTERNATIONAL CONSUMER SERVICES - CHAPMAN                                     
         DC    C'ICC',XL2'00',X'00'                                             
         DC    CL5' ',CL5' ',CL5' ',CL5' ',CL5' ',CL5' '                        
         DC    AL4(ICCCLT),AL4(ICCCMLTB),AL4(ICCPOLTB),AL4(ICCESTTB)            
* INTERNATIONAL CONSUMER SERVICES                                               
         DC    C'ICS',XL2'00',X'00'                                             
         DC    CL5'ICN',CL5'ICY',CL5'ICO',CL5' ',CL5' ',CL5' '                  
         DC    AL4(ICSCLT),AL4(ICSCMLTB),AL4(ICSPOLTB),AL4(ICSESTTB)            
* INTERNATIONAL CONSUMER SERVICES 1993                                          
         DC    C'IC3',XL2'00',X'00'                                             
         DC    CL5'ICW',CL5'ICR',CL5'ICM',CL5' ',CL5' ',CL5' '                  
         DC    AL4(IC3CLT),AL4(IC3CMLTB),AL4(IC3POLTB),AL4(IC3ESTTB)            
*                                                                               
         DC    C'NBA',XL2'00',X'00'                                             
         DC    CL5'NBW',CL5'NBR',CL5'NBM',CL5' ',CL5' ',CL5' '                  
         DC    AL4(NBACLT),AL4(NBACMLTB),AL4(NBAPOLTB),AL4(NBAESTTB)            
* INTERNATIONAL CONSUMER SERVICES                                               
         DC    C'UCS',XL2'00',X'00'                                             
         DC    CL5'UCN',CL5'UCY',CL5'UCO',CL5' ',CL5' ',CL5' '                  
         DC    AL4(UCSCLT),AL4(UCSCMLTB),AL4(UCSPOLTB),AL4(UCSESTTB)            
* INTERNATIONAL CONSUMER SERVICES - 1993                                        
         DC    C'UC3',XL2'00',X'00'                                             
         DC    CL5'UCW',CL5'UCR',CL5'UCM',CL5' ',CL5' ',CL5' '                  
         DC    AL4(UC3CLT),AL4(UC3CMLTB),AL4(UC3POLTB),AL4(UC3ESTTB)            
*                                                                               
         DC    X'FF'               EOT                                          
         EJECT                                                                  
*===================================================================*           
*  THIS TABLE DEFINES THE CLIENT CODES ON THE ATT FILE AND WHERE    *           
*  THE SOURCE DATA IS LOCATED ON THE AGENCY FILES.                  *           
*  NORMALLLY, THEY SHOULD BE THE SAME -- THAT IS, THE SOURCE DATA   *           
*  FOR CLIENT IBS ON ATT SHOULD COME FROM CLIENT IBS DATA ON THE    *           
*  AGENCY FILE.                                                     *           
*===================================================================*           
         SPACE 2                                                                
AGYTABD  DSECT                                                                  
AGYAGY   DS    CL2                 ALPHA AGENCY CODE                            
AGYATCLT DS    CL3                 ATT CLIENT CODE - EBCDIC                     
AGYAGCLT DS    CL3                 AGY CLIENT CODE - EBCDIC                     
AGYATCLB DS    XL2                 ATT CLIENT CODE - BINARY                     
AGYAGCLB DS    XL2                 AGY CLIENT CODE - BINARY                     
AGYADCLT DS    AL4                 ADDRESS OF CLTHDR                            
AGYTABL  EQU   *-AGYTABD                                                        
         SPACE 1                                                                
SPXA02   CSECT                                                                  
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'*AGYTAB*'                                                      
AGYTAB   DS    0C                                                               
         DC    C'NW',C'BCS',C'BCS',XL2'00',XL2'00',A(BCSCLT)                    
         DC    C'NW',C'BC3',C'BC3',XL2'00',XL2'00',A(BC3CLT)                    
         DC    C'NW',C'CCS',C'CCS',XL2'00',XL2'00',A(CCSCLT)                    
         DC    C'NW',C'CC3',C'CC3',XL2'00',XL2'00',A(CC3CLT)                    
         DC    C'NW',C'CC4',C'CC4',XL2'00',XL2'00',A(CC4CLT)                    
         DC    C'NW',C'CPD',C'CPD',XL2'00',XL2'00',A(CPDCLT)                    
         DC    C'NW',C'IBS',C'IBS',XL2'00',XL2'00',A(IBSCLT)                    
         DC    C'NW',C'IB3',C'IB3',XL2'00',XL2'00',A(IB3CLT)                    
         DC    C'NW',C'ICS',C'ICS',XL2'00',XL2'00',A(ICSCLT)                    
         DC    C'NW',C'IC3',C'IC3',XL2'00',XL2'00',A(IC3CLT)                    
         DC    C'NW',C'NBA',C'NBA',XL2'00',XL2'00',A(NBACLT)                    
         DC    C'NW',C'UCS',C'UCS',XL2'00',XL2'00',A(UCSCLT)                    
         DC    C'OM',C'BCS',C'BCS',XL2'00',XL2'00',A(BCSCLT)                    
         DC    C'OM',C'CCS',C'CCA',XL2'00',XL2'00',A(CCSCLT)                    
         DC    C'OM',C'CCS',C'CCS',XL2'00',XL2'00',A(CCSCLT)                    
         DC    C'OM',C'CC3',C'CC3',XL2'00',XL2'00',A(CC3CLT)                    
         DC    C'OM',C'CPD',C'CPD',XL2'00',XL2'00',A(CPDCLT)                    
         DC    C'OM',C'IBS',C'IBS',XL2'00',XL2'00',A(IBSCLT)                    
         DC    C'OM',C'IB3',C'IB3',XL2'00',XL2'00',A(IB3CLT)                    
         DC    C'OM',C'ICS',C'ICS',XL2'00',XL2'00',A(ICSCLT)                    
         DC    C'OM',C'IC3',C'IC3',XL2'00',XL2'00',A(IC3CLT)                    
         DC    C'OM',C'UCS',C'UCS',XL2'00',XL2'00',A(UCSCLT)                    
         DC    C'YN',C'BCS',C'BCS',XL2'00',XL2'00',A(BCSCLT)                    
         DC    C'YN',C'BC3',C'BC3',XL2'00',XL2'00',A(BC3CLT)                    
         DC    C'YN',C'CCS',C'CCS',XL2'00',XL2'00',A(CCSCLT)                    
         DC    C'YN',C'CC3',C'CC3',XL2'00',XL2'00',A(CC3CLT)                    
         DC    C'YN',C'CC4',C'CC4',XL2'00',XL2'00',A(CC4CLT)                    
         DC    C'YN',C'CPD',C'CPD',XL2'00',XL2'00',A(CPDCLT)                    
         DC    C'YN',C'IBS',C'IBS',XL2'00',XL2'00',A(IBSCLT)                    
         DC    C'YN',C'IB3',C'IB3',XL2'00',XL2'00',A(IB3CLT)                    
         DC    C'YN',C'IB4',C'IB4',XL2'00',XL2'00',A(IB4CLT)                    
         DC    C'YN',C'ICS',C'ICS',XL2'00',XL2'00',A(ICSCLT)                    
         DC    C'YN',C'IC3',C'IC3',XL2'00',XL2'00',A(IC3CLT)                    
         DC    C'YN',C'NBA',C'NBA',XL2'00',XL2'00',A(NBACLT)                    
         DC    C'YN',C'UCS',C'UCS',XL2'00',XL2'00',A(UCSCLT)                    
         DC    X'FF'                        EOL FLAG                            
         EJECT                                                                  
*========================================================*                      
* AGENCY OF RECORD TABLE                                 *                      
*                                                        *                      
* ENTRIES ASSIGN ATT PRODUCT TO AGENCY OF RECORD         *                      
* E.G., CLIENT IBS, PRODUCT ABC GOES TO CLIENT IBN (NW)  *                      
*                                                                               
* NOTE THAT ATCLB AND ATPRB ARE ADJACENT BECAUSE THEY    *                      
* ARE USED AS BINSRCH ARGUMENTS                          *                      
*========================================================*                      
         SPACE 1                                                                
AORTABD  DSECT                                                                  
*                                                                               
AORATCLT DS    CL3                 ATT CLIENT CODE  - EBCDIC                    
AORAGCLT DS    CL3                 AGY CLIENT CODE  - EBCDIC                    
AORATPRD DS    CL3                 ATT PRODUCT CODE - EBCDIC                    
AORAGCLB DS    XL2                 AGY CLIENT CODE  - BINARY                    
AORATCLB DS    XL2                 ATT CLIENT CODE  - BINARY                    
AORATPRB DS    XL1                 ATT PRODUCT CODE - BINARY                    
AORTABL  EQU   *-AORTABD                                                        
*                                                                               
SPXA02   CSECT                                                                  
         DS    0D                                                               
         DC    C'*AORTAB*'                                                      
AORTAB   DS    0CL(AORTABL)                                                     
         DC    C'BCS',C'BCS',C'EIT',2X'00',2X'00',X'00'                         
         DC    C'BCS',C'BCS',C'NBA',2X'00',2X'00',X'00'                         
         DC    C'BCS',C'BCS',C'PRW',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'CCS',C'CCN',C'ITA',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCN',C'ROF',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCN',C'STB',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCN',C'TAC',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCO',C'BAH',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCO',C'ROA',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCO',C'WIA',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCO',C'WIB',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCY',C'ACC',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCY',C'CPO',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCY',C'CDP',2X'00',2X'00',X'00'                         
         DC    C'CCS',C'CCY',C'QTM',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'CC3',C'CCW',C'EVE',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'FLS',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'ITL',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'ITR',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'LGL',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'OMB',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'NBA',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'ROF',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'SAV',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'STB',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'TAC',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'TR1',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'TRV',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCW',C'WIN',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCM',C'BAH',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCM',C'ROA',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCM',C'WIA',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCM',C'WIB',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCR',C'ACC',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCR',C'CCR',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCR',C'CDP',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCR',C'MZP',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCR',C'OPE',2X'00',2X'00',X'00'                         
         DC    C'CC3',C'CCR',C'YTH',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'CC4',C'CCI',C'CAL',2X'00',2X'00',X'00'                         
         DC    C'CC4',C'CCI',C'YTH',2X'00',2X'00',X'00'                         
         DC    C'CC4',C'CCU',C'MZP',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'CPD',C'CPN',C'PHN',2X'00',2X'00',X'00'                         
         DC    C'CPD',C'CPN',C'VPH',2X'00',2X'00',X'00'                         
         DC    C'CPD',C'CPY',C'COP',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'GBS',C'GBO',C'DTT',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'IBS',C'IBY',C'WOH',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'IB3',C'IBR',C'WOH',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'IB4',C'IBU',C'WOH',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'ICB',C'ICB',C'ILD',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'ICC',C'ICC',C'ROW',2X'00',2X'00',X'00'                         
         DC    C'ICC',C'ICC',C'WIN',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'ICS',C'ICY',C'CSB',2X'00',2X'00',X'00'                         
         DC    C'ICS',C'ICY',C'ILD',2X'00',2X'00',X'00'                         
         DC    C'ICS',C'ICY',C'IPP',2X'00',2X'00',X'00'                         
         DC    C'ICS',C'ICY',C'WCT',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'IC3',C'ICR',C'ILD',2X'00',2X'00',X'00'                         
         DC    C'IC3',C'ICR',C'IPP',2X'00',2X'00',X'00'                         
         DC    C'IC3',C'ICW',C'ROW',2X'00',2X'00',X'00'                         
         DC    C'IC3',C'ICW',C'SPC',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'NBA',C'NBR',C'YTH',2X'00',2X'00',X'00'                         
         DC    C'NBA',C'NBW',C'FLS',2X'00',2X'00',X'00'                         
         DC    C'NBA',C'NBW',C'ROW',2X'00',2X'00',X'00'                         
         DC    C'NBA',C'NBW',C'SPC',2X'00',2X'00',X'00'                         
         DC    C'NBA',C'NBW',C'CCS',2X'00',2X'00',X'00'                         
*                                                                               
         DC    C'UCS',C'UCS',C'BLT',2X'00',2X'00',X'00'                         
AORTABX  EQU   *                                                                
AORTABCT EQU   (AORTABX-AORTAB)/L'AORTAB                                        
         DC    X'FF'                                                            
         EJECT                                                                  
         DS    0D                                                               
         DC    C'**XAIO**'                                                      
         DS    F                   USED FOR RECORD LENGTH                       
XAIO     DS    2000C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*BCSCLT*'                                                    
BCSCLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'BCSPOLTB'                                                    
BCSPOLTB DS    256XL4                                                           
BCSPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'BCSESTTB'                                                    
BCSESTTB DS    210XL256                                                         
BCSESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*BC3CLT*'                                                    
BC3CLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'BC3POLTB'                                                    
BC3POLTB DS    256XL4                                                           
BC3POLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'BC3ESTTB'                                                    
BC3ESTTB DS    210XL256                                                         
BC3ESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*CCACLT*'                                                    
CCACLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'CCAPOLTB'                                                    
CCAPOLTB DS    256XL4                                                           
CCAPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CCAESTTB'                                                    
CCAESTTB DS    210XL256                                                         
CCAESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*CCSCLT*'                                                    
CCSCLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'CCSPOLTB'                                                    
CCSPOLTB DS    256XL4                                                           
CCSPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CCSESTTB'                                                    
CCSESTTB DS    210XL256                                                         
CCSESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*CC3CLT*'                                                    
CC3CLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'CC3POLTB'                                                    
CC3POLTB DS    256XL4                                                           
CC3POLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CC3ESTTB'                                                    
CC3ESTTB DS    210XL256                                                         
CC3ESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*CC4CLT*'                                                    
CC4CLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'CC4POLTB'                                                    
CC4POLTB DS    256XL4                                                           
CC4POLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CC4ESTTB'                                                    
CC4ESTTB DS    210XL256                                                         
CC4ESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*CPDCLT*'                                                    
CPDCLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'CPDPOLTB'                                                    
CPDPOLTB DS    256XL4                                                           
CPDPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CPDESTTB'                                                    
CPDESTTB DS    210XL256                                                         
CPDESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*CP3CLT*'                                                    
CP3CLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'CP3POLTB'                                                    
CP3POLTB DS    256XL4                                                           
CP3POLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CP3ESTTB'                                                    
CP3ESTTB DS    210XL256                                                         
CP3ESTTX EQU   *                                                                
*                                                                               
         DC    CL8'*GBSCLT*'                                                    
GBSCLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'GBSPOLTB'                                                    
GBSPOLTB DS    256XL4                                                           
GBSPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'GBSESTTB'                                                    
GBSESTTB DS    210XL256                                                         
GBSESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*IBSCLT*'                                                    
IBSCLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'IBSPOLTB'                                                    
IBSPOLTB DS    256XL4                                                           
IBSPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'IBSESTTB'                                                    
IBSESTTB DS    210XL256                                                         
IBSESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*IB3CLT*'                                                    
IB3CLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'IB3POLTB'                                                    
IB3POLTB DS    256XL4                                                           
IB3POLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'IB3ESTTB'                                                    
IB3ESTTB DS    210XL256                                                         
IB3ESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*IB4CLT*'                                                    
IB4CLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'IB4POLTB'                                                    
IB4POLTB DS    256XL4                                                           
IB4POLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'IB4ESTTB'                                                    
IB4ESTTB DS    210XL256                                                         
IB4ESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*ICSCLT*'                                                    
ICSCLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'ICSPOLTB'                                                    
ICSPOLTB DS    256XL4                                                           
ICSPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'ICSESTTB'                                                    
ICSESTTB DS    210XL256                                                         
ICSESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*IC3CLT*'                                                    
IC3CLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'IC3POLTB'                                                    
IC3POLTB DS    256XL4                                                           
IC3POLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'IC3ESTTB'                                                    
IC3ESTTB DS    210XL256                                                         
IC3ESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*NBACLT*'                                                    
NBACLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'NBAPOLTB'                                                    
NBAPOLTB DS    256XL4                                                           
NBAPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'NBAESTTB'                                                    
NBAESTTB DS    210XL256                                                         
NBAESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*ICBCLT*'                                                    
ICBCLT   DS    1500C                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'ICBPOLTB'                                                    
ICBPOLTB DS    256XL4                                                           
ICBPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'ICBESTTB'                                                    
ICBESTTB DS    210XL256                                                         
ICBESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*ICCCLT*'                                                    
ICCCLT   DS     1500C                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'ICCPOLTB'                                                    
ICCPOLTB DS    256XL4                                                           
ICCPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'ICCESTTB'                                                    
ICCESTTB DS    210XL256                                                         
ICCESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*UCSCLT*'                                                    
UCSCLT   DS     1500C                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'UCSPOLTB'                                                    
UCSPOLTB DS    256XL4                                                           
UCSPOLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'UCSESTTB'                                                    
UCSESTTB DS    210XL256                                                         
UCSESTTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*UC3CLT*'                                                    
UC3CLT   DS     1500C                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'UC3POLTB'                                                    
UC3POLTB DS    256XL4                                                           
UC3POLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'UC3ESTTB'                                                    
UC3ESTTB DS    210XL256                                                         
UC3ESTTX EQU   *                                                                
         EJECT                                                                  
*============================================================*                  
* STATION AND MARKET TABLES                                  *                  
*============================================================*                  
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'*PRDTAB*'                                                    
PRDTAB   DS    XL256                                                            
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'ATSTATAB'                                                    
ATSTATAB DS    30000C              5000 6 BYTE ENTRIES                          
ATSTATBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'ATMKTTAB'                                                    
ATMKTTAB DS    2000C               500 4 BYTE ENTRIES (RTGSVC/AGY)              
ATMKTTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'AGMKTTAB'                                                    
AGMKTTAB DS    2000C               500 4 BYTE ENTRIES (ATT/RTGSVC)              
AGMKTTBX EQU   *-1                                                              
         SPACE 1                                                                
*============================================================*                  
* COMMERCIAL TABLES                                          *                  
* BUILD/SEARCH ROUTINES ASSUME COUNT GOES AT TABLE-4         *                  
*============================================================*                  
         SPACE 1                                                                
MAXCMLS  EQU   1000                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*AGCMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
AGCMLTB DS     (MAXCMLS)XL10                                                    
AGCMLTBX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'BCSCMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
BCSCMLTB DS    (MAXCMLS)XL10                                                    
BCSCMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'BC3CMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
BC3CMLTB DS    (MAXCMLS)XL10                                                    
BC3CMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CCACMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
CCACMLTB DS    (MAXCMLS)XL10                                                    
CCACMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CCSCMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
CCSCMLTB DS    (MAXCMLS)XL10                                                    
CCSCMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CC3CMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
CC3CMLTB DS    (MAXCMLS)XL10                                                    
CC3CMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CC4CMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
CC4CMLTB DS    (MAXCMLS)XL10                                                    
CC4CMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CPDCMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
CPDCMLTB DS    (MAXCMLS)XL10                                                    
CPDCMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'CP3CMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
CP3CMLTB DS    (MAXCMLS)XL10                                                    
CP3CMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'GBSCMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
GBSCMLTB DS    (MAXCMLS)XL10                                                    
GBSCMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'IBSCMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
IBSCMLTB DS    (MAXCMLS)XL10                                                    
IBSCMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'IB3CMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
IB3CMLTB DS    (MAXCMLS)XL10                                                    
IB3CMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'IB4CMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
IB4CMLTB DS    (MAXCMLS)XL10                                                    
IB4CMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'ICBCMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
ICBCMLTB DS    (MAXCMLS)XL10                                                    
ICBCMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'ICCCMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
ICCCMLTB DS    (MAXCMLS)XL10                                                    
ICCCMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'ICSCMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
ICSCMLTB DS    (MAXCMLS)XL10                                                    
ICSCMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'IC3CMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
IC3CMLTB DS    (MAXCMLS)XL10                                                    
IC3CMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'NBACMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
NBACMLTB DS    (MAXCMLS)XL10                                                    
NBACMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'UCSCMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
UCSCMLTB DS    (MAXCMLS)XL10                                                    
UCSCMLTX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'UC3CMLTB'                                                    
         DC    2F'0'               SPARE/COMMERCIAL COUNT                       
UC3CMLTB DS    (MAXCMLS)XL10                                                    
UC3CMLTX EQU   *                                                                
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'TOTBUFF'                                                     
TOTBUFF  DS    800D                                                             
         EJECT                                                                  
       ++INCLUDE DDCNTRL                                                        
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STAMASTD DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
CMLRECD  DSECT                                                                  
       ++INCLUDE SPTRCMML                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
SPWORKD  DSECT                                                                  
         ORG   P                                                                
ERRKEY   DS    CL30                                                             
ERRMSG   DS    CL102                                                            
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096SPREPXA02 03/24/15'                                      
         END                                                                    
