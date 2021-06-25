*          DATA SET NEWRI69    AT LEVEL 004 AS OF 03/14/18                      
*          DATA SET NEWRI69    AT LEVEL 113 AS OF 02/29/96                      
*PHASE T32069A,*                                                                
*INCLUDE MOBILE                                                                 
*INCLUDE GETPROF                                                                
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE GETBROAD                                                               
         TITLE 'T32069 - JOHNSON WAX INVOICE TAPE'                              
T32069   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NW69**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)       RA=2ND BASE REGISTER                           
         LA    RA,1(RA)                                                         
         USING T32069,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          R7-ANETWS2+ANETWS3/WORKING STORAGE           
         LA    R7,300(R7)             (ACTUALLY ANETWS2+300 =WRKSTRG)           
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         L     R5,ANETWS4          R5-NDDEMBLOCK=ANETWS4                        
         ST    R5,NBADEM           ANETWS1=CLIENT RECORD                        
         USING NDDEMBLK,R5                                                      
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R1,BOOKVAL                                                       
         A     R1,RELO                                                          
         ST    R1,ANTWKTP                                                       
         L     R1,=A(MYIO)                                                      
         ST    R1,AMYIO                                                         
         L     R1,=A(MYIO2)                                                     
         ST    R1,AMYIO2                                                        
*                                                                               
SKIPB    CLI   MODE,VALREC                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
         CLI   MODE,RUNLAST                                                     
         BE    LASTRUN                                                          
         CLI   MODE,RUNFRST                                                     
         BE    FIRSTRUN                                                         
EXIT     XIT1                                                                   
         EJECT                                                                  
* - RUNFIRST                                                                    
FIRSTRUN DS    0H                                                               
         L     R1,ATWA                                                          
         MVI   29(R1),2            SET PASS RUNLAST                             
         B     EXIT                                                             
                                                                                
                                                                                
* - CLOSES TAPE                                                                 
LASTRUN  DS    0H                                                               
         L     R2,ANTWKTP             IF WE WROTE TAPE                          
         CLC   =X'90EC',0(R2)                                                   
         BE    LASTRX                                                           
         CLOSE ((R2),)                CLOSE IT                                  
LASTRX   B     EXIT                                                             
         EJECT                                                                  
*************************************                                           
* VALIDATE REQUEST SCREEN DATA                                                  
*                                                                               
VK       DS    0H                                                               
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
         MVI   NBQINIT,0                                                        
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
*                                                                               
* - CLIENT                                                                      
         LA    R2,SPLCLIH                                                       
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
         L     R3,NBAIO                MOVE CLIENT REC TO ANETWS1               
         L     RF,ANETWS1                                                       
         MOVE  ((RF),1250),0(R3)                                                
         L     RF,ANETWS1                                                       
         USING CLTHDR,RF                                                        
         MVC   NETID,CNETID                                                     
         LA    RF,CLIST                AND SET CLIST ADD TO ACLISTSV            
         ST    RF,ACLISTSV                                                      
         DROP  RF                                                               
                                                                                
* - PRODUCT                                                                     
         LA    R2,SPLPROH                                                       
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
                                                                                
* - ESTIMATE                                                                    
         LA    R2,SPLESTH                                                       
         NETGO NVESTRNG,DMCB,SPLESTN                                            
                                                                                
* - START/END DATES                                                             
* - USE START/END DATE ROUTINE FOR INVOICE DATE FILTER                          
* - NEMEDGEN FILLS USERQSTR/END FOR SSPEC                                       
         LA    R2,SPLSDTH          INVOICE START DATE                           
         NETGO NVSTRDAT,DMCB                                                    
         MVC   STRDAT,NBSELSTR                                                  
         GOTO1 DATCON,DMCB,(0,STRDAT),(2,BSTRDAT)                               
*                                                                               
         LA    R2,SPLEDTH          END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
         MVC   ENDDAT,NBSELEND                                                  
         GOTO1 DATCON,DMCB,(0,ENDDAT),(2,BENDDAT)                               
                                                                                
* - NOW SET UNIT START/END PARAMETERS                                           
* MOBILE USES THESE DATES TO BUILD LIST SO THERE IS A MAX                       
* LET'S MAKE IT SOFT                                                            
***      MVC   NBSELSTR,=C'910101'                                              
***      MVC   NBSELEND,=C'961231'                                              
         GOTO1 DATCON,DMCB,(5,0),(3,WORK)       TODAY'S DATE BINARY             
         ZIC   R1,WORK                                                          
         LA    R1,1(R1)                         BUMP TODAY BY ONE YEAR          
         STC   R1,WORK                                                          
         GOTO1 DATCON,DMCB,(3,WORK),(0,NBSELEND) TODAY+1YEAR=NBSELEND           
                                                                                
* NBSELSTR = NBSELEND-5 YEARS                                                   
         PRINT GEN                                                              
         GOTO1 ADDAY,DMCB,NBSELEND,NBSELSTR,F'-1780'                            
         PRINT NOGEN                                                            
                                                                                
* - TAPE OPTION                                                                 
         MVI   TAPEOPT,C'N'                                                     
         MVI   ERROR,INVALID                                                    
         LA    R2,SPLTAPEH                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    VKEXIT                                                           
         MVC   TAPEOPT,FLD                                                      
         CLI   FLD,C'Y'                                                         
         BE    VKEXIT                                                           
         CLI   FLD,C'N'                                                         
         BE    VKEXIT                                                           
         GOTO1 ERREX                                                            
*                                                                               
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
LR       DS    0H                                                               
* - INITIALIZE SORTER                                                           
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
* - OPEN TAPE                                                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   LR5                                                              
         L     RE,BOOKVAL                                                       
         CLC   =X'90EC',0(RE)                                                   
         BNE   LR5                                                              
         LA    RF,NTWKTP                                                        
         MVC   0(128,RE),0(RF)                                                  
         MVC   DSNAME+13(2),NBSELAGY                                            
         L     R4,ATWA                                                          
         USING T320FFD,R4                                                       
         L     RF,TWADCONS                                                      
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         GOTO1 (RF),DMCB,DDNAME,DSNAME                                          
******   GOTO1 =V(DYNALLOC),DMCB,DDNAME,DSNAME                                  
         L     R2,ANTWKTP                                                       
         OPEN  ((R2),(OUTPUT))                                                  
         B     LR5                                                              
         DROP  R4                                                               
                                                                                
DDNAME   DC    CL8'NTWKTP'                                                      
DSNAME   DC    CL20'NETTAPE.NE0NKAA1'                                           
DCBOPEN  DC    C'N'                                                             
                                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,19,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=300'                                   
*                                                                               
                                                                                
* - SET UP NETIO READ PARAMETERS                                                
LR5      DS    0H                                                               
         MVI   NBDATA,C'U'                UNITS ONLY                            
         OI    NBSPLOPT,X'C0'             SPLIT                                 
*        MVI   NBSELUOP,C'B'              ESTIMATED+ACTUAL SCHEDULE             
         MVI   NBSELUOP,0                 ESTIMATED+ACTUAL SCHEDULE             
         MVI   NBUSER+13,C'N'      OVERRIDE PRF,NO FILT PREEMPTS                
         MVI   NBSEQ,C'P'                 ESTIMATE HIGH (X'94' KEY)             
         LA    R1,DOUNIT                  UNIT HOOK                             
         ST    R1,NBHOOK                                                        
         L     RF,=A(PERLIST)                                                   
         XC    0(4,RF),0(RF)                                                    
* - SPOT00 PROFILE SETS UP START/END DATE TABL FOR MOS CALLS                    
****     GOTO1 =A(OVERFLOW),DMCB,(0,(RC))        0=SETDATE                      
*                                                                               
LR10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BNE   LR10                                                             
* - REQUEST LAST                                                                
         BAS   RE,DOTAPE                 DO TAPE/REPORT                         
         CLI   TAPEOPT,C'Y'              ,,IF WRITING TAPE                      
         BNE   EXIT                                                             
         OC    NETID,NETID               ,,IF NETID NOT = 0                     
         BZ    EXIT                      UPDATE CLIENT REC WITH NETID           
         NETGO NVSETSPT                                                         
         LA    R2,KEY                                                           
         USING CLTHDR,R2                                                        
         XC    KEY,KEY                                                          
         L     R3,ANETWS1                                                       
         MVC   KEY(13),0(R3)                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,NBAIO                                                        
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDIR'),=C'SPTFILE ',KEY+14,AIO          
         L     R2,AIO                                                           
         USING CLTHDR,R2                                                        
         MVC   CNETID,NETID        SET LATEST NET ID                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO                   
         CLI   8(R1),0             OK                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*  PROCESS UNIT / PUT TO SORTER                                                 
*                                                                               
DOUNIT   NTR1                                                                   
         MVI   NBUPUNIT,C'N'                                                    
         MVI   NBNOWRIT,C'N'                                                    
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
* - NEW CLIENT                                                                  
         CLI   NBMODE,NBVALCLI                                                  
         BNE   DOUNT0                                                           
         DC    H'0'                REQUEST FOR ONE CLI ONLY                     
**       L     RE,NBAIO            MOVE CLIENT REC TO ANETWS1                   
**       L     RF,ANETWS1                                                       
**       MOVE  ((RF),1250),(RE)                                                 
**       L     RF,ANETWS1                                                       
**       USING CLTHDR,RF                                                        
**       LA    RF,CLIST            AND SET CLIST ADDRESS                        
**       ST    RF,ACLISTSV                                                      
                                                                                
* - NEW CLIENT MEANS NEW SPOT00 PROFILE - GET START/END DATE TABLE              
**       GOTO1 =A(OVERFLOW),DMCB,(0,(RC))        0=SETDATE                      
**       DROP  RF                                                               
                                                                                
DOUNT0   DS    0H                                                               
* - UNITS ONLY                                                                  
         CLI   NBMODE,NBPROCUN                                                  
         BNE   EXIT                                                             
                                                                                
         L     RF,=A(PERLIST)      START/END DATE TABLE                         
         CLC   0(4,RF),=4X'00'     DO I ALREADY HAVE IT                         
         BNE   SKIPOV                                                           
         GOTO1 =A(OVERFLOW),DMCB,(0,(RC))        0=SETDATE                      
                                                                                
* - CLEAR SORTREC AREA                                                          
SKIPOV   LA    RE,SORTREC                                                       
         L     RF,=F'300'                                                       
         XCEF                                                                   
                                                                                
* - NEXT INSTRUCTION BECAUSE SOMETIMES NETIO PASSES UNIT                        
* - WITH NBSPLPRN=0/ MUST BE BUG/ CHECK IT OUT                                  
         CLI   NBSPLPRN,0          IF NO PROD                                   
         BNE   *+12                                                             
         MVI   CURPRD1,0          CLEAR FOR NEXT LOOKUP                         
         B     EXIT                AND EXIT                                     
         CLC   NBPRD,NBSPLPRN      MUST MATCH ONE OF PRODS                      
         BE    DOUNT1                                                           
         CLC   NBPRD2,NBSPLPRN                                                  
         BE    DOUNT1                                                           
         MVC   CURPRD1,NBSPLPRN    X'FF'/SET SO FORCES LOOKUP NEXTIME           
         B     EXIT                                                             
DOUNT1   CLC   CURPRD1,NBSPLPRN                PRODUCT                          
         BE    DOUNT2                                                           
         BAS   RE,GETPRD                                                        
         MVC   CURPRD1,NBSPLPRN                                                 
DOUNT2   CLC   CUREST,NBACTEST                 ESTIMATE                         
         BE    DOUNT7                                                           
         BAS   RE,GETEST                    GET EST NAME                        
         MVC   CUREST,NBACTEST                                                  
         EJECT                                                                  
DOUNT7   DS    0H                                                               
* - GET BILLING ELEMENT                                                         
         CLI   RELOCFLG,C'Y'       IF IN REALLOC SEARCH                         
         BNE   DOUNT8                                                           
         MVC   PRDSAVE,NBSPLPRN     SAVE PROD FOR NETIO SEQUENCING              
                                                                                
DOUNT8   L     R6,NBAIO                                                         
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+12                                                             
DOUNT10  MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   DOUNTX                                                           
         USING NUBILEL,R6                                                       
                                                                                
         CLI   RELOCFLG,C'Y'     IF REALLOCATED PROD SEARCH                     
         BNE   DOUNT20                                                          
         CLC   NUBILPRD,NBPRD    LOOK FOR BILL PRODS NOT = UNIT PROD            
         BE    DOUNT10                                                          
         CLC   NUBILPRD,NBPRD2                                                  
         BE    DOUNT10                                                          
         MVC   NBSPLPRN,NUBILPRD   GOT ONE   - FUDGE NBSPLPRN                   
         BAS   RE,GETPRD           GET 3CHAR PROD AND PROCESS                   
                                                                                
DOUNT20  CLC   NUBILPRD,NBSPLPRN       CHECK BILL PRD = UNIT PRD                
         BNE   DOUNT10                                                          
         MVC   CURRTYP,=C'TIME'    GET TYPE                                     
         CLI   NUBILTYP,C'T'                                                    
         BE    *+10                                                             
         MVC   CURRTYP,=C'INTG'                                                 
         MVC   CURRAMT,NUBILGRS    AND AMOUNT                                   
                                                                                
                                                                                
* - FILTER ON BILL ELEMENT BILLING DATE                                         
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,RUNDATE)                             
         CLC   RUNDATE,STRDAT                                                   
         BL    DOUNT10                                                          
         CLC   RUNDATE,ENDDAT                                                   
         BH    DOUNT10                                                          
                                                                                
         MVC   INVNUMBR(2),RUNDMM               MONTH (2)                       
         MVC   INVNUMBR+2(4),NUBILNUM           INVNO (4)                       
                                                                                
         MVC   WORKINV,INVNUMBR         ..GETBYMN RETURNS                       
         MVC   WORKYMD,RUNDATE                                                  
         BAS   RE,GETBYMN                                                       
         MVC   CURRBINV,WORK            ..3 BYTE(BINARY Y/M + INV NO)           
         MVC   CURBELEM,NUBILEL       SAVE CURRENT BILLING ELEMENT              
         MVC   CURRPRD,NBSPLPRN       SAVE PROD OF CURR BILL ELEMENT            
         BAS   RE,GETMOS           ..GET MO OF SERVICE OF NBACTDAT              
*                                  .. RETURNED IN BYMOS YR/MOS                  
*                                                                               
         BAS   RE,CHKIT            IS THERE BILL REC FOR THIS ELEM              
         BE    DOUNT30                                                          
         BAS   RE,PRNTERR          NO/PRINT AS ERROR                            
         B     *+8                                                              
DOUNT30  BAS   RE,DOSORT           YES/PUT TO SORTER                            
         MVI   NBFUNCT,NBFRDHI                                                  
         B     DOUNT10                                                          
*                                                                               
* - END OF BILL ELEMENTS                                                        
* - NOW SET RELOCFLG AND SEARCH BILL ELEMENT PRODS NOT = UNIT PRD               
* - THIS HAPPENS WHEN UNIT REALLOCATED AFTER BILLING                            
DOUNTX   DS    0H                                                               
         CLI   RELOCFLG,C'Y'     AM I IN MIDDLE OF REALLOCAT PRODS              
         BNE   DOUNTX20                                                         
         MVI   RELOCFLG,0         .YES/CLEAR FLAG                               
         MVC   NBSPLPRN,PRDSAVE    RESET PRODUCT FOR NETIO                      
         B     DOUNTX30            AND CONTINUE PROCESSING                      
                                                                                
DOUNTX20 CLI   NBPRD2,0           .NO/ IF LAST TIME FOR THIS UNIT               
         BE    *+14                                                             
         CLC   NBPRD2,NBSPLPRN                                                  
         BNE   EXIT                                                             
         MVI   RELOCFLG,C'Y'       SET REALLOCATED SEARCH FLAG                  
         B     DOUNT7              AND GO THROUGH BILL ELEMS ONCE MORE          
                                                                                
* - ADD UNIQUE ID ELEMENT TO UNIT                                               
DOUNTX30 CLI   NEWID,C'Y'          IF NEW UNIQUE ID                             
         BNE   EXIT                                                             
         L     R6,NBAIO            CHECK TO STOP MULTIPLE ELEMS                 
         MVI   ELCODE,5                                                         
         BAS   RE,GETEL                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    ELEM,ELEM            ADD UNIQUE ID ELEM                          
         MVI   ELEM,5                                                           
         MVI   ELEM+1,10                                                        
         MVC   ELEM+2(4),NETID                                                  
         GOTO1 ADDELEM                                                          
         MVI   NEWID,0                                                          
         CLI   TAPEOPT,C'Y'        IF WRITING TAPE                              
         BNE   EXIT                                                             
         MVI   NBUPUNIT,C'Y'        WRITE BACK UNIT                             
         MVI   NBNOWRIT,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CHKIT    NTR1             SEE IF BILL HEADER EXISTS FOR BILLING ELEM            
         LA    R2,KEY                                                           
         USING BILLREC,R2                                                       
         XC    BKEY,BKEY           BUILD BILL REC KEY                           
         MVC   BKEYAM,NBACTAM      FROM UNIT BILLING ELEM                       
         MVC   BKEYCLT,NBACTCLI                                                 
         MVC   BKEYPRD,CURPRD3                                                  
         MVC   BKEYEST,NBACTEST                                                 
         MVC   BKEYYSRV,BYMOS              BYMOS SET FROM NBACTDAT              
         MVC   BKEYMSRV,BYMOS+1                                                 
         MVC   BKEYMBIL(3),CURRBINV        M/Y + 2 BYTE INV NO                  
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                         ..DOES BILL REC EXIST FOR           
         CLC   KEY(13),KEYSAVE              ..THIS BILLING ELEM                 
         BE    CHK10                                                            
* - NO                                                                          
* - TRY UNITS NBACTDAT AS MOS WITHOUT GOING TO MOBILE                           
* - SEE IF THIS WILL FIX                                                        
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(3,MYWORK)                              
         MVC   BYMOS(2),MYWORK                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEYSAVE                                                  
         MVC   BKEYYSRV(2),BYMOS                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CHK10                                                            
*                                                                               
* - CAN NOT MATCH TO BILLING RECORD                                             
* - BUMP MOS UP BY ONE AND TRY                                                  
         LA    R5,12               12 MONTHS LEEWAY                             
RDB18    DS    0H                                                               
         ZIC   R1,BYMOS+1                                                       
         C     R1,=F'12'           ..IS IT DECEMBER                             
         BNL   RDB18A                                                           
         LA    R1,1(R1)            .. NO                                        
         STC   R1,BYMOS+1                                                       
         B     RDB19                                                            
RDB18A   DS    0H                                                               
         ZIC   R1,BYMOS            ..YES/BUMP YEAR                              
         LA    R1,1(R1)                                                         
         STC   R1,BYMOS                                                         
         MVI   BYMOS+1,1           ..AND SET MONTH TO JAN                       
RDB19    XC    KEY,KEY                                                          
         MVC   KEY(13),KEYSAVE                                                  
         MVC   BKEYYSRV(2),BYMOS                                                
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    CHK10                                                            
         BCT   R5,RDB18               TRY AGAIN                                 
                                                                                
* - NO MATCH                                                                    
* - BUMP MOS DOWN BY ONE AND TRY                                                
         LA    R5,12                                                            
         MVI   BYTE,0                                                           
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(3,MYWORK)                              
         MVC   BYMOS(2),MYWORK                                                  
RDB19A   ZIC   R1,BYMOS+1                                                       
         BCTR  R1,0                                                             
         STC   R1,BYMOS+1                                                       
         C     R1,=F'0'            ..WERE WE IN JANUARY                         
         BNE   RDB19B                                                           
         ZIC   R1,BYMOS            ..YES DECREASE YEAR                          
         BCTR  R1,0                                                             
         STC   R1,BYMOS                                                         
         MVI   BYMOS+1,12          ..AND SET MONTH TO DECEMBER                  
RDB19B   XC    KEY,KEY                                                          
         MVC   KEY(13),KEYSAVE                                                  
         MVC   BKEYYSRV(2),BYMOS                                                
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR  ',KEY,KEY                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    CHK10                                                            
         BCT   R5,RDB19A              AND TRY AGAIN                             
*                                                                               
         NETGO NVSETUNT              NO CAN FIND                                
         XC    FILENAME,FILENAME                                                
         LTR   RE,RE                                                            
         B     CHKX                                                             
                                                                                
* - READ THE BILL REC                                                           
CHK10    GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE ',KEY+14,AMYIO2,MYDM          
         NETGO NVSETUNT                        (RESET UNIT READ)                
         XC    FILENAME,FILENAME                                                
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
CHKX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
DOSORT   NTR1                                                                   
         LA    RE,SORTREC                                                       
         LA    RF,SRTRECL                                                       
         XCEF                                                                   
         L     R2,AMYIO2                                                        
         USING BILLREC,R2                                                       
* - SORTER KEY FIELDS                                                           
         MVC   SRTKPRD,CURPRD3              PROD                                
         EDIT  (B1,NBACTEST),(3,SRTKEST)    EST                                 
         MVC   SRTKINV,BINVNO               INVOICE NUMBER                      
* - DATA FIELDS                                                                 
         MVC   SRTPRD,CURPRD3                          PRODUCT                  
         EDIT  (B1,NBACTEST),(3,SRTESTNO),ALIGN=LEFT   EST NUMBER               
         CLI   SRTESTNO+2,X'40'                                                 
         BH    *+8                                                              
         MVI   SRTESTNO+2,0                                                     
         CLI   SRTESTNO+1,X'40'                                                 
         BH    *+8                                                              
         MVI   SRTESTNO+1,0                                                     
         MVC   SRTESTNM,CURESTNM                       EST NAME                 
         MVC   SRTINV,BINVNO                           INVOICE NUMBER           
         BAS   RE,GETBUYID                             UNIQUE UNIT ID           
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         MVC   SRTBUYID,DUB                                                     
         MVC   SRTIDAT(4),BDATE+2                      INVOICE DATE             
         MVC   SRTIDAT+4(2),=C'19'                     MMDDYYYY                 
         CLC   =C'80',BDATE                                                     
         BNH   *+10                                                             
         MVC   SRTIDAT+4(2),=C'20'                                              
         MVC   SRTIDAT+6(2),BDATE                                               
                                                                                
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(0,WORK)       DUE DATE                 
         MVC   SRTDUEDT(4),WORK+2                      MMDDYYYY                 
         MVC   SRTDUEDT+4(2),=C'19'                                             
         CLC   =C'80',WORK                                                      
         BNH   *+10                                                             
         MVC   SRTDUEDT+4(2),=C'20'                                             
         MVC   SRTDUEDT+6(2),WORK                                               
                                                                                
         MVC   SRTDPT,NBACTDP                          DAYPART                  
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(5,WORK)                                
         MVC   SRTMONTH,WORK                           MONTH                    
         MVI   SRTDAY,0                                DAY                      
         MVC   SRTDAY+1(2),WORK+3                      0NN OR 00N               
         CLI   SRTDAY+1,C'0'                                                    
         BNE   *+8                                                              
         MVI   SRTDAY+1,0                                                       
         MVC   SRTPROG,NBACTPRG                        PROGRAM                  
         MVC   SRTPRGNM,NBPROGNM                       PROGRAM NAME             
         LA    R1,SRTPRGNM+15                                                   
         CLI   0(R1),X'40'                                                      
         BH    *+14                                                             
         MVI   0(R1),0                                                          
         BCTR  R1,0                                                             
         B     *-14                                                             
         MVC   SRTNET,NBACTNET                         NETWORK                  
         CLI   SRTNET+3,X'40'                                                   
         BH    *+8                                                              
         MVI   SRTNET+3,0                                                       
         EDIT  (B1,NBLEN),(2,SRTULEN)                  LENGTH                   
         MVC   SRTTYPE,CURRTYP                         TYPE                     
                                                                                
         ICM   R1,15,CURRAMT                           INV AMOUNT               
         CVD   R1,DUB                                                           
         MVC   SRTIAMT,DUB                                                      
         MVI   SRTASIGN,0                               SIGN OF AMOUNT          
         MVC   BYTE,SRTIAMT+7                                                   
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0D'          IF MINUS                                     
         BNE   *+8                                                              
         MVI   SRTASIGN,C'-'                                                    
                                                                                
         L     R6,NBAIO                                ACTIVITY DATE            
         MVI   ELCODE,X'99'                            (MMDDYYYY)               
         BAS   RE,GETEL                                                         
         BNE   DSRT10                                                           
         USING NUACTD,R6                                                        
         CLI   NUACTCDT,0          IF NO CHANGE DATE                            
         BE    DSRT9                                                            
         LA    R6,NUACTCDT                                                      
         B     *+8                                                              
DSRT9    LA    R6,NUACTADT         USE BUY DATE                                 
         GOTO1 DATCON,DMCB,(3,0(R6)),(0,WORK)                                   
         MVC   SRTACTIV(4),WORK+2                                               
         MVC   SRTACTIV+4(2),=C'19'                                             
         CLC   =C'80',WORK                                                      
         BNH   *+10                                                             
         MVC   SRTACTIV+4(2),=C'20'                                             
         MVC   SRTACTIV+6(2),WORK                                               
         DROP  R6                                                               
                                                                                
DSRT10   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     DSRT30                                                           
         GOTO1 HEXOUT,DMCB,SORTREC,P,130                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 HEXOUT,DMCB,CURBELEM,P,30                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R2,NBAIO                                                         
         GOTO1 HEXOUT,DMCB,0(R2),P,20                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
DSRT30   DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        WRITE REPORT / TAPE                                                    
*        R3 POINTS TO REC FROM SORTER                                           
*                                                                               
DOTAPE   NTR1                                                                   
DOT0     LA    RE,SORTREC                                                       
         L     RF,=F'300'                                                       
         XCEF                                                                   
         GOTO1 SORTER,DMCB,=C'GET'       TAKE RECS FROM SORTER                  
         L     R3,4(R1)                                                         
         LA    RF,SORTREC                                                       
         LA    R1,SRTRECL                                                       
         LR    RE,R3                                                            
         MOVE  ((RF),(R1)),(RE)                                                 
         LTR   R3,R3                                                            
         BZ    DOTX                                                             
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         CLC   1(5,R3),=C'ERROR'   IF ERROR REC                                 
         BNE   DOT10                                                            
         MVC   PPRD(60),0(R3)                                                   
         GOTO1 SPOOL,DMCB,(R8)      PRINT/NO TAPE                               
         B     DOT0                                                             
DOT10    DS    0H                                                               
         CLI   ERRFLG,C'Y'          IF ERRORS PRINTED                           
         BNE   *+12                                                             
         MVI   FORCEHED,C'Y'       TOP OF PAGE                                  
         MVI   ERRFLG,0                                                         
                                                                                
* - PATCH TO PRINT SUBTOTALS FOR CHECKING                                       
         B     DOT15                                                            
         CLI   OLDPRD,0                                                         
         BNE   DOT12                                                            
         MVC   OLDPRD,SRTPRD       FIRST TIME                                   
         ZAP   TEMPTOT,=P'0'                                                    
         AP    TEMPTOT,SRTIAMT                                                  
         B     DOT15                                                            
DOT12    CLC   OLDPRD,SRTPRD       NOT FIRST TIME/HAS PROD CHANGED              
         BE    DOT14                                                            
         MVC   P+10(3),OLDPRD      YES/PRINT PROD/TOTALS                        
         EDIT  (P8,TEMPTOT),(15,P+15),2                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         ZAP   TEMPTOT,=P'0'                                                    
         MVC   OLDPRD,SRTPRD                                                    
DOT14    AP    TEMPTOT,SRTIAMT                                                  
                                                                                
DOT15    MVC   PPRD,SRTPRD                                                      
         MVC   PEST,SRTESTNO                                                    
         MVC   PINV(6),SRTINV                                                   
         EDIT  (P8,SRTBUYID),(9,PUNIQUE),ALIGN=LEFT                             
         MVC   PINVDAT,SRTIDAT                                                  
         MVC   PDUEDAT,SRTDUEDT                                                 
         MVC   PDPT,SRTDPT                                                      
         MVC   PMONTH,SRTMONTH                                                  
         MVC   PDAY,SRTDAY                                                      
         MVC   PPROG,SRTPROG                                                    
         MVC   PNET,SRTNET                                                      
         MVC   PUNLEN,SRTULEN                                                   
         MVC   PTYPE,SRTTYPE                                                    
         LA    R3,PIAMT                                                         
         LA    R4,SRTIAMT+2                                                     
         EDIT  (P6,0(R4)),(12,0(R3)),2,MINUS=YES                                
         MVC   PLSTACT,SRTACTIV                                                 
         BAS   RE,SPOOLIT                                                       
DOT30    BAS   RE,WRITAPE                                                       
         B     DOT0                                                             
DOTX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
WRITAPE  NTR1                                                                   
         L     R1,ANTWKTP                                                       
         LA    RE,RECWORK                                                       
         L     RF,=F'300'                                                       
         XCEF                                                                   
         LA    RF,RECWORK                                                       
         LA    R1,SRTRECL                                                       
         LA    RE,SRTPRD                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
         CLI   TAPEOPT,C'Y'                                                     
         BNE   WRT10                                                            
         L     R1,ANTWKTP                                                       
         PUT   (R1),RECWORK                   WRITE TAPE                        
WRT10    B     EXIT                                                             
         GOTO1 HEXOUT,DMCB,RECWORK,P,250                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
*******************************                                                 
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
         SPACE 3                                                                
**********************************                                              
*  TO GET PRD CODE FROM C LIST                                                  
*                                                                               
GETPRD   NTR1                                                                   
         L     R2,ACLISTSV                                                      
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                                                             
         MVC   CURPRD3,=C'UNA'    SET TO UNDEFINED                              
         B     GPX                                                              
GP12     CLC   NBSPLPRN,3(R2)                                                   
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   CURPRD3,0(R2)      SET 3 CHAR PRINTABLE PRD CODE                 
*                                                                               
GPX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* EACH UNIT REC FOR JOHNSON HAS UNIQUE BUY ID ELEMENT                           
* LATEST ID NUMBER LIVES ON CLIENT RECORD                                       
*                                                                               
GETBUYID NTR1                                                                   
         L     R6,NBAIO            ...DOES UNIT HAVE ID NUMBER                  
         MVI   ELCODE,5                                                         
         BAS   RE,GETEL                                                         
         BNE   GB10                                                             
         USING NUIDEL,R6                                                        
         MVC   FULL,NUID           ...YES/PASS BACK ID NUMBER                   
         B     GBX                                                              
GB10     CLI   NEWID,C'Y'          NO/BUT DID I ALREADY ASSIGN ID               
         BE    GBX                    TO THIS UNIT                              
         L     R4,NETID            NO/GET LAST UNIQE ID NUMBER                  
         LA    R4,1(R4)            BUMP IT                                      
         ST    R4,NETID                 SAVE LATEST ID NUMBER                   
         MVC   FULL,NETID               PASS IT BACK                            
         MVI   NEWID,C'Y'               SET ELEM ADD SWITCH                     
GBX      XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H10                                                           
         USING PLINED,R2                                                        
         CLI   ERRFLG,C'Y'          IF ERRORS                                   
         BNE   HD10                                                             
         LA    R1,PPRD+1                                                        
         MVC   0(42,R1),=C'ERROR TABLE - NO MATCH UNIT TO BILLING REC'          
         BE    HDX                 NO HEADS/BOXES                               
HD10     MVC   PPRD,=C'PRD'                                                     
         MVC   PEST,=C'EST'                                                     
         MVC   PINV,=C'INVOICE'                                                 
         MVC   PUNIQUE(7),=C'UNIT ID'                                           
         MVC   PINVDAT,=C' INVOICE'                                             
         MVC   PINVDAT+133(8),=C'  DATE  '                                      
         MVC   PDUEDAT,=C'DUE DATE'                                             
         MVC   PDPT-1(3),=C'DPT'                                                
         MVC   PMONTH(7),=C'MON/DAY'                                            
         MVC   PPROG,=C'PROGRM'                                                 
         MVC   PNET,=C'NET '                                                    
         MVC   PIAMT+1(12),=C'   INVOICE  '                                     
         MVC   PIAMT+133(12),=C'    AMOUNT  '                                   
         MVC   PLSTACT,=C'ACTIVITY'                                             
         MVC   PLSTACT+134(4),=C'DATE'                                          
         DROP  R2                                                               
*                                                                               
         DS    0H                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R4,BOXCOLS                                                       
         USING PLINED,R4                                                        
         MVI   PSTR,C'L'                                                        
         MVI   PEST-1,C'C'                                                      
         MVI   PINV-1,C'C'                                                      
         MVI   PUNIQUE-1,C'C'                                                   
         MVI   PINVDAT-1,C'C'                                                   
         MVI   PDUEDAT-1,C'C'                                                   
         MVI   PDPT-2,C'C'                                                      
         MVI   PMONTH-1,C'C'                                                    
         MVI   PPROG-1,C'C'                                                     
         MVI   PNET-1,C'C'                                                      
         MVI   PUNLEN-1,C'C'                                                    
         MVI   PLSTACT-1,C'C'                                                   
         MVI   PEND,C'R'                                                        
         SPACE                                                                  
         LA    R4,BOXROWS                                                       
         LA    R4,8(R4)                                                         
         MVI   0(R4),C'T'                                                       
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'M'                                                       
         LA    R4,49(R4)                                                        
         MVI   0(R4),C'B'                                                       
HDX      B     EXIT                 (XIT1)                                      
         DROP  R1,R4                                                            
         EJECT                                                                  
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C' NETWORK MEDIA TAPE REPORT'                              
         SSPEC H2,52,C' -------------------------'                              
         SSPEC H3,52,PERIOD                                                     
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
NTWKTP   DCB   DDNAME=NTWKTP,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00114,                                            X        
               BLKSIZE=11400,                                          X        
               MACRF=PM                                                         
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         EJECT                                                                  
* - READ EST RECORD / RETURNS EST NAME                                          
GETEST   NTR1                                                                   
         XC    KEY,KEY                                                          
         NETGO NVSETSPT                                                         
         LA    R2,KEY                                                           
         USING ESTHDR,R2                                                        
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,NBACTEST                                                 
         BAS   RE,DOHIGH                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,KEY+14                                                        
         DROP  R2                                                               
         L     R4,AMYIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL  ',(R3),(R4),MYDM              
         USING ESTHDR,R4                                                        
         MVC   CURESTNM,EDESC                                                   
         LA    R2,CURESTNM+19                                                   
         CLI   0(R2),X'40'         TRAILING BLANKS BECOME ZERO                  
         BH    *+14                                                             
         MVI   0(R2),0                                                          
         BCTR  R2,0                                                             
         B     *-14                                                             
         MVI   USEIO,C'N'                RESET UNTFILE VALUES                   
         XC    FILENAME,FILENAME                                                
         NETGO NVSETUNT                                                         
         MVI   NBFUNCT,NBFRDHI                                                  
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
DOHIGH   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         B     EXIT                                                             
*                                                                               
GETMOS   DS    0H                                                               
         L     RF,=A(PERLIST)                                                   
GETM4    DS    0H                                                               
         CLC   NBACTDAT,2(RF)      TEST DATE VS START                           
         BNL   *+6                                                              
         DC    H'0'                TIME TO LENGTHEN PERLIST                     
         BE    GETM8                                                            
         CLC   NBACTDAT,4(RF)                                                   
         BNH   GETM8                                                            
         LA    RF,6(RF)            NEXT ENTRY                                   
         B     GETM4               NOTE- LIST ENDS IN FF                        
GETM8    DS    0H                                                               
         MVC   BYMOS,0(RF)         SET YR/MOS                                   
         BR    RE                                                               
*                                                                               
                                                                                
                                                                                
* INPUT:   WORKINV=MONTH(2BYTES)+INVNO(4BYTES)                                  
*          WORKYMD=YYMMDD OF BILLING RUN DATE                                   
*                                                                               
* OUTPUT:  3BYTE 1(Y/M) + 2(INVNO)                                              
GETBYMN  NTR1                                                                   
         PACK  DUB,WORKINV+2(4)   .GET BINARY VALUE OF INVNO                    
         CVB   R1,DUB                                                           
         STCM  R1,3,WORK+1                                                      
* - CONVERT BILLING RUN MONTH TO BINARY                                         
         PACK  DUB,WORKINV(2)     .GET BINARY VALUE OF MONTH                    
         CVB   R1,DUB                                                           
         STCM  R1,1,WORK                                                        
* GET YEAR INTO ZONE OF MONTH BYTE                                              
         PACK  BYTE,WORKYMD+1(1)   .SWITCH FN TO NF  (YYMMDD)                   
         NI    BYTE,X'F0'          .MAKE IT N0                                  
         OC    WORK(1),BYTE        .SET IT INTO FIRSTHALF OF MONTH BYTE         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* - PRINT ELEMENTS FOR WHICH THERE IS NO BILL RECORD MATCH                      
PRNTERR  NTR1                                                                   
         L     R2,NBAIO                                                         
         LA    R3,SORTREC                                                       
         MVI   0(R3),0                                                          
         MVC   1(5,R3),=C'ERROR'                                                
         MVC   7(3,R3),NBCLICOD                                                 
         MVC   11(3,R3),CURPRD3                                                 
         EDIT  (B1,NBACTEST),(3,15(R3))                                         
         MVC   20(4,R3),NBACTNET                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,25(R3))                              
         MVI   32(R3),C'-'                                                      
         EDIT  (B1,NBACTSUB),(3,33(R3)),ALIGN=LEFT                              
         B     PRNT10                                                           
* - PATCH TO GIVE HEX WHEN TROUBLE SHOOTING                                     
         GOTO1 HEXOUT,DMCB,0(R2),7(R3),40                                       
         GOTO1 HEXOUT,DMCB,CURBELEM,50(R3),48                                   
PRNT10   GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   ERRFLG,C'Y'                                                      
PRNTX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
OVERFLOW NMOD1 0,*N32OVFL,RA                                                    
         L     RC,0(R1)            REESTABLISH WORKING STORAGE                  
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2         ANETWS1=WORKING STORAGE                       
         LA    R7,300(R7)                                                       
         USING WORKD,R7                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVFLOW(RF)                                                       
XIT2     XIT1                                                                   
*                                                                               
OVFLOW   B     SETDATE                                                          
         SPACE 2                                                                
*                                                                               
SETDATE  DS    0D                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'B3'           GET B3 PROFILE                        
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..IF FILTERING                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 =V(GETPROF),DMCB,(0,WORK),MYWORK2,(0,(R2))                       
*                                                                               
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=C'SP'                                                   
         MVC   WORK+2(2),=C'00'                                                 
         MVC   WORK+4(2),NBEFFAGY         AGENCY ALPHA                          
         MVC   WORK+6(1),NBSELMFL         ..MEDIA FILTER                        
         CLI   NBSELMFL,0                                                       
         BNE   *+10                                                             
         MVC   WORK+6(1),NBSELMED         ..ELSE SET IT TO NETWORK              
         MVC   WORK+7(3),NBCLICOD         CLIENT                                
         CLI   NBEFFOFF,X'40'                                                   
         BH    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),NBEFFOFF        OFFICE                                
         L     R2,DATAMGR                                                       
         L     R3,ATWA                                                          
         XC    DMCB(16),DMCB                                                    
         GOTO1 =V(GETPROF),DMCB,(0,WORK),MYWORK,(0,(R2))                        
* - SET B3 PROFILE VALUES INTO 00 DATE AREAS                                    
         MVC   MYWORK+2(1),MYWORK2                                              
         MVC   MYWORK+6(3),MYWORK2+1                                            
*                                                                               
         IC    R0,MYWORK+2       DATE CONTROL                                   
*                                BUILD LONG LIST OF DATE PAIRS                  
         L     R2,=A(MYIO)                                                      
         LA    R4,MYWORK         PASS ADDRESS OF 00 PROFILE                     
         PRINT GEN                                                              
         GOTO1 =V(MOBILE),DMCB,(208,NBSELSTR),((R0),(R2)),,(R4)                 
         PRINT NOGEN                                                            
*                                  FIND FIRST PERIOD OF A NEW YEAR              
SETD4    DS    0H                                                               
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD6               YES                                          
         LA    R2,4(R2)                                                         
         B     SETD4                                                            
*                                  BUILD  A LIST OF YM, START-END               
SETD6    DS    0H                                                               
         L     R3,=A(PERLIST)                                                   
SETD7    DS    0H                                                               
         ZIC   R0,2(R2)                                                         
         SRL   R0,1                                                             
         STC   R0,BYTE             YEAR                                         
         SR    R4,R4               FOR PER SEQUENCE WITHIN YR                   
SETD8    DS    0H                                                               
         LA    R4,1(R4)                                                         
         MVC   0(1,R3),BYTE        YEAR                                         
         STC   R4,1(R3)            MONTH                                        
         MVC   2(4,R3),0(R2)       START-END OF PER                             
         LA    R3,6(R3)                                                         
         LA    R2,4(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BE    SETD12              EOL                                          
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BZ    SETD7               YES                                          
         B     SETD8                                                            
*                                                                               
SETD12   DS    0H                                                               
*                                                                               
SETDATEX DS    0H                                                               
         B     XIT2                                                             
         SPACE 1                                                                
*                                  FIND START OF NEW YEAR                       
*                                  1) A PERIOD THAT SPANS YEAR CHANGE           
*                                     AND BEGINS NO FURTHER AWAY                
*                                     FROM 12/31 THAN IT ENDS                   
*                             OR   2) A PERIOD THAT STARTS BEFORE 1/14          
*                                                                               
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R2)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PERLIST  DS    XL(15*13*6+1)       15YRS X 13 MNTHS X 6                         
*                                  MOS(2) + START(2) + END(2)                   
MYIO     DS    CL2000                                                           
         DS    0D                                                               
MYIO2    DS    CL1000                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2                         
*                                                                               
*                                                                               
RELO     DS    F                                                                
MYDM     DS    CL96                                                             
ACLISTSV DS    F                                                                
ANTWKTP  DS    F                                                                
AMYIO    DS    A                                                                
AMYIO2   DS    A                                                                
NETID    DS    F                                                                
TAPEOPT  DS    CL1                                                              
NEWID    DS    CL1            CONTROLS WRITING OF NEW UNIQUE ID ELEM            
*                             TO PREVENT MULTIPLE IDS ON ONE UNIT               
*                                                                               
ERRFLG   DS    CL1            Y=YES WE HAVE NO-MATCH ERRORS                     
RELOCFLG DS    CL1            Y=REALLOCATED UNIT AFTER BILLING SEARCH           
PRDSAVE  DS    CL1                                                              
OLDPRD   DS    CL3                                                              
TEMPTOT  DS    PL8                                                              
*                                                                               
*                                                                               
CUREST   DS    CL1                                                              
CURESTNM DS    CL20                                                             
CURESTST DS    CL8                                                              
CURESTND DS    CL8                                                              
CURPRD1  DS    CL1                                                              
CURPRD3  DS    CL3                                                              
CURPROG  DS    CL6                                                              
CURPRGNM DS    CL16                                                             
CURDEMNM DS    CL10                                                             
*                                                                               
CURRPRD  DS    CL1         CURRENT WORKING PROD OF BILLING ELEM                 
CURRBINV DS    CL3         CURRENT WORKING BINARY Y/M + INVNO                   
STRDAT   DS    CL6                 YYMMDD                                       
ENDDAT   DS    CL6                 YYMMDD                                       
BSTRDAT  DS    CL2                 YMD COMPRESSED                               
BENDDAT  DS    CL2                 YMD                                          
BYMOS    DS    CL2                 BILLING YR/MOS FROM GETMOS                   
RUNDATE  DS    0CL6                                                             
RUNDYY   DS    CL2                                                              
RUNDMM   DS    CL2                                                              
RUNDDD   DS    CL2                                                              
*                                                                               
INVNUMBR DS    CL6         CURRENT INV NUMBER MONTH(2) + NUMBER                 
INVAMT   DS    CL4                                                              
*                                                                               
INVERROR DS    CL1                                                              
INVDUP   DS    CL1                 INVOICE DUPLICATE                            
CURBELEM DS    CL24          BILLING ELEM SAVE **HARD CODED LENGTH              
INVNUMSV DS    CL6                                                              
CURBTYP  DS    CL1                                                              
ERRORSW  DS    CL1                                                              
WORKINV  DS    CL6                 TEMPORARY INVOICE  WORK AREA                 
WORKYMD  DS    CL6                 TEMPORARY YYMMDD WORK AREA                   
MYKEY    DS    CL20                                                             
MYIIKSV  DS    CL13                                                             
SAVECLT  DS    CL3                                                              
CURRAMT  DS    CL4                                                              
CURRTYP  DS    CL4                                                              
*                                                                               
MYWORK   DS    CL100                                                            
MYWORK2  DS    CL100                                                            
         DS    0D                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
SORTREC  DS    CL300                                                            
         ORG   SORTREC                                                          
SRTKPRD  DS    CL3                                                              
SRTKEST  DS    CL3                                                              
SRTKINV  DS    CL6                                                              
SRTKEYL  EQU   *-SRTKPRD                                                        
SRTPRD   DS    CL3                                                              
SRTCCODE DS    XL1                 COUNTRY CODE=X'00'                           
SRTESTID DS    0CL23                                                            
SRTESTNO DS    CL3                 EST NUMBER                                   
SRTESTNM DS    CL20                ESTNAME                                      
SRTFILL  DS    CL1                 BLANK                                        
SRTINV   DS    CL6                 INVOICE                                      
SRTBUYID DS    PL8                 UNIQUE IDENTIFICATION NO                     
SRTIDAT  DS    CL8                 INVOICE DATE MMDDYYYY                        
SRTDUEDT DS    CL8                 DUE DATE MMDDYYYY                            
SRTDPT   DS    CL1                 DAYPART                                      
SRTMONTH DS    CL3                 MONTH                                        
SRTDAY   DS    CL3                 DAY                                          
SRTPROG  DS    CL6                 PROGRAM                                      
SRTPRGNM DS    CL16                PROGRAM NAME                                 
SRTNET   DS    CL4                 NETWORK                                      
SRTULEN  DS    CL2                 UNIT LENGTH/30/60/ETC                        
SRTTYPE  DS    PL4                 INTG/TIME                                    
SRTASIGN DS    PL1                 0 OR - (COST AMOUNT SIGN)                    
SRTIAMT  DS    PL8                 INVOICE AMOUNT 2 DECIMALS                    
SRTACTIV DS    CL8                 LAST ACTIVITY DATE/MMDDYYYY                  
SRTRECL  EQU   *-SORTREC           LENGTH OF TOTAL SORT REC                     
SRTRECL1 EQU   *-SRTPRD            LENGTH OF REC                                
*                                                                               
RECWORK  DS    CL300                                                            
         EJECT                                                                  
*                                                                               
PLINED   DSECT                    DSECT FOR PRINTED REPORT                      
         DS    CL10                                                             
PSTR     DS    CL1                                                              
         DS    CL1                                                              
PPRD     DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
PEST     DS    CL3                 ESTIMATE                                     
         DS    CL1                                                              
PINV     DS    CL7                 INVOICE  NUMBER                              
         DS    CL2                                                              
PUNIQUE  DS    CL9                 UNIQUE ID                                    
         DS    CL1                                                              
PINVDAT  DS    CL8                 MMDDYYYY                                     
         DS    CL2                                                              
PDUEDAT  DS    CL8                                                              
         DS    CL2                                                              
PDPT     DS    CL1                 DAYPART                                      
         DS    CL2                                                              
PMONTH   DS    CL3                                                              
         DS    CL1                                                              
PDAY     DS    CL3                                                              
         DS    CL2                                                              
PPROG    DS    CL6                                                              
         DS    CL1                                                              
PNET     DS    CL4                                                              
         DS    CL1                                                              
PUNLEN   DS    CL2                 LENGTH                                       
         DS    CL1                                                              
PTYPE    DS    CL4                 TIME/INTG                                    
         DS    CL1                                                              
PIAMT    DS    CL13                INVOICE AMOUNT                               
         DS    CL2                                                              
PLSTACT  DS    CL8                 LAST ACTIVITY                                
         DS    CL1                                                              
PEND     DS    CL1                                                              
PLENE    EQU   *-PLINED                                                         
*                                                                               
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIDFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENBILL                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEWRI69   03/14/18'                                      
         END                                                                    
