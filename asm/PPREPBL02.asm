*          DATA SET PPREPBL02  AT LEVEL 010 AS OF 09/03/15                      
*PHASE PPBL02A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE PPBVAL                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE PRNTOFC                                                                
         SPACE 1                                                                
*                                                                               
*****************    CHANGE LOG    ********************************             
*                                                                               
* BPLA  09/15     SEND NO DATA MESSAGE - DON'T DUMP                             
*                                                                               
* SMYE  11/05     2-CHR MEDIA OFFICE CHANGES                                    
*                                                                               
* SMYE  01/01     ADD *SYSTEM= AND *AGENCY AND *END RECORDS TO                  
*                 DOWNLOAD OUTPUT                                               
*                                                                               
* SMYE  01/01     CHANGE (FROM PROFILES) TO AGENCY HEADERS FOR                  
*                 SKIPPING AND PRODUCT DETAIL INFORMATION                       
*                                                                               
* SMYE  10/18/00  CHANGE TO PACKED ARITHMETIC FOR BILLS GT 21MM                 
*                                                                               
* SMYE  08/21/00  CHANGES FOR LARGER PBILLREC FIELDS (PL5 TO PL6)               
*                                                                               
*=================================================================*             
* CREATE DOWNLOAD FILE OF AGENCY BILLING RECORDS                  *             
*                                                                 *             
* QAGENCY = ALPHA AGENCY TO RUN ONE AGENCY (OR ONE AGENCY/MEDIA)  *             
*               ( **** SEE QOPT1 BELOW **** )                     *             
*                                                                 *             
* QOPT1 = Y TO RUN FOR AGENCY IN QAGENCY FIELD ONLY OR            *             
*           AGENCY/MEDIA IN QAGENCY/QMEDIA FIELDS ONLY            *             
*           ELSE ALL AGENCIES ON THIS PRTFILE                     *             
*                                                                 *             
*=================================================================*             
*                                                                 *             
* ***  FOR SKIPPING AND PRODUCT DETAIL (AGENCY LEVEL ONLY)    *** *             
* ***     AGENCY HEADER RECORDS ARE USED AS FOLLOWS:          *** *             
*                                                                 *             
*         X'01' IN PAGYSTAT MEANS SKIP THIS AGENCY                *             
*         X'02' IN PAGYSTAT MEANS SHOW PRODUCT DETAIL             *             
*                                                                 *             
*=================================================================*             
         TITLE 'PPBL02 - DOWNLOAD DDS BILLING RECORDS'                          
PPBL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPBL02                                                         
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING PPBL02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R8,PPFILEC                                                       
         LA    R9,1(R8)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,R8,R9                                                    
*                                                                               
         XC    DMCB(4),DMCB        GET ADDRESS OF OFFICER                       
         MVC   DMCB+4(4),=X'D9000A38'                                           
         L     RF,VCOMFACS                                                      
         L     RF,(CCALLOV-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB                                                        
         MVC   VOFFICER,DMCB                                                    
*                                                                               
         L     RE,=V(PRNTOFC)                                                   
         ST    RE,VPRNTOFC         STORE PRNTOFC ADDRESS                        
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    BL02                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    BL10                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    BL300                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
BL02     MVI   ACTIVITY,C'N'                                                    
         B     EXIT                                                             
*============================================================*                  
* REQFRST - FIRST BUILD AGENCY DRIVER TABLE                  *                  
*============================================================*                  
BL10     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   HAVEDATA,0                                                       
         XC    THISREC,THISREC                                                  
*                                                                               
         MVC   QSTART+4(2),=C'01'  FORCE THE DAY TO 01                          
         GOTO1 DATCON,DMCB,QSTART,(3,DUB)                                       
         MVC   STRTBILL,DUB        START OF MONTH                               
         MVC   ENDBILL(2),DUB                                                   
         MVI   ENDBILL+2,X'1F'     END OF MONTH                                 
*                                                                               
         MVC   BBILLYM,STRTBILL    SET BILL YEAR/MONTH FILTER                   
*                                                                               
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(3,DUB),(20,WORK)                                    
         MVC   THISBLYM(4),WORK     MOVE YYYY                                   
         MVI   THISBLYM+4,C'/'           /                                      
         MVC   THISBLYM+5(2),WORK+4      MM                                     
         OPEN  (CLTFILE,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    BL11                                                             
         CLI   ACTIVITY,C'Y'                                                    
         BNE   BL10E               JUST EXIT IF NO DATA                         
         DC    H'0'                OTHERWISE DUMP                               
BL10E    MVI   MODE,RUNLAST                                                     
         B     EXIT                                                             
*                                                                               
BL11     DS    0H                  BUILD TABLE OF AGENCIES                      
*                                                                               
         XC    BINPARMS(24),BINPARMS                                            
         XCEFL BINTBL,1800                                                      
*                                                                               
         SR    R0,R0               ADDRS OF INSERT                              
         LA    R1,BINTBL           ADDRS OF TBL                                 
         SR    R2,R2               NUM OF RECS SO FAR                           
         LA    R3,LAGYTAB          L'REC                                        
         LA    R4,3                BYTE 0=KEY DISP,1-3=L'KEY                    
         LA    R5,300              MAX NUM OF RECS                              
         STM   R0,R5,BINPARMS                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY+3,1             AGENCY RECORD CODE                           
         CLI   QOPT1,C'Y'          ONE AGENCY ONLY ?                            
         BNE   BL13                NO - START AT BEGINNING                      
         MVC   KEY(3),QAGENCY      START AT AGENCY/(MEDIA)                      
*                                                                               
BL13     GOTO1 HIGH                                                             
BL13D    CLI   KEY,X'FF'           END OF FILE ?                                
         BE    BL20                YES - DONE                                   
         CLI   KEY,C' '            POSITION 1 BLANK ?                           
         BE    BL19                YES - BYPASS REC                             
         CLI   KEY+1,C' '          POSITION 2 BLANK ?                           
         BE    BL19                YES - BYPASS REC                             
         CLI   KEY+2,C' '          POSITION 3 BLANK ?                           
         BE    BL19                YES - BYPASS REC                             
         CLI   KEY+3,1             AGENCY RECORD ?                              
         BNE   BL18                NO  - NEXT AGENCY/MEDIA                      
*                                                                               
         CLI   QOPT1,C'Y'          ONE AGENCY ONLY ?                            
         BNE   BL15                NO - GET RECORD                              
         CLC   KEY(2),QAGENCY      SAME AGENCY ?                                
         BNE   BL20                NO - DONE                                    
         CLI   QMEDIA,C' '         ONE MEDIA ONLY ?                             
         BNH   BL15                NO - GET RECORD                              
         CLC   KEY(3),QAGENCY      SAME AGENCY/MEDIA ?                          
         BNE   BL20                NO - DONE                                    
*                                                                               
BL15     DS    0H                  GET AGENCY RECORD                            
*                                                                               
         LA    R0,PAGYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETAGY                                                           
*                                                                               
         TM    PAGYSTAT,X'01'      SKIP THIS AGENCY ?                           
         BO    BL18                YES - NEXT AGENCY REC                        
*                                                                               
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AGYTAB,R3                                                        
         MVC   AGYTAGY(3),KEY      AGENCY/MEDIA                                 
         MVC   AGYTCID,KEY         DEFAULT CORP ID IS AGENCY CODE               
         MVI   AGYTPRD,C'N'        DEFAULT (DO NOT SHOW PRD DETAIL)             
         TM    PAGYSTAT,X'02'      SHOW PRODUCT DETAIL ?                        
         BNO   BL17                NO                                           
         MVI   AGYTPRD,C'Y'        YES                                          
*                                                                               
         DROP  R3                                                               
*                                  ADD TO AGENCY TABLE                          
BL17     DS    0H                                                               
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
BL18     DS    0H                  NEXT AGENCY/MEDIA                            
         MVC   WORK(3),KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(3),WORK                                                      
         MVI   KEY+3,X'FF'                                                      
         B     BL13                GET NEXT (HIGH)                              
*                                                                               
BL19     DS    0H                  NEXT RECORD (BYPASS)                         
         GOTO1 SEQ                                                              
         B     BL13D               GO TEST KEY                                  
         EJECT                                                                  
*============================================================*                  
* OPEN A REPORT ON THE PRTQUE AND INITALIZE FOR DOWNLOADING  *                  
*============================================================*                  
         SPACE 1                                                                
BL20     DS    0H                                                               
         OC    BINPARMS+8(4),BINPARMS+8    ANY ENTRIES IN AGY TABLE ?           
         BZ    BADREQ                      NO - BAD                             
*                                                                               
         MVC   WORK(6),=6X'FF'     *******  END OF TABLE                        
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         XC    DLCB,DLCB                                                        
D        USING DLCBD,DLCB                                                       
*                                                                               
         MVI   D.DLCBACT,C'I'        START AND INITIALIZE REPORT                
         MVC   D.DLCBAPR,=A(BLPRINT) PRINT ROUTINE ADDRESS                      
         LA    R0,P                                                             
         ST    R0,D.DLCBAPL          PRINT LINE ADDRESS                         
         OI    D.DLCBFLG1,DLCBFXTN                                              
         MVC   D.DLCXTND(7),MAXLINE                                             
*                                                                               
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
*========================================================                       
* OUTPUT A SYSNUM IDENTIFYING RECORD AND                                        
* A RECORD FOR EACH AGENCY IN THE TABLE                                         
*========================================================                       
*                                                                               
         MVC   D.DLCBFLD(8),=C'*SYSNUM='                                        
         L     RE,UTL              GET UTL ENTRY ADDRESS                        
         SR    R0,R0                                                            
         IC    R0,4(RE)            GET SYSTEM NUMBER                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  D.DLCBFLD+8(2),DUB                                               
*                                                                               
         MVI   D.DLCBTYP,C'T'                                                   
         MVI   D.DLCBACT,DLCBPUT                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     RE,=A(BINTBL)                                                    
         OC    0(LAGYTAB,RE),0(RE)        TEST DATA IN AGENCY TABLE             
         BZ    BADREQ                                                           
*                                                                               
         XC    THISAGY,THISAGY                                                  
         L     R4,=A(BINTBL)                                                    
BL22     CLI   0(R4),X'FF'         END OF AGY/MED TABLE ?                       
         BE    BL30                YES                                          
         CLC   THISAGY,0(R4)                                                    
         BE    BL24                NEXT TABLE ENTRY                             
         MVC   THISAGY,0(R4)                                                    
         LA    R6,D.DLCBFLD                                                     
         MVC   0(7,R6),=C'*AGENCY'               RECORD ID                      
         LA    R6,8(R6)                                                         
*                                                                               
         MVC   0(2,R6),0(R4)                     AGENCY ALPHA                   
*                                                                               
         MVI   D.DLCBTYP,C'T'                                                   
         MVI   D.DLCBACT,DLCBPUT                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 (RF),(R1)                                                        
*                                                                               
BL24     LA    R4,LAGYTAB(R4)                                                   
         B     BL22                TEST NEXT ENTRY                              
*                                                                               
         DROP  D                                                                
*                                                                               
BADREQ   MVC   P(32),=C'** ERROR ** NO AGENCIES SELECTED'                       
         GOTO1 REPORT                                                           
         MVI   HAVEDATA,C'B'       INDICATES NO AGENCY SELECTED                 
         MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,31,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=80'                                    
         EJECT                                                                  
*===============================================================*               
* NOW READ BILLING RECORDS FOR SELECTED AGENCIES                *               
*===============================================================*               
         SPACE 1                                                                
BL30     GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                  GET FIRST AGENCY FROM TABLE                  
         XC    WORK(6),WORK                                                     
         GOTO1 =V(BINSRCH),BINPARMS,(X'02',WORK)                                
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'           AT LEAST ONE AGENCY/MEDIA MUST BE FOUND           
         ZICM  R3,1(R1),3     POINT R3 TO TABLE - A(FOUND AGY/MED REC)          
         USING AGYTAB,R3                                                        
*                                                                               
BL30A    DS    0H                                                               
         CLI   AGYTAGY,X'FF'       END OF AGY/MED TABLE ?                       
         BE    BL100               YES - GO GET SORTED RECORDS                  
*                                                                               
         MVC   THISAGY,AGYTAGY                                                  
         MVC   THISMED,AGYTMED                                                  
         MVC   THISCORP,AGYTCID    MOVE CORP ID SAVED IN TABLE                  
*                                                                               
         MVI   PRDBILL,C'N'        AGENCY PRODUCT DETAIL DEFAULT                
         CLI   AGYTPRD,C'Y'        SHOW PRODUCT DETAIL ?                        
         BNE   *+8                 NO                                           
         MVI   PRDBILL,C'Y'        YES                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PBILLKEY,R4                                                      
         MVC   PBILKAGY(3),AGYTAGY     AGENCY/MEDIA                             
         MVI   PBILKRCD,8              RECORD CODE                              
*                                                                               
BL30X    GOTO1 HIGH                                                             
         B     BL34                                                             
*                                                                               
BL32     MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
BL34     DS    0H                                                               
         CLC   KEY(4),KEYSAVE      TEST CHANGE OF AGY/MED/RECORD CODE           
         BE    BL36                NO                                           
         LA    R3,LAGYTAB(R3)      BUMP TO NEXT AGY/MED                         
         B     BL30A                                                            
*                                                                               
BL36     DS    0H                                                               
         CLC   BBILLYM,PBILKBMN     BILLED THIS MONTH?                          
         BNE   BL32                 NO - IGNORE                                 
*                                                                               
         OC    PBILKEST,PBILKEST   IGNORE PRODUCT TOTAL BILLS                   
         BZ    BL32                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         LA    R0,PBILLREC                                                      
         ST    R0,AREC                                                          
         GOTO1 GETBILL             READ BILL RECORD                             
*                                                                               
         CLI   PBRETAIL,X'41'      IGNORE RETAIL BILLS                          
         BE    BL32                                                             
         TM    PBILCMSW,X'20'      IGNORE AOR BILLS ALSO                        
         BO    BL32                                                             
*                      REPLACE BILL RECORD CODE WITH CLIENT RECORD CODE         
*                            FOR CLC BELOW (RESTORED AT PROC BL38)              
         MVI   PBILKRCD,2                                                       
         CLC   PBILLKEY(PBILKPRD-PBILLKEY),PCLTKEY    HAVE CLT REC ?            
         BE    BL38                YES                                          
*                                                                               
BL36B    DS    0H                  GET CLIENT RECORD                            
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(PBILKPRD-PBILLKEY),PBILLREC                                  
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETCLI                                                           
         MVI   CLTSW,C'N'          CLIENT DATA NOT OUTPUT YET                   
*                                                                               
*SMY*    GOTO1 =V(OFFOUT),DMCB,PCLTOFF,HEXOUT,(C'L',THISOFFC)                   
*SMY*    CLI   0(R1),X'FF'                                                      
*SMY*    BNE   *+6                                                              
*SMY*    DC    H'0'                                                             
*                                  2-CHAR CLIENT OFFICE CHECKING                
         MVC   THISOFFC(1),PCLTOFF                                              
         MVI   THISOFFC+1,C' '                                                  
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         GOTOR VPRNTOFC,DMCB,PCLTOFF,WORK,VOFFICER,PCLTKAGY,VCOMFACS            
*                                                                               
         CLI   WORK+1,C' '                                                      
         BNH   *+10                                                             
         MVC   THISOFFC,WORK       USE 2-CHAR OFFICE IF AVAILABLE               
*                                                                               
         CLI   THISOFFC,X'00'                                                   
         BNE   *+8                                                              
         MVI   THISOFFC,C' '                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE KEY TO BILL REC                      
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
BL38     MVI   PBILKRCD,8          RESTORE BILL RECORD CODE                     
         MVC   THISCLT,PBILKCLT                                                 
*                                                                               
         MVC   THISPRD,SPACES                                                   
         CLI   PRDBILL,C'Y'        TEST OUTPUT PRODUCT DETAIL                   
         BNE   *+10                                                             
         MVC   THISPRD,PBILKPRD                                                 
*                                                                               
         SR    R0,R0                                                            
**NOP**  ZICM  R0,PBILKEST,2       SUPPRESS ESTIMATE DETAIL                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEST,DUB                                                      
*                                                                               
         MVC   DUB(2),PBILKMOS                                                  
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(3,DUB),(20,WORK)                                    
         MVC   THISSVYM(4),WORK     MOVE YYYY                                   
         MVI   THISSVYM+4,C'/'      /                                           
         MVC   THISSVYM+5(2),WORK+4 MM                                          
*                                                                               
* ADJUST BILLED AMOUNTS (COMMISSION ONLY BILLS)                                 
*                                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'B',PBILLREC),PPBVALD                          
*                                                                               
*NOP*    XC    DUB,DUB                                                          
*NOP*    MVC   DUB+3(5),PPBVEBG    PPBVEBG IS "EFFECTIVE" GROSS                 
*NOP*    MVC   DUB+2(6),PPBVEBG    PPBVEBG IS "EFFECTIVE" GROSS                 
*NOP*    CVB   R0,DUB                          FROM PPBVAL                      
*NOP*    STCM  R0,15,THISBGRS      GROSS BILL AMOUNT                            
         ZAP   THISBGRS,PPBVEBG    PPBVEBG IS "EFFECTIVE" GROSS                 
*                                                                               
*NOP*    XC    THISBNET,THISBNET   DEFAULT OF ZEROS (NET BILL AMOUNT)           
         ZAP   THISBNET,=P'0'      DEFAULT OF ZEROS (NET BILL AMOUNT)           
*                                                                               
         MVI   THISTRF,C'0'        DEFAULT                                      
*                                                                               
*==========================================================*                    
* PUT CLIENT DATA TO FILE AND BILL DATA TO SORT            *                    
*==========================================================*                    
         SPACE 1                                                                
         CLI   CLTSW,C'Y'          TEST OUTPUT CLIENT DATA YET                  
         BE    BL50                YES                                          
         XC    SVCLDATA,SVCLDATA                                                
         MVC   SVCLAGY,PCLTKAGY                                                 
         MVI   SVCLSYS,C'P'                                                     
         MVC   SVCLMED,PCLTKMED                                                 
         MVC   SVCLCODE,PCLTKCLT                                                
         MVC   SVCLNAME(20),PCLTNAME                                            
         L     R1,=A(CLTFILE)                                                   
         LA    R0,SVCLDATA                                                      
         PUT   (1),(0)                                                          
         MVI   CLTSW,C'Y'                                                       
*                                                                               
BL50     GOTO1 =V(SORTER),DMCB,=C'PUT',THISREC                                  
         B     BL32                NEXT PRTDIR REC                              
         EJECT                                                                  
*================================================================*              
* GET RECORDS FROM SORT AND ADD BILLED AMOUNTS IF KEYS EQUAL     *              
*================================================================*              
         SPACE 1                                                                
BL100    GOTO1 =V(SORTER),DMCB,=C'GET'   GET FIRST RECORD                       
         ICM   R6,15,4(R1)                                                      
         BNZ   BL101                                                            
         MVC   P(30),=C'** ERROR ** NO BILLING ON FILE'                         
         GOTO1 REPORT                                                           
         GOTO1 ANXTREQ                                                          
         B     EXIT                                                             
*                                                                               
BL101    MVC   NEWREC,0(R6)                                                     
         MVC   THISREC,NEWREC                                                   
         MVI   ACTIVITY,C'Y'      SET ACTIVITY FOUND SW                         
*                                                                               
BL102    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R6,15,4(R1)                                                      
         BNZ   BL104                                                            
         MVI   NEWREC,X'FF'                                                     
         B     BL106                                                            
*                                                                               
BL104    MVC   NEWREC,0(R6)        MOVE RECORD SO CAN SEE IT                    
*                                                                               
BL106    CLC   THISKEY,NEWREC      TEST SAME KEYS                               
         BNE   BL110                                                            
* KEYS EQUAL, ADD BILLED AMOUNTS                                                
*NOP*    ICM   R0,15,THISBGRS                                                   
*NOP*    ICM   R1,15,THISBGRS-THISREC+NEWREC                                    
*NOP*    AR    R0,R1                                                            
*NOP*    STCM  R0,15,THISBGRS                                                   
         AP    THISBGRS,THISBGRS-THISREC+NEWREC                                 
*                                                                               
*NOP*    ICM   R0,15,THISBNET                                                   
*NOP*    ICM   R1,15,THISBNET-THISREC+NEWREC                                    
*NOP*    AR    R0,R1                                                            
*NOP*    STCM  R0,15,THISBNET                                                   
         AP    THISBNET,THISBNET-THISREC+NEWREC                                 
         B     BL102                                                            
         EJECT                                                                  
*=============================================================*                 
* OUTPUT BILL DATA TO PRTQUE REPORT                           *                 
*=============================================================*                 
         SPACE 1                                                                
BL110    DS    0H                  CONVERT DOLLARS TO EBCDIC                    
*NOP*    ICM   R0,15,THISBGRS                                                   
         MVC   WORK+50(11),THISBGRS                                             
         BAS   RE,EDDOLS                                                        
         MVC   THISBGRS,DOLS                                                    
*                                                                               
*NOP*    ICM   R0,15,THISBNET                                                   
         MVC   WORK+50(11),THISBNET                                             
         BAS   RE,EDDOLS                                                        
         MVC   THISBNET,DOLS                                                    
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         LA    R4,THISBTAB                                                      
*                                                                               
         MVI   THISSYS,C'P'        PRINT                                        
*                                                                               
BL120    DS    0H                                                               
         L     RE,0(R4)            GET DATA ADDR                                
         ZIC   RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(RE)                                                 
*                                                                               
         MVC   DLCBTYP(1),5(R4)                                                 
         MVI   DLCBACT,DLCBPUT                                                  
*                                                                               
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         LA    R4,L'THISBTAB(R4)                                                
         CLI   0(R4),X'FF'                                                      
         BNE   BL120                                                            
*                                                                               
         MVI   DLCBACT,DLCBEOL                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         MVC   THISREC,NEWREC      SAVE 'NEW' RECORD                            
         CLI   THISREC,X'FF'                                                    
         BNE   BL102                                                            
*                                                                               
* OUTPUT A GOOD EOF MESSAGE                                                     
         MVC   DLCBFLD(11),=C'*END OF RUN'                                      
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         MVI   DLCBACT,DLCBEOL                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         B     EXIT                                                             
*                                                                               
EDDOLS   EDIT  (P11,WORK+50),(11,DOLS),MINUS=YES,ZERO=NOBLANK                   
         BR    RE                                                               
         EJECT                                                                  
*=============================================================*                 
* RUNLAST - CLOSE THE DOWNLOADED REPORT                       *                 
*=============================================================*                 
         SPACE 1                                                                
BL300    DS    0H                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         DROP  R1                                                               
*                                                                               
         CLOSE CLTFILE                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   HAVEDATA,C'B'                                                    
         BE    EXIT                NO AGENCY SELECTED - NO DOWNLOAD             
*                                                                               
* NOW READ CLTFILE AND DOWNLOAD CONTENTS                                        
         OPEN  (CLTFILE,INPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVI   DLCBACT,C'I'        INITIALIZE CLIENT NAME REPORT                
         GOTO1 =V(DLFLD),(R1)                                                   
         DROP  R1                                                               
*                                                                               
BL310    L     R1,=A(CLTFILE)                                                   
         LA    R0,SVCLDATA                                                      
         GET   (1),(0)                                                          
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
*                                                                               
         LA    R4,SVCLTAB                                                       
*                                                                               
BL315    MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,5(R4)                                                    
*                                                                               
         L     RE,0(R4)            GET DATA ADDR                                
         ZIC   RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(RE)                                                 
*                                                                               
         GOTO1 =V(DLFLD),(R1)                                                   
*                                                                               
         LA    R4,L'SVCLTAB(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   BL315                                                            
*                                                                               
         MVI   DLCBACT,DLCBEOL                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 =V(DLFLD),(R1)                                                   
         B     BL310               GET NEXT RECORD                              
         DROP  R1                                                               
*                                                                               
* END-OF-INPUT FILE                                                             
*                                                                               
BL320    LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVI   DLCBACT,C'R'        SET E-O-R                                    
         GOTO1 =V(DLFLD),(R1)                                                   
         DROP  R1                                                               
*                                                                               
         CLOSE CLTFILE                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BL350    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*==============================================================*                
* USER PRINT ROUTINE EXIT CALLED BY DLFLD                      *                
* ALL DATA PRINTED HERE GOES ON PAGE 2                         *                
*==============================================================*                
BLPRINT  NTR1                                                                   
         MVI   LINE,0              FORCE NO PAGE BREAK                          
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* THESE FIELDS USED TO GET WIDE PRINT LINE OVERRIDES                            
* THEY GET MOVED INTO THE DLCB TO OVERRIDE MAXLINE                              
*                                                                               
MAXLINE  DC    H'132'              MAX LINE WIDTH                               
DELIM    DC    C' '                FIELD DELIMITER CHR                          
EOTCHR   DC    C'"'                END OF TEXT FIELD DELIMITER                  
EOTALT   DC    C''''               END OF TEXT CHR ALTERNATE                    
EOLCHR   DC    X'5E'               END OF LINE CHAR - SEMICOLON                 
EORCHR   DC    C':'                END OF REPORT CONTROL CHR                    
*                                                                               
SVUTL    DS    X                                                                
*                                                                               
BBILLYM  DS    XL2                 BILLING YEAR/MONTH FILTER                    
HAVEDATA DS    CL1                 NOTHING SELECTED FLAG                        
PRDBILL  DS    CL1                 OUTPUT PRODUCT DETAILS ?                     
NEXTAM   DS    CL1                 GET NEXT AGY/MED TABLE ENTRY ?               
CLTSW    DS    CL1                 CLIENT DATA OUTPUT ?                         
DOLS     DS    CL11                                                             
*                                                                               
STRTBILL DS    XL3                 START DATE IN BINARY                         
ENDBILL  DS    XL3                 END DATE IN BINARY                           
*                                                                               
SVCLDATA DS    0XL32                                                            
SVCLAGY  DS    CL2                                                              
SVCLSYS  DS    CL1                                                              
SVCLMED  DS    CL1                                                              
SVCLCODE DS    CL3                                                              
SVCLNAME DS    CL20                                                             
         DS    CL5                 SPARE                                        
*                                                                               
SVCLTAB  DS    0XL6                                                             
         DC    AL4(THISBEAC),AL1(L'THISBEAC),C'T'                               
         DC    AL4(SVCLAGY),AL1(L'SVCLAGY),C'T'                                 
         DC    AL4(SVCLSYS),AL1(L'SVCLSYS),C'T'                                 
         DC    AL4(SVCLMED),AL1(L'SVCLMED),C'T'                                 
         DC    AL4(SVCLCODE),AL1(L'SVCLCODE),C'T'                               
         DC    AL4(SVCLNAME),AL1(L'SVCLNAME),C'T'                               
         DC    X'FF'                                                            
THISBEAC DC    C'C'                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*THISREC'                                                    
THISREC  DS    0XL80                                                            
THISKEY  DS    0XL31                                                            
THISCORP DS    CL2     +00         CORPORATE ID                                 
THISAGY  DS    CL2     +02         ALPHA AGY                                    
THISSYS  DS    CL1     +04         SYSTEM CODE (SPOT/PRINT/ACC)                 
THISMED  DS    CL1     +05         MEDIA CODE                                   
THISOFFC DS    CL2     +06         MEDIA OFFICE CODE                            
THISCLT  DS    CL3     +08         ALPHA CLIENT CODE                            
THISPRD  DS    CL3     +11         ALPHA PRODUCT CODE                           
THISEST  DS    CL3     +14         NUMERIC ESTIMATE                             
*                                                                               
THISBLYM DS    CL7     +17         BILLING Y/M  YYYY/MM                         
THISSVYM DS    CL7     +24         Y/M OF SERVICE YYYY/MM                       
*                                                                               
THISBGRS DS    CL11    +31         GROSS BILLING IN PENNIES                     
THISBNET DS    CL11    +42         NET BILLING IN PENNIES                       
THISTRF DS     CL1     +53         SPOT TRAFFIC FLAG                            
         DS    CL26    +54         SPARE                                        
*                                                                               
         DS    0D                                                               
THISBTAB DS    0XL6                                                             
         DC   AL4(THISBEAB),AL1(L'THISBEAB),C'T'                                
         DC   AL4(THISCORP),AL1(L'THISCORP),C'T'                                
         DC   AL4(THISAGY),AL1(L'THISAGY),C'T'                                  
         DC   AL4(THISSYS),AL1(L'THISSYS),C'T'                                  
         DC   AL4(THISMED),AL1(L'THISMED),C'T'                                  
         DC   AL4(THISOFFC),AL1(L'THISOFFC),C'T'                                
         DC   AL4(THISCLT),AL1(L'THISCLT),C'T'                                  
         DC   AL4(THISPRD),AL1(L'THISPRD),C'T'                                  
         DC   AL4(THISEST),AL1(L'THISEST),C'N'                                  
         DC   AL4(THISBLYM),AL1(L'THISBLYM),C'N'                                
         DC   AL4(THISSVYM),AL1(L'THISSVYM),C'N'                                
         DC   AL4(THISBGRS),AL1(L'THISBGRS),C'N'                                
         DC   AL4(THISBNET),AL1(L'THISBNET),C'N'                                
         DC   AL4(THISTRF),AL1(L'THISTRF),C'N'                                  
         DC   X'FF'                                                             
THISBEAB DC   C'B'                                                              
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
DLCB     DS    XL256                                                            
         DC    CL8'*NEWREC*'                                                    
NEWREC   DS    CL80                                                             
SAVEKEY  DS    CL32                                                             
ACTIVITY DS    CL1                                                              
BINPARMS DS    6F                                                               
PROFKEY  DS    CL12                                                             
VOFFICER DS    V                                                                
VPRNTOFC DS    V                                                                
         EJECT                                                                  
* NOTE THERE IS NO DSECT ON THE FOLLOWING INCLUDE                               
       ++INCLUDE PPBVALD                                                        
         SPACE 2                                                                
*                                                                               
CLTFILE  DCB   DDNAME=CLTFILE,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=32,MACRF=(GM,PM),EODAD=BL320                               
*                                                                               
         DS    0D                                                               
         DC    C'*AGYTAB*'                                                      
BINTBL   DS    300XL(LAGYTAB)                                                   
LBINTBL  EQU   *-BINTBL                                                         
* DSECT FOR AGENCY TABLE (BINTBL)                                               
AGYTAB   DSECT                                                                  
*                                                                               
AGYTAGY  DS    CL2                 AGENCY CODE (KEY)                            
AGYTMED  DS    CL1                 AGENCY MEDIA (KEY)                           
AGYTCID  DS    CL2                 AGENCY CORPORATE ID                          
AGYTPRD  DS    CL1                 OUTPUT PRODUCT DETAIL ? (Y/N)                
LAGYTAB  EQU   *-AGYTAGY                                                        
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
*                                                                               
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PSYSTEM  DS    CL1                                                              
PMEDIA   DS    CL1                                                              
         DS    CL1                                                              
POFFICE  DS    CL2                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PBILLYR  DS    CL4                                                              
         DS    CL1                                                              
PBILLMON DS    CL2                                                              
         DS    CL1                                                              
PYRSVC   DS    CL4                                                              
         DS    CL1                                                              
PMNSVC   DS    CL2                                                              
         DS    CL1                                                              
PBILLNUM DS    CL6                                                              
         DS    CL1                                                              
PGROSSP  DS    CL10                                                             
         DS    CL1                                                              
PNET     DS    CL10                                                             
         DS    CL1                                                              
PACTUAL  DS    CL10                                                             
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDCOMFACSD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPREPBL02 09/03/15'                                      
         END                                                                    
