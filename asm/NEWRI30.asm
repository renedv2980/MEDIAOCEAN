*          DATA SET NEWRI30    AT LEVEL 104 AS OF 05/01/02                      
*PHASE T32030A,+0                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T32030 - N8  MISS/MG REPORT'                                    
T32030   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEN8**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4         ANETWS4 = WORKING STORAGE                     
         USING MYD,R7                                                           
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         ST    R2,RELO                                                          
         L     R1,=A(RECTABLE)                                                  
         A     R1,RELO                                                          
         ST    R1,ATABLE                                                        
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,REPMOD                                                        
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
REPMOD   NTR1                                                                   
         L     RE,ATABLE                                                        
         L     RF,=F'30000'                                                     
         XCEF                                                                   
         XC    LINENUM,LINENUM                                                  
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
REP01    NETGO NSNETIO,DMCB,NETBLOCK    TO SET NBCOMPST,NBCMPEND                
         CLI   NBMODE,NBVALDAT          (DON'T NEED DATE LIST)                  
         BNE   REP01                                                            
         MVC   NUMMONS,=F'2'                                                    
         MVI   PERTYPE,C'W'                                                     
         NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
         B     REP5                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(4,2,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=350'                                   
*                                                                               
         EJECT                                                                  
REP5     MVI   NBDATA,C'U'         UNITS ONLY                                   
         MVI   NBACTOPT,0                                                       
         MVI   NBESTOPT,0                                                       
         MVI   NBSEQ,C'Q'          NETWORK/PROGRAM/AIR DATE                     
***->    MVI   NBRESUME,NBPROCPK                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK                                            
**       TM    NBSUBMSK,NBSBMNET      BREAK ON NETWORK                          
**       BZ    REP6                                                             
         CLI   NTWKSV,0                                                         
         BE    REP6                                                             
         CLC   NTWKSV,NBACTNET                                                  
         BE    REP6                                                             
                                                                                
         CLC   SPLNET(3),=C'ALL'                                                
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,REP30                                                         
*                                                                               
REP6     CLI   NBMODE,NBPROCUN                                                  
         BE    REP10                                                            
*                                                                               
         CLI   NBMODE,NBREQLST                                                  
         BNE   REP7                                                             
         BAS   RE,REP30                                                         
         B     XIT                                                              
*                                                                               
REP7     CLI   NBERROR,0                                                        
         BE    GETUNIT                                                          
         DC    H'0'                                                             
*                                                                               
REP10    DS    0H                                                               
         MVC   NTWKSV,NBACTNET                                                  
*                                 R5 - M/G ELEMENT                              
*                                 R6 - RECTABLE                                 
*                                                                               
         L     R5,NBAIO                                                         
         MVI   ELCODE,7            IS IT MADE GOOD                              
         USING NUMGD,R5                                                         
         BAS   RE,GETEL                                                         
         BNE   GETUNIT             IF NOT/ SKIP THE UNIT                        
         L     R6,ATABLE                                                        
         USING RECTABLD,R6                                                      
REP11    CLC   0(6,R6),=6X'0'                                                   
         BE    REP12                                                            
         LA    R6,RECLENE(R6)                                                   
         B     REP11                                                            
REP12    DS    0H                                                               
         LA    R0,9                SET MAX NO OF M/G                            
         BAS   RE,RECNUMB          RETURNS REC NUMBER IN WORK                   
*                                  ZERO IF M/G FOR ANOTHER UNIT                 
         MVC   RECNO,WORK                                                       
         MVC   RECDAT,NBACTDAT      UNIT DATA                                   
         MVC   RECSUB,NBACTSUB                                                  
         MVC   RECPROG,NBPROGNM                                                 
         MVC   RECPROGC,NBACTPRG                                                
** ->    MVC   RECKEY,NBKEY         NBKEY CAN BE OUT OF SYNC WITH AIO           
         LA    R1,RECKEY            SO BUILD MY OWN PASSIVE POINTER             
         USING NUKPKEY,R1                                                       
         L     RE,NBAIO             USE REC IN IO AREA TO BUILD DATA            
         MVI   0(R1),X'84'                                                      
         MVC   NUKPAM,1(RE)                                                     
         MVC   NUKPCLT,2(RE)                                                    
         MVC   NUKPNET,7(RE)                                                    
         MVC   NUKPPROG,11(RE)                                                  
         MVC   NUKPDATE,4(RE)                                                   
         MVC   NUKPEST,17(RE)                                                   
         MVC   NUKPSUB,18(RE)                                                   
         MVC   NUKPDP,19(RE)                                                    
         DROP  R1                                                               
*                                                                               
         MVC   RECACT$,NBACTUAL                                                 
******************************                                                  
**       GOTO1 =V(PRNTBL),DMCB,=C'TTT',NBAIO,C'DUMP',40,=C'1D'                  
**       GOTO1 =V(PRNTBL),DMCB,=C'TT2',RECKEY,C'DUMP',40,=C'1D'                 
         CLC   NBKEY(20),=X'8483B405D4E3E540D9E2E7F0F0F1C2636B01C300'           
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
REP14    MVC   RECDAT2,NUMGDATE     M/G BY DATA                                 
         MVC   RECSUB2,NUMGSUB                                                  
         MVC   RECPRG2,NUMGPNM                                                  
         MVC   RECPRGCD,NUMGPCOD                                                
**************************************                                          
         CLC   =C'KING',RECPRGCD                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
*************************************                                           
         MVI   ELCODE,7                                                         
         BAS   RE,NEXTEL           ARE THERE MULTIPLE M/G                       
         BNE   GETUNIT                                                          
         LA    R6,RMGLENE(R6)      BUMP TO NXT M/G ELEM                         
         BCT   R0,REP14                                                         
         DC    H'0'                EXCEEDS NO OF M/G'S                          
         EJECT                                                                  
*                                                                               
REP30    NTR1                                                                   
* STEP THROUGH TABLE. IF RECDAT2 IS BEFORE REQUEST DATE                         
* GET THAT REC AND ADD TO TABLE                                                 
* R2 -> POINTS TO START OF CHAIN   R6 -> POINTS TO M/G ELEMS IN CHAIN           
*                                                                               
         L     R2,ATABLE                                                        
         OC    0(6,R2),0(R2)                                                    
         BZ    REP75                TABLE EMPTY/ EXIT                           
         L     R6,ATABLE                                                        
REP31    LA    R0,9                                                             
         USING RECTABLD,R6                                                      
REP31A   CLC   RECDAT2,NBCMPSTR                                                 
         BL    REP31B                                                           
         CLC   RECDAT2,NBCMPEND                                                 
         BNH   REP31C                                                           
REP31B   BAS   RE,READREC                                                       
         BAS   RE,ADDIT                                                         
REP31C   LA    R6,RMGLENE(R6)      ARE THERE MULTIPLE M/G                       
         CLC   RECDAT2,=2X'00'                                                  
         BE    *+8                                                              
         BCT   R0,REP31A                                                        
         LA    R2,RECLENE(R2)      BUMP TO NEXT REC                             
         OC    0(6,R2),0(R2)       IS IT END OF TABLE                           
         BZ    REP31E                                                           
         LR    R6,R2               NO/SO SET R6 START OF NEW REC                
         B     REP31                                                            
*                                                                               
* ALL UNITS HAVE BEEN READ.                                                     
* NOW STEP THROUGH TABLE AND ONLY CONSIDER                                      
* THOSE RECS THAT HAVE LINE NUMBERS. TAKE THEIR M/G DATE(S) AND CHK             
* IF THE M/G DATE(S) MATCH ANY RECDAT. IF YES, GIVE MATCHED REC THE             
* LINE NO + 1 OF FIRST REC.                                                     
*                                                                               
REP31E   DS    0H                                                               
         LA    R1,9                                                             
         BAS   RE,MATCH                                                         
         BCT   R1,*-4                                                           
         B     REP50                                                            
*                                                                               
MATCH    NTR1                                                                   
         L     R6,ATABLE                                                        
         USING RECTABLD,R6                                                      
         LR    R2,R6                STORE AD OF 1ST REC                         
         OC    RECNO,RECNO         DOES REC HAVE LINE NUMBER                    
         BZ    REP40                                                            
REP32    L     R5,ATABLE                                                        
REP32A   CLC   RECDAT2(19),4(R5)       DATE/SUB/PROG MATCH                      
         BNE   REP39                                                            
         OC    0(4,R5),0(R5)       DOES MATCHED REC HAVE LINE NO                
*        BZ    *+6                                                              
*        DC    H'0'                IF SO/BOMB                                   
         BNZ   REP39                                                            
         L     R1,RECNO                                                         
         LA    R1,1(R1)                                                         
         STCM  R1,15,0(R5)                                                      
         B     REP39A                                                           
*                                                                               
REP39    LA    R5,RECLENE(R5)      BUMP TO NEXT REC TO MATCH                    
         OC    0(6,R5),0(R5)                                                    
         BNZ   REP32A                                                           
REP39A   LA    R6,RMGLENE(R6)      ARE THERE MULTIPLE M/G                       
         OC    0(2,R6),0(R6)                                                    
         BNZ   REP32                                                            
REP40    LA    R2,RECLENE(R2)      BUMP TO NEXT REC WITH LINE NO                
         OC    0(4,R2),0(R2)                                                    
         BNZ   REP42                                                            
         OC    0(6,R2),0(R2)       END OF TABLE                                 
*        BZ    REP50               YES                                          
         BZ    XIT                                                              
         B     REP40               NO                                           
REP42    LR    R6,R2                                                            
         B     REP32                                                            
         EJECT                                                                  
REP50    DS    0H                                                               
* ALL RECS HAVE BEEN CHECKED FOR LINE NUMBERS                                   
* WE WILL IGNORE THOSE THAT HAVE NONE BUT PASS THEM TO SORTER                   
* NEVERTHELESS IN CASE WE NEED TO LOOK AT THEM LATER FOR CHECKING               
*                                                                               
         L     R2,ATABLE                                                        
REP52    GOTO1 SORTER,DMCB,=C'PUT',0(R2)                                        
         LA    R2,RECLENE(R2)                                                   
         OC    0(6,R2),0(R2)                                                    
         BNZ   REP52                                                            
*                                                                               
REP60    DS    0H                                                               
         LA    R2,P                                                             
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,4(R1)                                                         
         LTR   R6,R6                                                            
         BZ    REP70                                                            
         MVI   MULTMGDS,0          CLEAR FLAG                                   
         USING RECTABLD,R6                                                      
         OC    PREVNO,PREVNO       IS IT FIRST TIME                             
         BNZ   REP62                                                            
*                                                                               
         LA    R1,10               YES/SET PREVNO                               
         ST    R1,PREVNO                                                        
         MVI   NEWLINE,C'Y'                                                     
         B     REP63A                                                           
*                                                                               
REP62    CLC   PREVNO,RECNO                                                     
         BH    REP63A                                                           
REP63    L     R1,RECNO         ADD 10 / BUMPS OF M/G CHAINS ARE BY 10          
         LA    R1,10(R1)                                                        
         ST    R1,PREVNO                                                        
         MVI   NEWLINE,C'Y'                                                     
         MVI   ALLOWLIN,7                                                       
         BAS   RE,PRINT                             SKIP LINE                   
         BAS   RE,PRINT                             SKIP LINE                   
REP63A   DS    0H                                                               
         CLI   PRDCODE,C'N'                                                     
         BE    REP63B                                                           
         S     R2,=F'7'                                                         
         MVC   20(6,R2),RECPROGC                                                
         MVI   26(R2),C'/'                                                      
REP63B   GOTO1 DATCON,DMCB,(2,RECDAT),(5,44(R2))                                
         MVI   52(R2),C'-'                                                      
         EDIT  (B1,RECSUB),(2,53(R2)),ALIGN=LEFT                                
         MVC   27(16,R2),RECPROG                                                
                                                                                
         CLI   ACT$,C'Y'           ACTUAL DOLLARS?                              
         BNE   REP63D                                                           
                                                                                
         OC    RECACT$,RECACT$                                                  
         BNZ   *+14                                                             
         MVC   64(2,R2),=C'$0'                                                  
         B     REP63D                                                           
                                                                                
         SR    R0,R0                                                            
         ICM   R1,15,RECACT$                                                    
         D     R0,=F'100'          DROP PENNIES                                 
         LR    R3,R1                                                            
         EDIT  (R3),(7,59(R2)),FLOAT=$                                          
                                                                                
REP63D   CLI   NEWLINE,C'Y'                                                     
         BNE   REP65                                                            
         MVI   NEWLINE,0                                                        
         LA    R2,P+62                                                          
         CLI   ACT$,C'Y'                                                        
         BNE   *+8                                                              
         LA    R2,4(R2)                                                         
                                                                                
         MVC   0(12,R2),=C'MADE GOOD BY'                                        
                                                                                
REP65    LA    R2,P+81                                                          
         CLI   PRDCODE,C'N'                                                     
         BE    REP65A                                                           
         MVC   0(6,R2),RECPRGCD                                                 
         MVI   6(R2),C'/'                                                       
         LA    R2,7(R2)                                                         
REP65A   MVC   0(16,R2),RECPRG2                                                 
         GOTO1 DATCON,DMCB,(2,RECDAT2),(5,17(R2))                               
         MVI   23(R2),C'-'                                                      
         EDIT  (B1,RECSUB2),(2,24(R2)),ALIGN=LEFT                               
                                                                                
**********************************************************                      
         CLI   ACT$,C'Y'           REPORT ACTUAL DOLLARS?                       
         BNE   REP65AA                                                          
         CLI   MULTMGDS,C'Y'       IF IN MULTIPLE MKGD LOOP                     
         BE    *+10                ALREADY HAVE KEY                             
         MVC   KEY,RECKEY                                                       
         LA    R3,KEY                                                           
         USING NUKPKEY,R3                                                       
         MVC   NUKPDATE,RECDAT2                                                 
         MVC   NUKPSUB,RECSUB2                                                  
         MVC   NUKPPROG,RECPRGCD                                                
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R3,NBAIO                                                         
                                                                                
         SR    R0,R0                                                            
         ICM   R1,15,NUACTUAL                                                   
                                                                                
         LTR   R1,R1                                                            
         BNZ   *+14                                                             
         MVC   37(2,R2),=C'$0'                                                  
         B     REP65AA                                                          
                                                                                
         D     R0,=F'100'          DROP PENNIES                                 
         LR    R3,R1                                                            
         EDIT  (R3),(10,29(R2)),FLOAT=$                                         
         DROP  R3                                                               
**************************************************************                  
                                                                                
REP65AA  DS    0H                                                               
         BAS   RE,PRINT                                                         
         LA    R6,RMGLENE(R6)                                                   
         OC    RECDAT2,RECDAT2     IS IT MULTIPLE M/GS                          
         BZ    REP60               NO/GET NXT SORTREC                           
         MVI   MULTMGDS,C'Y'       YES/ SET FLAG TO NOT PRINT ACT$              
         B     REP65                   GET NEXT MULTIPLE M/G                    
*                                                                               
REP70    DS    0H                                                               
         MVI   FORCEHED,C'Y'       NEW PAGE/ NXT NTWK                           
         XC    PREVNO,PREVNO       STARTS NEW M/G GROUP                         
         XC    NEWLINE,NEWLINE                                                  
         XC    LINENUM,LINENUM                                                  
         L     R1,ATABLE                                                        
REP72    OC    0(6,R1),0(R1)                                                    
         BZ    REP75                                                            
         LR    RE,R1                                                            
         LA    RF,RECLENE                                                       
         XCEF                                                                   
****     XC    0(RECLENE,R1),0(R1) CLEAR TABLE                                  
         LA    R1,RECLENE(R1)                                                   
         B     REP72                                                            
REP75    DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'      CLOSE/OPEN SORTER FOR                   
         CLI   NBMODE,NBREQLST          FOR NEXT NETWORK                        
         BE    XIT                                                              
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* THIS ROUTINE READS UNIT RECS WHOSE DATE FALLS OUTSIDE REQUEST                 
* PARAMETERS BUT WHO ARE POINTED TO AS M/G BY READ UNITS                        
* R2 POINTS TO START OF CHAIN                                                   
* R6 POINTS TO M/G ELEMS WITHIN CHAIN                                           
* PUT REC IN ANETWS1                                                            
READREC  NTR1                                                                   
         DROP  R6                                                               
         XC    KEY,KEY                                                          
         USING RECTABLD,R2                                                      
         MVC   KEY(20),RECKEY          GET CURRENT KEY VALUES                   
         DROP  R2                                                               
         LA    R2,KEY                                                           
         USING NUKPKEY,R2                                                       
         USING RECTABLD,R6                                                      
         MVC   NUKPPROG,RECPRGCD       SET UP FOR NEW PROG/DATE                 
         MVC   NUKPDATE,RECDAT2                                                 
         MVC   NUKPSUB,RECSUB2                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ANETWS1                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFILE ',KEY+21,(R3),MYDMWRK         
         MVI   NBFUNCT,NBFRDHI     READ HI WHEN RESUMING NETIO READS            
RDX      B     XIT                                                              
         DROP  R2,R6                                                            
*                                                                               
* UNIT REC READ INTO ANETWS1                                                    
* THIS IS A M/G UNIT WHOSE DATE IS OUTSIDE REQUEST PARAMETERS BUT WAS           
* POINTED TO BY A READ UNIT.  IF IT IS MADE GOOD THEN ADD IT TO TABLE.          
* IF NOT, IT IS THE END OF A CHAIN AND WE ALREADY HAVE ITS DATA.                
*                                                                               
ADDIT    NTR1                                                                   
         LA    R0,9                                                             
         USING RECTABLD,R6                                                      
         L     R5,ANETWS1                                                       
         USING NUMGD,R5                                                         
         MVI   ELCODE,7                                                         
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         L     R6,ATABLE                                                        
ADD5     OC    0(6,R6),0(R6)                                                    
         BZ    *+12                                                             
         LA    R6,RECLENE(R6)                                                   
         B     ADD5                                                             
         MVC   RECDAT,NBACTDAT      UNIT DATA                                   
         MVC   RECSUB,NBACTSUB                                                  
         MVC   RECPROG,NBPROGNM                                                 
         MVC   RECKEY,NBKEY                                                     
*                                                                               
ADD7     MVC   RECDAT2,NUMGDATE     M/G BY DATA                                 
         MVC   RECSUB2,NUMGSUB                                                  
         MVC   RECPRG2,NUMGPNM                                                  
         MVC   RECPRGCD,NUMGPCOD                                                
         BAS   RE,NEXTEL           ARE THERE MULTIPLE M/G                       
         BNE   ADDX                                                             
         LA    R6,RMGLENE(R6)      BUMP TO NXT M/G ELEM                         
         BCT   R0,ADD7                                                          
         DC    H'0'                EXCEEDS NO OF M/G'S                          
ADDX     B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
* IF UNIT IS NOT A MAKE GOOD FOR ANOTHER UNIT, IT IS THE START OF A             
* MAKE-GOOD CHAIN AND GETS A LINE NUMBER.                                       
*      LINE NUMBERS START AT 10 AND BUMP BY 10                                  
*      INPUT R6 - UNIT DATA IN RECTABLE                                         
*                                                                               
RECNUMB  NTR1                                                                   
         XC    WORK(4),WORK                                                     
         MVI   ELCODE,6      IF UNIT IS A M/G FOR ANOTHER UNIT                  
         L     R5,NBAIO      THEN IT DOES NOT GET A DATA-LINE NO                
         BAS   RE,GETEL                                                         
         BE    RNUMX                                                            
         L     R5,ATABLE          STEP THROUGH TABLE - GET HIGHEST NO           
         USING RECTABLD,R5                                                      
RNUM3    OC    0(6,R5),0(R5)                                                    
         BZ    RNUM7                                                            
RNUM5    CLC   RECNO,=4X'0'                                                     
         BE    *+10                                                             
         MVC   LINENUM,RECNO                                                    
         LA    R5,RECLENE(R5)                                                   
         B     RNUM3                                                            
RNUM7    L     R1,LINENUM                                                       
         A     R1,=F'10'                                                        
         ST    R1,LINENUM                                                       
         MVC   WORK(4),LINENUM                                                  
RNUMX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'MISSED/MAKEGOOD REPORT'                                  
         SSPEC H2,52,C'----------------------'                                  
         SSPEC H3,50,PERIOD                                                     
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H3,105,RUN                                                       
         SSPEC H5,123,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+15(20),SPLCLIN                                                
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+15(20),SPLPRON                                                
         MVC   H7+27(4),NTWKSV                                                  
         CLC   NEWNET,NTWKSV                                                    
         BNE   *+10                                                             
         MVC   H7+31(7),=C'(CON''T)'                                            
         MVC   NEWNET,NTWKSV                                                    
         CLI   SPLEST,0                                                         
         BE    HDH4                                                             
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(3),SPLEST                                                  
         MVC   H5+15(20),SPLESTN                                                
HDH4     CLI   SPLPAK,0                                                         
         BE    HDH5                                                             
         MVC   H4+50(7),=C'PACKAGE'                                             
         MVC   H4+59(2),SPLPAK                                                  
         MVC   H4+65(20),SPLPAKN                                                
HDH5     CLI   SPLNET,0                                                         
         BE    HDH6                                                             
         MVC   H4+93(7),=C'NETWORK'                                             
         MVC   H4+103(8),SPLNET                                                 
HDH6     CLI   SPLDPT,0                                                         
         BE    BOXES                                                            
         MVC   H5+93(7),=C'DAYPART'                                             
         MVC   H5+103(7),SPLDPT                                                 
BOXES    DS    0H                   SET PARAMS FOR BOXES                        
         CLI   NOBOX,C'N'                                                       
         BE    HDHX                                                             
         CLI   ACT$,C'Y'           IF PRINTING ACTUAL $                         
         BE    HDHX                NO BOXES                                     
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDHX                YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         CLI   PRDCODE,C'N'                                                     
         BE    BOX5                                                             
         MVI   12(R5),C'L'                                                      
         MVI   114(R5),C'R'                                                     
         B     BOX7                                                             
BOX5     MVI   26(R5),C'L'                                                      
         MVI   106(R5),C'R'                                                     
BOX7     LA    R5,BOXROWS                                                       
         LA    R5,5(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,2(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,50(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDHX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
*                                                                               
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
RECTABLE DS    CL60000                                                          
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
NOBOX    DS    CL1                                                              
PRDCODE  DS    CL1                                                              
ACT$     DS    CL1                                                              
RELO     DS    A                                                                
LINENUM  DS    F                                                                
ATABLE   DS    F                                                                
NUMMONS  DS    F                                                                
PREVNO   DS    F                                                                
PERTYPE  DS    CL1                                                              
PEROPT   DS    CL1                                                              
NEWLINE  DS    CL1                                                              
MONLIST  DS    CL20                                                             
NTWKSV   DS    CL4                                                              
NEWNET   DS    CL4                                                              
MULTMGDS DS    CL1                                                              
MYDMWRK  DS    12D                                                              
         EJECT                                                                  
RECTABLD DSECT                                                                  
RECNO    DS    CL4                 REC LINE NUMBER                              
RECDAT   DS    CL2                                                              
RECSUB   DS    CL1                                                              
RECPROG  DS    CL16                                                             
RECPROGC DS    CL6                                                              
RECKEY   DS    CL20                                                             
RECACT$  DS    XL4                 ACTUAL DOLLARS                               
*                                                                               
RECDAT2  DS    CL2                 1ST M/G/DATA                                 
RECSUB2  DS    CL1                                                              
RECPRG2  DS    CL16                                                             
RECPRGCD DS    CL6                                                              
RMGLENE  EQU   *-RECDAT2                                                        
         DS    CL225                FOR 9 MORE M/G DATA                         
RECLENE  EQU   *-RECNO                                                          
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIE5D                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104NEWRI30   05/01/02'                                      
         END                                                                    
