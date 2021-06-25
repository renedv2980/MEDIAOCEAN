*          DATA SET SPPAY16    AT LEVEL 125 AS OF 06/26/15                      
*PHASE T21316C                                                                  
         TITLE 'T21316 - SPOTPAK PAY - CANAD NTWK'                              
T21316   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKDX-WORKD,T21316                                              
         LR    RA,RC                                                            
         USING WORKD,RA                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     R3,VTWA                                                          
         USING T213FFD,R3                                                       
TSB      USING TSARD,TSARBLK                                                    
*                                                                               
**********************************************************************          
*                                                                               
         MVI   PASS,1                INITIALIZE VARIABLES                       
         XC    LINCOUNT,LINCOUNT                                                
         XC    TOTG(52),TOTG                                                    
         XC    TOTPST(40),TOTPST                                                
         MVI   GSTCODE,0           RESET INITIAL VALUE                          
         XC    BUYCOUNT,BUYCOUNT                                                
*                                                                               
* INITIALIZE TSAR CORE BUFFER                                                   
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A5D'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAR,0(R1)                                                      
*                                                                               
         XC    TSARBLK,TSARBLK                                                  
         MVC   TSB.TSACOM,VCOMFACS                                              
         MVI   TSB.TSKEYL,TSBUYKLN            MKT-STA/EST/LIN                   
         MVC   TSB.TSRECL,=Y(TSBUYLEN)                                          
*                                                                               
         OI    TSB.TSINDS,TSINODSK                                              
         OI    TSB.TSIND2,TSI2MANY+TSI2BIGN   USE BOTH BUFFERS                  
         MVI   TSB.TSPAGN,20                                                    
*                                                                               
         LA    RE,MYTSREC                                                       
         ST    RE,TSB.TSAREC                                                    
*                                                                               
         MVI   TSB.TSACTN,TSAINI                                                
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSB.TSERRS,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,BUYTAB             CLEAR BUYLINE TABLE                        
         LHI   RF,BUYTABX-BUYTAB                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,BUYTAB                                                        
         AHI   RE,BUYTABX-BUYTAB                                                
         MVC   0(3,RE),=3X'FF'                                                  
*                                                                               
         LA    R0,OWRTAB                                                        
         LHI   R1,OWRTABX-OWRTAB                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,DFRTAB                                                        
         LHI   R1,DFRTABX-DFRTAB                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R4,DFRTAB                                                        
         USING DFRTABD,R4                                                       
*                                                                               
**********************************************************************          
*                                                                               
         BRAS  RE,BLDESTAB                                                      
*                                                                               
         LA    R2,PAYPRH                                                        
         MVC   KEY(9),SVKEY                                                     
         MVC   KEY+9(1),SVESTLO                                                 
         MVI   KEY+10,0                                                         
         CLI   KEY+3,X'FF'         TEST PAYING POL                              
         BE    *+8                                                              
         MVI   KEY+10,X'FF'                                                     
         MVC   KEY+11(2),SVRANLO                                                
*                                                                               
PAY10    MVI   RDUPDATE,C'N'       NO DIRECTORY UPDATE                          
         GOTO1 HIGH                                                             
         B     PAY14                                                            
*                                                                               
PAY12    GOTO1 SEQ                                                              
*                                                                               
PAY14    CLC   KEY(9),KEYSAVE      SAME A-M/CLT/PRD/MKT/STA                     
         BNE   PAY120                                                           
         CLC   KEY+9(1),SVESTHI    EST IN RANGE                                 
         BH    PAY120              NO                                           
         CLI   KEY+10,X'FF'        TEST POL PASSIVE                             
         BE    PAY16               YES - ACCEPT                                 
         CLI   KEY+10,0            IGNORE SPILL POINTERS                        
         BNE   PAY18               SPILL - SKIP TO NEXT EST                     
*                                                                               
PAY16    LLC   RE,KEY+9                                                         
         LA    RE,ESTTAB(RE)                                                    
         CLI   0(RE),0             TEST ESTIMATE ACTIVE                         
         BNE   PAY20               YES - GET BUY RECORD                         
*                                                                               
PAY18    MVC   KEY+10(3),=3X'FF'   FORCE NEXT EST                               
         B     PAY10                                                            
*                                                                               
PAY20    CLI   SVKEY+9,0           TEST PAYING ONE ESTIMATE                     
         BE    PAY30               NO                                           
         CLC   KEY+11(2),SVRANHI   TEST WITHIN RANGE OF BUYLINES                
         BH    PAY120                                                           
*                                                                               
PAY30    MVC   SVKEY+14(4),KEY+14    READ NETWORK BUYLINE INTO AREC2            
*                                                                               
         L     RE,AREC2                                                         
         ST    RE,AREC                                                          
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         GOTO1 GETREC                                                           
         MVI   BUYACTIV,C'N'                                                    
         NI    DMINBTS,X'FF'-X'08'                                              
*                                                                               
         L     R8,AREC2                                                         
         USING BUYRECD,R8                                                       
         TM    BUYRCNTL,X'80'      TEST RECORD DELETED                          
         BO    PAY12                                                            
*                                                                               
         OC    SVID,SVID           TEST PAYING BY ID                            
         BZ    PAY40               NO                                           
*                                                                               
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL           GET ID ELEMENT                               
         BE    PAY32               GOT IT                                       
         CLC   =C'NONE ',SVID      'NONE' PAYS BUYS WITHOUT ID'S                
         BE    PAY40                                                            
*                                                                               
PAY32    OC    3(12,R6),SPACES     INSURE SPACES IN ELEMENT                     
         CLC   3(12,R6),SVID                                                    
         BNE   PAY12                                                            
*                                                                               
PAY40    MVI   ERRCD,BADSPREP        SPECIAL REP CODE FROM NETWORK BUY          
         CLC   SVESTLO,SVESTHI       TEST PAYING ONE EST                        
         BNE   PAY42                                                            
         CLC   SVRANLO,SVRANHI       TEST PAYING RANGE OF LINES                 
         BNE   PAY42                 YES                                        
         CLC   BDREP,SVSPREP         NO - MATCH SPEC REP                        
         BE    PAY50                                                            
         B     PAYERR                                                           
*                                                                               
PAY42    CLC   BDREP,SVSPREP         PAYING RANGE - SKIP IF NO MATCH            
         BNE   PAY100                GET NEXT                                   
*                                                                               
PAY50    MVI   ELEMNO,0                                                         
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
         USING REGELEM,R6                                                       
PAY60    BAS   RE,NEXTEL                                                        
         BNE   PAY100                                                           
*                                                                               
         LLC   R0,ELEMNO             COUNT SPOT NUMBER WITHIN                   
         CLC   ELEMDT,RDATE          DATE FOR DEFERRED TABLE                    
         BE    *+6                                                              
         SR    R0,R0                                                            
         MVC   ELEMDT,RDATE                                                     
         TM    RSTATUS,X'80'                                                    
         BO    *+8                                                              
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
*                                                                               
         CLI   RLEN,10             TEST ALLOCATED                               
         BNH   PAY60               NO - IGNORE                                  
*                                                                               
         CLI   QPRD+3,X'FF'        TEST PAYING POL                              
         BE    PAY64               YES                                          
                                                                                
* MATCH ALLOCATION                                                              
                                                                                
         CLC   QPRD+3(1),10(R6)    MATCH PRD 1                                  
         BNE   PAY60                                                            
         CLI   1(R6),14            TEST P/B SPOT                                
         BH    PAY62               YES                                          
                                                                                
* NOT A P/B SPOT                                                                
                                                                                
         OC    QPRD2,QPRD2         TEST PAYING P/B SPOTS                        
         BNZ   PAY60               YES - SKIP                                   
         B     PAY64               ELSE PROCESS                                 
                                                                                
* SPOT IS P/B                                                                   
                                                                                
PAY62    OC    QPRD2,QPRD2         TEST PAYING P/B SPOTS                        
         BZ    PAY60               NO - SKIP                                    
         CLC   =C'ALL',QPRD2                                                    
         BE    PAY64                                                            
         CLC   QPRD2+3(1),14(R6)   MATCH PRD 2                                  
         BNE   PAY60                                                            
*                                                                               
PAY64    CLC   RDATE,SVSTARTP      TEST IF PRIOR TO PERIOD                      
         BL    *+14                                                             
         CLC   RDATE,SVENDP        TEST AFTER PERIOD                            
         BH    PAY60                                                            
*                                                                               
         BAS   RE,TESTPD                                                        
         BZ    PAY70               UNPAID                                       
         OI    BUYFOUND,X'10'      SET SPOT IN PERIOD                           
         B     PAY60                                                            
*                                                                               
PAY70    MVC   RUNDATE,2(R6)         MOVE ELEMENT DATE                          
*                                                                               
         MVI   OWRDAYS,0                                                        
         LLC   R0,BDSEDAY                                                       
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         CR    R0,R1                 TEST OUT-OF-WEEK ROT                       
         BNH   PAY80                                                            
         AHI   R1,7                  CALC DAYS TO END OF ROT                    
         SR    R1,R0                                                            
         STC   R1,OWRDAYS            SET OUT-OF-WEEK ROT FLAG                   
*                                                                               
         CLI   OWRDAYS,0             TEST OOWR                                  
         BE    PAY80                 NO                                         
*                                                                               
         CLI   SVPPROF+3,C'Y'        TEST PAY ONLY IF MATCHED                   
         BNE   PAY80                 NO                                         
         TM    SVTSTOPT,X'80'        UNLESS OPTION TO SUPPRESS                  
         BO    PAY80                                                            
         TM    RSTATUS,X'C0'         TEST MINUS OR MINUSSED                     
         BNZ   PAY80                                                            
         LLC   R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   0(R7),X'10'           TEST AFFID FOLLOWS                         
         BNE   PAY72                                                            
                                                                                
* SPOT IS MATCHED - PAY IF AFFID DATE IN THIS MONTH *                           
                                                                                
         MVC   RUNDATE,2(R7)       SAVE AFFID DATE                              
         B     PAY80                                                            
                                                                                
* SPOT IS NOT MATCHED - ADVANCE TO LAST DAY OF ROT TO TEST IN MONTH *           
                                                                                
PAY72    CLC   RUNDATE,SVENDP      TEST SPOT AFTER MONTH                        
         BH    PAY98               YES - ADD TO DEFERRED TABLE                  
*                                                                               
         GOTO1 VDATCON,DMCB,(2,2(R6)),WORK                                      
         LLC   R0,OWRDAYS                                                       
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(2,RUNDATE)                                  
*                                                                               
         CLC   RUNDATE,SVENDP      TEST SPOT AFTER PERIOD                       
         BH    PAY98               YES - DEFERRED                               
         CLC   RUNDATE,SVSTARTP    TEST SPOT BEFORE PERIOD                      
         BL    PAY60                                                            
         OI    BUYFOUND,X'10'      SET SPOT IN PERIOD                           
                                                                                
* CLEAR UNMATCHED SPOTS IF HAVE MATCH=NO ON BUYLINE                             
                                                                                
         ST    R6,DUB              SAVE ELEMENT ADDRESS                         
         MVC   DUB+4(2),ELCDLO     SAVE ELEMENT ARGS                            
*                                                                               
         MVI   ELCDLO,X'66'        SET COMMENT ELEM CODE                        
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
PAY74    BRAS  RE,NEXTEL                                                        
         BNE   PAY76ERR                                                         
         CLC   =C'MATCH=NO',3(R6)                                               
         BNE   PAY74                                                            
*                                                                               
         CLI   SVPPROFB+6,C'Z'     TEST CLEAR IF $0 AND UNMATCHED               
         BNE   PAY76               NO                                           
         OC    BDCOST,BDCOST       TEST $0 BUY                                  
         BE    PAY78               YES - DON'T SET UNMATCHED FLAG               
         B     PAY76ERR                                                         
*                                                                               
PAY76    CLI   SVPPROFB+6,C'N'     TEST MUST MATCH EVEN IF MATCH=NO             
         BNE   PAY78               NO - SO DON'T SET UNMATCHED FLAG             
*                                                                               
PAY76ERR LA    R2,PAYMDH                                                        
         MVI   ERRCD,MSSNGAFD                                                   
         B     PAYERR                                                           
*                                                                               
PAY78    L     R6,DUB              RESTORE ELEMENT ADDRESS                      
         MVC   ELCDLO(2),DUB+4     RESTORE ELCODE ARGS                          
                                                                                
*===========================================================                    
* RDATE IS IN THE PERIOD.                                                       
* IF RUNDATE IS AFTER PERIOD END DATE, ADD TO DFRTAB                            
*===========================================================                    
                                                                                
PAY80    CLC   RUNDATE,SVSTARTP    TEST IF PRIOR TO PERIOD                      
         BL    PAY60                                                            
                                                                                
         CLC   RUNDATE,SVENDP      TEST AFTER PERIOD                            
         BH    PAY98                                                            
         OI    BUYFOUND,X'10'      SET SPOT IN PERIOD                           
*                                                                               
         CLC   RUNDATE,RDATE       TEST DOING OOWR                              
         BE    PAY86               NO                                           
                                                                                
* ADD SPOT TO OWRTAB OF SPOTS TO BE CLEARED                                     
                                                                                
         CLC   RDATE,SVSTARTP      IF IN PERIOD DON'T NEED TABLE ENTRY          
         BNL   PAY86                                                            
*                                                                               
         LA    R1,OWRTAB                                                        
         LHI   R0,(OWRTABX-OWRTAB)/L'OWRTAB                                     
         USING OWRTABD,R1                                                       
*                                                                               
PAY82    OC    0(4,R1),0(R1)                                                    
         BZ    PAY84                                                            
         AHI   R1,L'OWRTAB                                                      
         BCT   R0,PAY82                                                         
         DC    H'0'                                                             
*                                                                               
PAY84    MVC   OWRLINE,KEY+11                                                   
         MVC   OWRDATE,ELEMDT                                                   
         MVC   OWRSPNUM,ELEMNO                                                  
         B     PAY94                 GO SET ACTIVITY FLAGS                      
         DROP  R1                                                               
*                                                                               
PAY86    CLI   SVPPROF+3,C'Y'        TEST PAY ONLY IF MATCHED                   
         BNE   PAY94                 NO                                         
         TM    SVTSTOPT,X'80'        UNLESS OPTION TO SUPPRESS                  
         BO    PAY94                                                            
         TM    RSTATUS,X'C0'         TEST MINUS OR MINUSSED                     
         BNZ   PAY94                                                            
         LLC   R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   0(R7),X'10'           TEST AFFID FOLLOWS                         
         BE    PAY94                                                            
                                                                                
* CLEAR UNMATCHED SPOTS IF HAVE MATCH=NO ON BUYLINE                             
                                                                                
         ST    R6,DUB              SAVE ELEMENT ADDRESS                         
         MVC   DUB+4(2),ELCDLO     SAVE ELEMENT ARGS                            
*                                                                               
         MVI   ELCDLO,X'66'        SET COMMENT ELEM CODE                        
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
PAY88    BRAS  RE,NEXTEL                                                        
         BNE   PAY90ERR                                                         
         CLC   =C'MATCH=NO',3(R6)                                               
         BNE   PAY88                                                            
*                                                                               
         CLI   SVPPROFB+6,C'Z'     TEST CLEAR IF $0 AND UNMATCHED               
         BNE   PAY90               NO                                           
         OC    BDCOST,BDCOST       TEST $0 BUY                                  
         BE    PAY92               YES - DON'T SET UNMATCHED FLAG               
         B     PAY90ERR                                                         
*                                                                               
PAY90    CLI   SVPPROFB+6,C'N'     TEST MUST MATCH EVEN IF MATCH=NO             
         BNE   PAY92               NO - SO DON'T SET UNMATCHED FLAG             
*                                                                               
PAY90ERR LA    R2,PAYMDH                                                        
         MVI   ERRCD,MSSNGAFD                                                   
         B     PAYERR                                                           
*                                                                               
PAY92    L     R6,DUB              RESTORE ELEMENT ADDRESS                      
         MVC   ELCDLO(2),DUB+4     RESTORE ELCODE ARGS                          
*                                                                               
PAY94    OI    BUYFOUND,X'01'      SET UNPAID SPOT IN PERIOD                    
         MVI   BUYACTIV,C'Y'       SET FLAG                                     
*                                                                               
         CLI   SVGSTVAL,0                                                       
         BNE   PAY96                                                            
         OC    SVNETPST,SVNETPST      ANY NETWORK PST VALUES                    
         BNZ   PAY96                                                            
         OC    SVOVRPST,SVOVRPST      ANY PST OVERRIDES                         
         BZ    PAY96X                                                           
*                                                                               
PAY96    GOTO1 PSTFIX                 (BLDPST IN SPPAY00)                       
*                                                                               
PAY96X   B     PAY60                                                            
*                                                                               
PAY98    MVC   DFREST,KEY+9                                                     
         MVC   DFRLINE,KEY+11        IF DEFERRED NETWORK BUY, STORE             
         MVC   DFRDATE,ELEMDT        BUYLINE, BUY DATE, AND SPOT NUMBER         
         MVC   DFRSPNUM,ELEMNO       SPOT NUMBER WITHIN DATE                    
         LA    R4,DFRLNQ(R4)         NEXT TABLE ENTRY                           
         LA    R0,DFRTABX                                                       
         CR    R4,R0                                                            
         BNH   PAY60                                                            
         LA    R2,PAYCLH                                                        
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(DFROVMAX)                                              
         GOTO1 ERROR                                                            
         DROP  R6                                                               
                                                                                
*=====================================================================          
* WHEN ALL ELEMENTS PROCESSED, ADD AN ENTRY TO BUYTAB IF BUY IS ACTIVE          
* AND ADD A TSAR ENTRY FOR EACH LOCAL STATION                                   
*=====================================================================          
                                                                                
PAY100   CLI   BUYACTIV,C'Y'       TEST BUYLINE ACTIVE                          
         BNE   PAY12               NO                                           
*                                                                               
         LH    RE,BUYCOUNT         ADD TO BUYTAB                                
         MHI   RE,L'BUYTAB                                                      
         LA    RE,BUYTAB(RE)                                                    
         USING BUYTABD,RE                                                       
*                                                                               
         MVC   BUYTBEST,BUYKEST                                                 
         MVC   BUYTBLIN,BUYKBUY                                                 
         MVC   BUYTBDSK,KEY+14      DISK ADDRESS                                
         DROP  RE                                                               
*                                                                               
         LH    RF,BUYCOUNT                                                      
         LA    RF,1(RF)                                                         
         STH   RF,BUYCOUNT                                                      
*                                                                               
         USING NTWKELEM,R6                                                      
         LA    R6,BDELEM                                                        
*                                                                               
PAY110   MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   PAY114                                                           
*                                                                               
         CLI   KEY+3,X'FF'         TEST PAYING POL                              
         BE    PAY112                                                           
         CLI   1(R6),11            TEST ELEM HAS ROOM FOR FLAG                  
         BNH   PAY112                                                           
         TM    NTWKFLG,NTWKFLG_CUTIN  TEST CUT-INS THIS STATION                 
         BZ    PAY112                                                           
         LA    R2,PAYPRH                                                        
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPAYCUT)                                              
         GOTO1 ERROR                                                            
*                                                                               
PAY112   XC    MYTSREC,MYTSREC                                                  
T        USING TSBUYD,MYTSREC                                                   
         MVC   T.TSBUYMKT(5),NTWKMKST   MOVE MKT/STA                            
         MVC   T.TSBUYEST,KEY+9                                                 
         MVC   T.TSBUYLIN,KEY+11                                                
         DROP  T                                                                
         MVI   TSB.TSACTN,TSAADD                                                
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSB.TSERRS,0                                                     
         BE    PAY110                                                           
         LA    R2,PAYMDH                                                        
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(MANYBUYS)   TOO MANY BUYS                              
         GOTO1 ERROR                                                            
*                                                                               
PAY114   B     PAY12                                                            
         EJECT                                                                  
*==================================================================             
* ALL NETWORK BUYLINES HAVE BEEN PROCESSED                                      
*==================================================================             
                                                                                
PAY120   MVC   0(3,R4),=3X'FF'       MARK END OF DFRTAB                         
         DROP  R4                                                               
*                                                                               
         OC    BUYTAB,BUYTAB       TEST ANY BUYS FOUND                          
         BNZ   PAY130              YES - GO READ LOCAL BUYS                     
*                                                                               
         LA    R2,PAYMDH                                                        
         MVC   NERRCD,=Y(NOBUYS)                                                
         TM    BUYFOUND,X'10'      TEST UNPAID SPOTS FOUND                      
         BZ    *+10                                                             
         MVC   NERRCD,=Y(NOUNPD)   THEN MUST BE NO UNPAID BUYS                  
         MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
         EJECT                                                                  
*=================================================================              
* READ LOCAL BUYS VIA TSAR RECORDS                                              
* AND POST BUY TOTALS TO TSAR BUFFER                                            
*=================================================================              
                                                                                
T        USING TSBUYD,MYTSREC                                                   
                                                                                
PAY130   XC    MYTSREC,MYTSREC                                                  
         MVI   TSB.TSACTN,TSAGET       READ ALL RECORDS IN TSAR BUFFER          
         LA    R0,1                                                             
         STH   R0,TSB.TSRNUM                                                    
         B     *+8                                                              
*                                                                               
PAY140   MVI   TSB.TSACTN,TSANXT                                                
         GOTO1 VTSAR,TSARBLK                                                    
*                                                                               
         CLI   TSB.TSERRS,0                                                     
         BE    PAY142                                                           
         TM    TSB.TSERRS,TSEEOF       TEST NO MORE RECORDS                     
         BO    PAY300                                                           
         DC    H'0'                                                             
*                                                                               
PAY142   XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY          A-M/CLT/PRD                                
         MVC   KEY+4(5),T.TSBUYMKT   MKT/STA                                    
         MVC   KEY+9(1),T.TSBUYEST   EST                                        
         MVC   KEY+11(2),T.TSBUYLIN  LIN                                        
*                                                                               
         CLI   KEY+3,X'FF'         TEST PAYING POL                              
         BE    PAY144                                                           
         MVI   KEY+10,X'FF'        SET TO READ POL PASSIVES                     
*                                                                               
PAY144   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADAFFIL                                                         
*                                                                               
         L     R8,AREC1                                                         
         ST    R8,AREC                                                          
         USING BUYRECD,R8                                                       
*                                                                               
PAY150   MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         GOTO1 GETREC                                                           
         MVI   BUYACTIV,C'N'                                                    
         NI    DMINBTS,X'FF'-X'08'                                              
*                                                                               
         TM    BUYRCNTL,X'80'      TEST RECORD DELETED                          
         BO    PAY140                                                           
*                                                                               
PAY160   CLI   SVGSTVAL,0             IF ANY GST OVERRIDE                       
         BNE   PAY162                 GO FIX RECORD                             
         OC    SVNETPST,SVNETPST      ANY NETWORK PST VALUES                    
         BNZ   PAY162                                                           
         OC    SVOVRPST,SVOVRPST      ANY PST OVERRIDES                         
         BZ    PAY170                 PST CODE                                  
PAY162   GOTO1 PSTFIX                 (BLDPST IN SPPAY00)                       
*                                                                               
         MVI   ELEMNO,0                                                         
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
         USING REGELEM,R6                                                       
PAY170   LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
PAY172   BRAS  RE,NEXTEL                                                        
         BNE   PAY250                                                           
*                                                                               
         LLC   R0,ELEMNO             COUNT SPOT NUMBER WITHIN                   
         CLC   ELEMDT,2(R6)          DATE FOR DEFERRED TABLE                    
         BE    *+6                                                              
         SR    R0,R0                                                            
         MVC   ELEMDT,2(R6)                                                     
         TM    6(R6),X'80'                                                      
         BO    *+8                                                              
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
*                                                                               
         CLI   RLEN,10                                                          
         BNH   PAY172                                                           
*                                                                               
         CLI   QPRD+3,X'FF'        TEST PAYING POL                              
         BE    PAY200              YES                                          
                                                                                
* MATCH ALLOCATION                                                              
                                                                                
         CLC   QPRD+3(1),10(R6)    MATCH PRD 1                                  
         BNE   PAY172                                                           
         CLI   1(R6),14            TEST P/B SPOT                                
         BH    PAY174              YES                                          
                                                                                
* NOT A P/B SPOT                                                                
                                                                                
         OC    QPRD2,QPRD2         TEST PAYING P/B SPOTS                        
         BNZ   PAY172              YES - SKIP                                   
         B     PAY200              ELSE PROCESS                                 
                                                                                
* SPOT IS P/B                                                                   
                                                                                
PAY174   OC    QPRD2,QPRD2         TEST PAYING P/B SPOTS                        
         BZ    PAY172              NO - SKIP                                    
         CLC   =C'ALL',QPRD2                                                    
         BE    PAY200                                                           
         CLC   QPRD2+3(1),14(R6)   MATCH PRD 2                                  
         BNE   PAY172                                                           
*                                                                               
PAY200   CLC   RDATE,SVENDP        IF AFTER END DATE - DON'T PAY                
         BH    PAY172                                                           
         CLC   RDATE,SVSTARTP                                                   
         BNL   PAY210                                                           
                                                                                
* IF PRIOR TO START DATE, CHECK OWR PAY TABLE AND PAY IF IT'S THERE             
                                                                                
         LA    R1,OWRTAB                                                        
         USING OWRTABD,R1                                                       
PAY206   CLC   OWRLINE,KEY+11                                                   
         BNE   PAY208                                                           
         CLC   OWRDATE,ELEMDT                                                   
         BNE   PAY208                                                           
         CLC   OWRSPNUM,ELEMNO                                                  
         BE    PAY210                                                           
*                                                                               
PAY208   AHI   R1,L'OWRTAB                                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   PAY206                                                           
         B     PAY172                                                           
*                                                                               
PAY210   BRAS  RE,TESTPD                                                        
         BNZ   PAY172                                                           
         DROP  R1                                                               
*                                                                               
         USING DFRTABD,RE                                                       
         LA    RE,DFRTAB                                                        
PAY212   CLC   0(3,RE),=3X'FF'     CHECK IF SPOT IS IN DEFERRED TABLE           
         BE    PAY216              SPOT NOT IN TABLE, CONTINUE                  
         CLC   DFREST,KEY+9                                                     
         BNE   PAY214                                                           
         CLC   DFRLINE,KEY+11                                                   
         BNE   PAY214                                                           
         CLC   DFRDATE,ELEMDT                                                   
         BNE   PAY214                                                           
         CLC   DFRSPNUM,ELEMNO                                                  
         BE    PAY172              DEFERRED SPOT, DON'T PAY                     
*                                                                               
PAY214   LA    RE,DFRLNQ(RE)       NEXT ENTRY                                   
         B     PAY212                                                           
         DROP  RE                                                               
*                                                                               
PAY216   CLI   PASS,2                                                           
         BNE   PAY220                                                           
         MVI   BUYACTIV,C'Y'                                                    
         MVC   12(1,R6),STATSEQ    SET CLEARANCE STATUS SEQNUM                  
         CLI   SVUNPAY,C'Y'        AND IF NOT UNPAYING                          
         BE    *+10                                                             
         MVC   RPAY,TODAYP         SET PAY DATE                                 
                                                                                
*==================================================================             
* ON FIRST PASS THROUGH AFFILIATE'S BUY RECORD, GET DOLLARS.                    
*==================================================================             
*                                                                               
PAY220   CLI   PASS,2                                                           
         BE    PAY172                                                           
*                                                                               
         XC    XCHDATA,XCHDATA                                                  
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),BUYREC,(C'Z',(R6)),          X        
               (C'C',XCHDATA)                                                   
*                                                                               
         L     R0,GROSS              ADD AFFILIATE'S GROSS TO BUYLINE'S         
         A     R0,T.TSBUYGRS         TOTAL GROSS                                
         ST    R0,T.TSBUYGRS                                                    
*                                                                               
         L     R0,NET                ADD AFFILIATE'S NET TO BUYLINE'S           
         A     R0,T.TSBUYNET         TOTAL NET                                  
         ST    R0,T.TSBUYNET                                                    
*                                                                               
         L     R0,XGSTAMT            ADD AFFILIATE'S GST TO BUYLINE'S           
         A     R0,T.TSBUYGST         TOTAL GST                                  
         ST    R0,T.TSBUYGST         TOTAL GST                                  
*                                                                               
         CLI   GSTCODE,0             TEST FIRST TIME                            
         BNE   *+10                                                             
         MVC   GSTCODE,XGSTCODE      ADD AFFILIATE'S GST TO BUYLINE'S           
*                                                                               
         CLC   GSTCODE,XGSTCODE      MAKE SURE SAME GST CODE                    
         BNE   GSTERR                                                           
                                                                                
* NOW ADD IN PST                                                                
                                                                                
         LA    R2,XPSTTAB                                                       
         LA    R4,10               COUNTER                                      
*                                                                               
PAY222   ICM   R0,15,XPSTAMT-XPSTPROV(R2)   PST FOR THIS PROVINCE               
         BZ    PAY224                                                           
         A     R0,T.TSBUYPST                                                    
         ST    R0,T.TSBUYPST                                                    
         MVC   T.TSBUYPCD,XPSTPROV          SET CODE FOR PROVINCE               
*                                                                               
PAY224   LA    R2,L'XPSTTAB(R2)                                                 
         BCT   R4,PAY222                                                        
*                                                                               
* NOW POST TO OLD PROGRAM ACCUMS TOO                                            
*                                                                               
         BAS   RE,ACCPST                                                        
         B     PAY172                                                           
*                                                                               
GSTERR   MVC   NERRCD,=Y(DIFFGST)                                               
*                                                                               
GSTERRX  MVI   ERRCD,NEWERRS                                                    
         LLC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R1),DUB                                                      
         MVI   3(R1),C'-'                                                       
         ICM   R0,3,BUYREC+10      GET LINE NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R1),DUB                                                      
         GOTO1 ERROR                                                            
*                                                                               
BADAFFIL LA    R1,DMCB                                                          
         LA    RE,BUYREC+4                                                      
         ST    RE,0(R1)                                                         
         LA    RE,WORK                                                          
         ST    RE,4(R1)                                                         
         LA    RE,WORK+4                                                        
         ST    RE,8(R1)                                                         
         BRAS  RE,GOMSUNPK                                                      
         MVC   ERRTEXT(4),WORK+4                                                
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOAFFBUY)                                              
         LA    R1,ERRTEXT+5                                                     
         B     GSTERRX                                                          
         EJECT                                                                  
*=====================================================================          
* POST PST TO OLD TOTALS SO THAT CHECKS WILL PICK UP PST!                       
*=====================================================================          
                                                                                
ACCPST   NTR1                                                                   
         LA    R1,XPSTPROV         PROVINCE CODES FROM GETRATE                  
         LA    R2,PSTCODE          10 X 1 BYTE PST VALUES                       
         LA    RE,TOTPST           10 X 4 BYTE PST ACCUMS                       
         LA    RF,10               COUNTER                                      
*                                                                               
ACCPST10 CLI   2(R1),C' '          IF NO PST CODE                               
         BNH   ACCPST20            SKIP                                         
         L     R0,0(RE)            ADD IN AMOUNT FOR PROVINCE                   
         A     R0,8(R1)                                                         
         ST    R0,0(RE)                                                         
         MVC   0(1,R2),2(R1)       SET CODE FOR PROVINCE                        
*                                                                               
ACCPST20 LA    R2,1(R2)            BUMP                                         
         LA    RE,4(RE)                                                         
         LA    R1,XPSTLEN(R1)                                                   
         BCT   RF,ACCPST10                                                      
         J     EXIT                                                             
                                                                                
*====================================================================           
* END OF BUY RECORD                                                             
*====================================================================           
                                                                                
PAY250   CLI   PASS,2              TEST PASS 2                                  
         BNE   PAY252              NO                                           
         CLI   BUYACTIV,C'Y'       TEST BUYREC UPDATED                          
         BNE   PAY254                                                           
         GOTO1 PUTREC                                                           
         B     PAY254                                                           
*                                                                               
PAY252   MVI   TSB.TSACTN,TSAPUT       WRITE DOLLARS TO TSAR                    
         GOTO1 VTSAR,TSARBLK                                                    
         CLI   TSB.TSERRS,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         BRAS  RE,BUMPTOTS         ADD DOLLARS TO TOTAL ACCUMS                  
*                                                                               
PAY254   OC    KEY+4(2),KEY+4      TEST DOING NETWORK BUYS                      
         BNZ   PAY140              NO - GET NEXT LOCAL BUY                      
         B     PAY395              GET NEXT BUYTAB ENTRY                        
         EJECT                                                                  
*==============================================================                 
* ALL LOCAL BUYLINES HAVE BEEN READ                                             
*==============================================================                 
                                                                                
PAY300   CLI   PASS,2               TEST SECOND PASS                            
         BE    PAY390                                                           
*                                                                               
         OC    SVTST(3),SVTST       TEST OPTION ACTIVE                          
         BZ    PAY302                                                           
         BRAS  RE,DSPTST            GO DISPLAY TST OPTION TOTALS                
*                                                                               
PAY302   MVI   ERRCD,ALLPAID        ON FIRST PASS ... AFTER ALL BUY-            
         TM    SVTSTOPT,X'10'       LINES HAVE BEEN PROCESSED ...               
         BZ    *+8                  TEST PAID OPTION ACTIVE                     
         MVI   ERRCD,ALLUNPD                                                    
         TM    BUYFOUND,X'10'      TEST UNPAID SPOT IN PERIOD                   
         BO    *+12                                                             
         LA    R2,PAYDTH                                                        
         B     PAYERR                                                           
*                                                                               
         CLI   SVUNPAY,C'Y'        ALLOW UNPAY TO GET THROUGH HERE              
         BE    PAY304                                                           
         CLI   SVPPROFB+3,C'Y'     ALLOW $0 CLEARANCES                          
         BE    PAY304              YES - THEN ALLOW THIS                        
         OC    TOTG,TOTG           TEST $0 CLEARANCE                            
         BNZ   PAY304                                                           
         OC    TOTGST,TOTGST       TEST NON-ZERO TAX                            
         BNZ   PAY302ER                                                         
         OC    TOTPST(40),TOTPST                                                
         BNZ   PAY302ER                                                         
         B     PAY304                                                           
*                                                                               
PAY302ER MVC   NERRCD,=Y(PAY0ERR)                                               
         MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
*                                                                               
PAY304   L     R0,TOTG              DEPENDING ON SVPPROF, USE GROSS             
         CLI   SVPPROF+0,C'G'       OR NET ... ADD TOTAL GST AND                
         BE    *+8                  TOTAL PST                                   
         L     R0,TOTN                                                          
         A     R0,TOTGST                                                        
*                                                                               
         LA    RE,TOTPST                                                        
         LA    RF,10                                                            
         A     R0,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   RF,*-8                                                           
*                                                                               
         CLI   SVPPROFB+5,1        TEST PAY INVOICE AMOUNT                      
         BNH   PAY310              NO                                           
                                                                                
*==============================================================                 
* NEW FEATURE TO PAY INVOICE AMOUNT WITHIN PERCENTAGE TOLERANCE                 
*==============================================================                 
                                                                                
         L     RF,TOTAMT           TOTAL OF INVOICES                            
         AR    RF,RF               X 2                                          
         LLC   RE,SVPPROFB+5       PCT DIFF TO 2 DEC (1%=100)                   
         MR    RE,RE                                                            
         D     RE,=F'10000'        SCALE DIFFERENCE TO PENNIES                  
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                GIVES MAX DIFFERENCE                         
         LPR   RF,RF               TAKE ABSVAL                                  
*                                                                               
         L     RE,TOTAMT           TOTAL OF INVOICES                            
         SR    RE,R0               LESS FILE TOTAL                              
         LPR   RE,RE               GIVES ABSVAL                                 
         CR    RE,RF               COMPARE TO ABSVAL OF DIFF                    
         BH    PAY360                                                           
         B     PAY312              WITHIN PERCENTAGE TOLERANCE                  
*                                                                               
PAY310   C     R0,TOTAMTLO          IF (LESS THAN TOTALAMTLO) OR                
         BL    PAY360               (GREATER THAN TOTALAMTHI) ...               
         C     R0,TOTAMTHI          CALL PAYICL AND EXIT                        
         BH    PAY360                                                           
         CLI   SVPPROFB+5,1         TEST PAY INVOICE AMOUNT BUT CHECK $         
         BL    PAY320                                                           
*                                                                               
PAY312   L     RE,TOTAMT           OK TO PAY INVOICE AMOUNT                     
         SR    RE,R0                                                            
         ST    RE,FILEDIFF         SAVE DIFF BETWEEN FILE/INVOICES              
*                                                                               
PAY320   MVI   STATSEQ,0                                                        
         CLI   SVUNPAY,C'Y'         IF UNPAYING ....                            
         BE    PAY350               CONTINUE TO PASS 2                          
         TM    SVTSTOPT,X'10'       IF PAID OPTION ... CALL PAYICL              
         BO    PAY360               AND EXIT                                    
*                                                                               
         L     RE,ASVAMTS          CHECK FOR INVOICE 1 AMOUNT 1                 
         LA    R1,AMTLEN(RE)       IS THERE MORE THAN 1 INVOICE                 
         CLI   AMTFLAGS-AMTD(R1),0                                              
         BNE   PAY338              YES THEN CONTINUE AS USUAL                   
         CLC   AMTINV-AMTD(4,RE),=C'1   '   INVOICE 1?                          
         BNE   PAY338                                                           
         CLC   AMT-AMTD(4,RE),=F'100'       AMOUNT 1?                           
         BE    PAY360              ERROR GIVEN FROM PAYICL                      
*                                                                               
PAY338   BRAS  RE,TSTLOCK           TEST DATA LOCKED                            
         BE    PAY340                                                           
         MVC   NERRCD,=AL2(DATALOCK)                                            
         MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
*                                                                               
PAY340   BAS   RE,UPDINV            MARK INVOICE PAID                           
         GOTO1 BLDSTAT              UPDATE CLEARANCE SEQ RECORD                 
*                                                                               
PAY350   MVI   PASS,2                                                           
         B     PAY130                                                           
*                                                                               
PAY360   GOTO1 PAYICL                                                           
         B     EXIT                  END OF PASS1                               
         EJECT                                                                  
                                                                                
*===================================================================            
* ALL LOCAL BUYS HAVE BEEN MARKED PAID -                                        
* NOW READ NETWORK BUYS IN BUYTAB AND MARK THEM PAID TOO                        
*===================================================================            
                                                                                
PAY390   LA    R5,BUYTAB                                                        
         USING BUYTABD,R5                                                       
*                                                                               
PAY392   XC    KEY,KEY               AT CONCLUSION OF SECOND PASS ...           
         MVC   KEY(9),SVKEY          READ NETWORK RECORD FOR EACH               
         MVC   KEY+9(1),BUYTBEST     BUYLINE IN BUYLINE TABLE INTO              
         MVC   KEY+11(2),BUYTBLIN    AREC2 AND MARK                             
         MVC   KEY+14(4),BUYTBDSK    MOVE DISK ADDRESS                          
         MVI   KEY+13,X'FF'          INDICATE DIDN'T READ DIR                   
         B     PAY150                                                           
*                                                                               
PAY395   LA    R5,L'BUYTAB(R5)       NEXT BUY TABLE ENTRY                       
         OC    0(L'BUYTAB,R5),0(R5)                                             
         BZ    PAY396                                                           
         CLC   =X'FFFFFF',0(R5)                                                 
         BNE   PAY392                                                           
*                                                                               
PAY396   CLI   SVUNPAY,C'Y'        TEST UNPAY REQUEST                           
         BNE   PAY397              NO                                           
         CLC   =C'REVCHECK',PAYER  TEST TO ISSUE UNPAY CHECK                    
         BNE   PAY398              NO                                           
*                                                                               
PAY397   GOTO1 PAYCHK               ON 2ND PASS, WRITE CHECK REQUESTS           
         DROP  R5                                                               
*                                                                               
**********************************************************************          
*                                                                               
PAY398   CLI   SVUNPAY,C'Y'                                                     
         BNE   EXIT                                                             
         XC    PAYMSG,PAYMSG                                                    
         MVC   PAYMSG(11),=C'BUYS UNPAID'                                       
         CLC   =C'REVCHECK',PAYER                                               
         BNE   EXIT                                                             
         MVC   PAYMSG+12(18),=C'AND CHECK REVERSED'                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         JL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         JH    NEXTEL                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
NEXTELX  LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
*                                                                               
PAYERR   GOTO1 ERROR                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*====================================================================*          
* ACCUMULATE GROSS AND NET TOTALS                                               
* AND POST TOTALS TO NETWORK ENTRY IN BUYTAB                                    
* NOTE THAT PST VALUES ARE ADDED INTO TOTPST ARRAY BY ACCPST                    
*====================================================================*          
         SPACE 1                                                                
BUMPTOTS NTR1                                                                   
         L     R0,T.TSBUYGRS                                                    
         A     R0,TOTG               ADD CURRENT BUYLINE'S GROSS, NET,          
         ST    R0,TOTG               AND GST TO TOTAL GROSS, TOTAL NET          
         L     R0,T.TSBUYNET         AND TOTAL GST FOR ALL BUYLINES             
         A     R0,TOTN                                                          
         ST    R0,TOTN                                                          
         L     R0,T.TSBUYGST                                                    
         A     R0,TOTGST                                                        
         ST    R0,TOTGST                                                        
*                                                                               
         LA    R5,BUYTAB                                                        
         USING BUYTABD,R5                                                       
*                                                                               
BUMPTOT2 CLC   BUYTBEST,BUYKEST                                                 
         BNE   BUMPTOT4                                                         
         CLC   BUYTBLIN,BUYKBUY                                                 
         BE    BUMPTOT6                                                         
*                                                                               
BUMPTOT4 LA    R5,L'BUYTAB(R5)                                                  
         CLC   =X'FFFFFF',0(R5)                                                 
         BNE   BUMPTOT2                                                         
         DC    H'0'                                                             
*                                                                               
BUMPTOT6 L     R0,T.TSBUYGRS                                                    
         A     R0,BUYTBGRS           ADD CURRENT BUYLINE'S GROSS, NET,          
         ST    R0,BUYTBGRS           AND GST TO TOTAL GROSS, TOTAL NET          
         L     R0,T.TSBUYNET         AND TOTAL GST FOR ALL BUYLINES             
         A     R0,BUYTBNET                                                      
         ST    R0,BUYTBNET                                                      
         L     R0,T.TSBUYGST                                                    
         A     R0,BUYTBGST                                                      
         ST    R0,BUYTBGST                                                      
         L     R0,T.TSBUYPST                                                    
         A     R0,BUYTBPST                                                      
         ST    R0,BUYTBPST                                                      
         J     EXIT                                                             
         EJECT                                                                  
*====================================================================*          
* SUBROUTINE SETS CONDITION CODES FOR PAID/UNPAID STATUS                        
* DEPENDING ON 'PAID' OPTION, 'UNPAY' OPTION                                    
* REMEMBER THAT UNPAY OPTION SETS 'PAID' OPTION AS WELL                         
*====================================================================*          
         SPACE 1                                                                
TESTPD   NTR1                                                                   
         TM    SVTSTOPT,X'10'      TEST PAID OPTION ACTIVE                      
         BO    TESTPD2             YES                                          
         OC    4(2,R6),4(R6)       TEST PAID                                    
         B     TESTPDX             AND RETURN WITH NORMAL CC                    
*                                                                               
TESTPD2  OC    KEY+4(2),KEY+4      TEST MARKET 0                                
         B     TESTPD2X            <==== NOP TO IGNORE DATE                     
         BZ    TESTPD4             YES - IGNORE DATE                            
*                                                                               
TESTPD2X OC    4(2,R6),4(R6)       TEST PAID                                    
         BZ    TESTPDNE            NO - SET TO IGNORE                           
         OC    SVPAYDT,SVPAYDT     TEST DATE SPECIFIED                          
         BZ    TESTPDEQ            NO -                                         
         CLC   SVPAYDT,4(R6)       TEST RIGHT DATE                              
         BNE   TESTPDNE                                                         
TESTPD4  CLI   SVUNPAY,C'Y'        TEST UNPAY OPTION ACTIVE                     
         BNE   TESTPDEQ            NO - EXIT                                    
         XC    4(2,R6),4(R6)       CLEAR PAYMENT DATE                           
*                                                                               
TESTPDEQ CR    RE,RE               SET CC EQ                                    
         B     TESTPDX                                                          
*                                                                               
TESTPDNE LTR   RE,RE               SET CC NEQ                                   
*                                                                               
TESTPDX  XIT1                                                                   
         EJECT                                                                  
GOMSUNPK NTR1                                                                   
         LM    R5,R7,0(R1)         GET MSUNPK PARAMS                            
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,PAYMD                                                    
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPMKST,0(R5)      MKTSTA                                       
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R6),STAPQMKT    RETURN RESULT                                
         MVC   0(5,R7),STAPQSTA                                                 
         LTR   R5,R5               R5 IS NEG FOR 8 BYTE OUTPUT                  
         BNM   *+10                                                             
         MVC   0(8,R7),STAPQSTA                                                 
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*====================================================================*          
* UPDATE INVOICE - MARK PAID                                                    
*====================================================================*          
         SPACE 1                                                                
UPDINV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ASVAMTS                                                       
         USING AMTD,R8                                                          
         LA    R0,MAXAMTS                                                       
*                                                                               
UPDINV2  DS    0H                                                               
         TM    AMTFLAGS,X'80'      TEST INVOICE PRESENT                         
         BZ    UPDINV10                                                         
*                                                                               
         XC    BIGKEY,BIGKEY       NOTE USES ** ELEM **                         
         LA    R5,BIGKEY                                                        
         USING SNVKEYD,R5                                                       
         MVI   SNVKTYPE,X'0E'                                                   
         MVI   SNVKSUB,X'03'                                                    
         MVC   SNVKAM,SVKEY        A/M                                          
         MVC   SNVKCLT,SVKEY+1     CLT                                          
         MVC   SNVKSTA,SVKEY+6     STATION                                      
* NOW GET END YMD                                                               
         GOTO1 VDATCON,DMCB,(2,SVENDP),(3,WORK)                                 
         MVI   WORK+2,1            SET TO 1ST OF MONTH                          
         GOTO1 (RF),(R1),(3,WORK),(2,SNVKMOS)                                   
         XC    SNVKMOS,=X'FFFF'                                                 
         MVC   SNVKINV,AMTINV      NOTE USE 10 CHAR INVOICE NUMBER              
         OC    SNVKINV,SPACES                                                   
         MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',BIGKEYSV,BIGKEY              
*                                                                               
         CLC   BIGKEY(22),BIGKEYSV  COMPARE THROUGH INVOICE NUMBER              
         BE    UPDINV4                                                          
         TM    SVTSTOPT,X'08'      TEST NOINV OPTION ON                         
         BO    UPDINV10            THEN CONTINUE PAYMENT WHEN NO INV            
         CLI   SVPPROFA+15,C'D'    DISALLOW PAYMENT IF NO INVOICE               
         BNE   UPDINV10            NO TRY NEXT INVOICE                          
         MVC   PAYMSG(15),=C'MISSING INVOICE'                                   
         MVC   PAYMSG+16(10),AMTINV                                             
         LA    R2,PAYMDH                                                        
         OI    6(R2),X'40'         CURSOR                                       
         DC    H'0',C'$ABEND'      NEED TO UNWIND OTHER INV PAID                
*                                                                               
UPDINV4  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL',SNVDDA,     X        
               AREC2,DMWORK                                                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AREC2                                                         
         LA    R5,SNVELS                                                        
         CLI   0(R5),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SNVHDELD,R5                                                      
         OI    SNVHDCTL,SNVHDPDQ   SET INVOICE PAID FLAG                        
         CLI   SVUNPAY,C'Y'        TEST UNPAY OPTION ACTIVE                     
         BNE   *+8                                                              
         NI    SNVHDCTL,X'FF'-SNVHDPDQ                                          
         GOTO1 (RF),(R1),=C'PUTREC'                                             
         DROP  R5                                                               
*                                                                               
UPDINV10 LA    R8,AMTLEN(R8)                                                    
         BCT   R0,UPDINV2                                                       
*                                                                               
UPDINVX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BU'    LOCK BUYS                                    
         L     RE,AREC                                                          
         MVC   L.LOCKMED,PAYMD                                                  
         MVC   L.LOCKCLT,QCLT                                                   
         MVC   L.LOCKSTA,QSTA                                                   
         CLI   L.LOCKSTA,C'0'                                                   
         BL    *+8                                                              
         MVI   L.LOCKSTA+4,C'/'                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
* TEST ALLOCATION KEYS                                                          
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BA'    LOCK BUYS                                    
         MVC   L.LOCKMED,PAYMD                                                  
         MVC   L.LOCKCLT,QCLT                                                   
         CLI   SVKEY+9,0           NO EST = LEAVE AS NULLS                      
         BE    TSTLOCK1            LOCKED IF ANY EST IS LOCKED                  
         SR    R0,R0                                                            
         IC    R0,SVKEY+9                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  L.LOCKEST,DUB                                                    
         MVC   DUB(3),L.LOCKEST                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
TSTLOCK1 BAS   RE,TSTIT                                                         
*                                                                               
         CLI   SVAPROF+7,C'C'      FOR CAN, MAKE SURE MED C NOT LOCKED          
         BNE   TSTLKEQ                                                          
         CLI   PAYMD,C'T'                                                       
         BE    *+12                                                             
         CLI   PAYMD,C'N'                                                       
         BNE   TSTLKEQ                                                          
         MVI   L.LOCKMED,C'C'                                                   
         CLI   SVKEY+9,0           NO EST = LEAVE AS NULLS                      
         BE    TSTLOCK3            LOCKED IF ANY EST IS LOCKED                  
         MVC   L.LOCKEST,DUB                                                    
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
TSTLOCK3 BAS   RE,TSTIT                                                         
         B     TSTLKEQ                                                          
         DROP  L                                                                
*                                                                               
TSTIT    LR    R0,RE                                                            
*                                                                               
TSTIT2   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTIT2                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* BUILD A TABLE OF ESTIMATES OVERLAPPING THE PAY PERIOD                         
*=================================================================              
                                                                                
BLDESTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ESTTAB,ESTTAB                                                    
         MVI   SVESTHI,X'FF'                                                    
         MVI   SVESTLO,0           MAY HAVE BEEN SET BY PREV TRANSACT!          
         CLI   SVKEY+9,0           TEST ESTIMATE ENTERED                        
         BE    BLDES2              NO                                           
         LLC   RE,SVKEY+9                                                       
         LA    RE,ESTTAB(RE)                                                    
         MVC   0(1,RE),SVKEY+9     SET VALUE IN TABLE                           
         MVC   SVESTLO,SVKEY+9                                                  
         MVC   SVESTHI,SVKEY+9                                                  
         B     BLDESTX                                                          
*                                                                               
BLDES2   XC    KEY,KEY                                                          
         MVC   KEY+1(3),SVKEY      MOVE A-M/CLT                                 
         MVC   KEY+4(3),=C'POL'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
BLDES4   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(7),KEYSAVE                                                   
         BNE   BLDES10                                                          
*                                                                               
         CLC   KEY+7(1),SVESTLO    TEST ESTNUM IN EST RANGE                     
         BL    BLDES4                                                           
         CLC   KEY+7(1),SVESTHI                                                 
         BH    BLDES10                                                          
*                                                                               
         L     R8,AREC2                                                         
         ST    R8,AREC                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R8                                                       
*                                                                               
         CLC   SVSTART,EEND        PERIOD START AFTER EST END                   
         BH    BLDES4                                                           
         CLC   SVEND,ESTART        PERIOD END BEFORE EST START                  
         BL    BLDES4                                                           
         LLC   RE,KEY+7            GET ESTIMATE NUMBER                          
         LA    RF,ESTTAB(RE)       POINT TO SLOT                                
         STC   RE,0(RF)            SET ESTNUM IN SLOT                           
         B     BLDES4                                                           
*                                                                               
BLDES10  OC    ESTTAB,ESTTAB       MAKE SURE FOUND AN ESTIMATE                  
         BZ    BLDESERR                                                         
                                                                                
* SET HIGH AND LOW ESTIMATE NUMBERS                                             
                                                                                
         CLI   SVESTLO,0                                                        
         JNZ   EXIT                                                             
*                                                                               
         LA    R1,ESTTAB                                                        
         LA    R0,255                                                           
*                                                                               
BLDES12  CLI   0(R1),0                                                          
         BNE   BLDES14                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
*                                                                               
BLDES14  LA    RE,ESTTAB                                                        
         SR    R1,RE                                                            
         STC   R1,SVESTLO                                                       
*                                                                               
         LA    R1,ESTTAB+255       NOW GO BACKWARDS                             
         CLI   0(R1),0                                                          
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         LA    RE,ESTTAB                                                        
         SR    R1,RE                                                            
         STC   R1,SVESTHI                                                       
*                                                                               
BLDESTX  J     EXIT                                                             
*                                                                               
BLDESERR MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPEREST) NO ESTIMATE IN PERIOD                        
         LA    R2,PAYDTH                                                        
         GOTO1 ERROR                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* DISPLAY BUYLINE TOTALS FROM TSAR BUFFER FOR NTEST                             
* NETWORK BUYLINE DOLLARS ARE DISPLAYED FROM BUYTAB                             
* LOCAL BUYLINE DOLLARS ARE DISPLAYED FROM TSAR BUFFER                          
*=================================================================              
                                                                                
DSPTST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    SVSUPMKST,SVSUPMKST   TEST DISPLAY 1 STATION ONLY                
         BNZ   DSPT10                                                           
*                                                                               
         OC    SVKEY+11(2),SVKEY+11  TEST PAYING ONLY 1 LINE                    
         BNZ   DSPT20                                                           
                                                                                
         LA    R5,BUYTAB                                                        
         USING BUYTABD,R5                                                       
*                                                                               
DSPT2    CLC   BUYTBEST,SVTST      GREATER THAN START EST-LIN                   
         BL    DSPT4               NO                                           
*                                                                               
         LA    R1,BUYTBEST                                                      
         BAS   RE,DSPESLN          SET EST-LIN IN WORK                          
         XC    BUYTOTS,BUYTOTS                                                  
         MVC   BUYTOTG,BUYTBGRS                                                 
         MVC   BUYTOTN,BUYTBNET                                                 
         MVC   BUYTGST,BUYTBGST                                                 
         MVC   BUYTPST,BUYTBPST                                                 
         GOTO1 PAYTEST             DISPLAY TEST DATA                            
*                                                                               
DSPT4    LA    R5,L'BUYTAB(R5)                                                  
         OC    0(L'BUYTAB,R5),0(R5) TEST MORE ENTRIES                           
         BZ    DSPTSTX                                                          
         CLC   =X'FFFFFF',0(R5)                                                 
         BNE   DSPT2                                                            
         B     DSPTSTX                                                          
*                                                                               
T        USING TSBUYD,MYTSREC                                                   
*                                                                               
DSPT10   DS    0H                                                               
         XC    MYTSREC,MYTSREC                                                  
         MVC   T.TSBUYMKT(4),SVSUPMKST                                          
         MVI   TSB.TSACTN,TSARDH                                                
         B     *+8                                                              
*                                                                               
DSPT12   MVI   TSB.TSACTN,TSANXT                                                
         GOTO1 VTSAR,TSARBLK                                                    
*                                                                               
         CLI   TSB.TSERRS,0                                                     
         BE    DSPT14                                                           
         TM    TSB.TSERRS,TSEEOF         TEST NO MORE RECORDS                   
         BO    DSPTSTX                                                          
*                                                                               
DSPT14   CLC   T.TSBUYMKT(4),SVSUPMKST   STILL MATCH MKT/STA                    
         BNE   DSPTSTX                                                          
         CLC   T.TSBUYEST,SVTSTEST       REACHED STARTING EST-LIN               
         BL    DSPT12                                                           
*                                                                               
         LA    R1,T.TSBUYEST                                                    
         BAS   RE,DSPESLN                                                       
         MVC   BUYTOTG,T.TSBUYGRS                                               
         MVC   BUYTOTN,T.TSBUYNET                                               
         MVC   BUYTGST,T.TSBUYGST                                               
         MVC   BUYTPST,T.TSBUYPST                                               
         GOTO1 PAYTEST                                                          
         B     DSPT12                                                           
*                                                                               
DSPT20   XC    MYTSREC,MYTSREC                                                  
         MVI   TSB.TSACTN,TSARDH                                                
         B     *+8                                                              
*                                                                               
DSPT22   MVI   TSB.TSACTN,TSANXT                                                
         GOTO1 VTSAR,TSARBLK                                                    
*                                                                               
         CLI   TSB.TSERRS,0                                                     
         BE    DSPT24                                                           
         TM    TSB.TSERRS,TSEEOF         TEST NO MORE RECORDS                   
         BO    DSPTSTX                                                          
*                                                                               
DSPT24   LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,PAYMD                                                    
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPMKST,T.TSBUYMKT   MKT/STA                                    
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         MVI   ERRCD,INVERR                                                     
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(4),STAPQSTA                                                 
         MVI   WORK+4,C'/'                                                      
         MVC   WORK+5(3),STAPQNET                                               
         DROP  R1                                                               
*                                                                               
         MVC   BUYTOTG,T.TSBUYGRS                                               
         MVC   BUYTOTN,T.TSBUYNET                                               
         MVC   BUYTGST,T.TSBUYGST                                               
         MVC   BUYTPST,T.TSBUYPST                                               
         GOTO1 PAYTEST                                                          
         B     DSPT22                                                           
*                                                                               
DSPTSTX  J     EXIT                                                             
*                                                                               
DSPESLN  LLC   R0,0(R1)            EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         MVI   WORK+3,C'-'                                                      
         SR    R0,R0                                                            
         ICM   R0,3,1(R1)          LINE                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+4(3),DUB                                                    
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*====================================================================*          
*        SAVED STORAGE DSECT                                                    
*====================================================================*          
         SPACE 1                                                                
WORKD    DSECT                                                                  
VTSAR    DS    V                                                                
BUYFOUND DS    XL1                 X'01'=SPOT IN PERIOD                         
BUYACTIV DS    CL1                                                              
*                                  X'10'=UNPAID SPOT IN PERIOD                  
LINCOUNT DS    H                                                                
BUYCOUNT DS    H                                                                
*                                                                               
         DS    0D                                                               
TSARBLK  DS    XL64                                                             
MYTSREC  DS    XL26                                                             
*                                                                               
         DS    0D                                                               
DFRTAB   DS    200XL6                                                           
         DS    XL1                                                              
DFRTABX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
OWRTAB   DS    200XL6                                                           
         DS    XL1                                                              
OWRTABX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
ESTTAB   DS    XL255                                                            
ESTTABX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
BUYTAB   DS    1024XL24                                                         
BUYTABX  DS    X                                                                
WORKDX   EQU   *                                                                
*                                                                               
TSBUYD   DSECT                                                                  
TSBUYMKT DS    XL2                                                              
TSBUYSTA DS    XL3                                                              
TSBUYEST DS    XL1                                                              
TSBUYLIN DS    XL2                                                              
TSBUYKLN EQU   *-TSBUYD                                                         
*                                                                               
TSBUYGRS DS    XL4                                                              
TSBUYNET DS    XL4                                                              
TSBUYGST DS    XL4                                                              
TSBUYPST DS    XL4                                                              
TSBUYPCD DS    CL2                 PROVINCE CODE                                
TSBUYLEN EQU   *-TSBUYD                                                         
*                                                                               
BUYTABD  DSECT                                                                  
BUYTBEST DS    CL1                                                              
BUYTBLIN DS    XL2                                                              
         DS    XL1                 SPARE                                        
BUYTBGRS DS    XL4                                                              
BUYTBNET DS    XL4                                                              
BUYTBGST DS    XL4                                                              
BUYTBPST DS    XL4                                                              
BUYTBDSK DS    XL4                 DISK ADDRESS OF NETWORK BUY                  
*                                                                               
DFRTABD  DSECT                                                                  
DFREST   DS    XL1                                                              
DFRLINE  DS    XL2                                                              
DFRDATE  DS    CL2                                                              
DFRSPNUM DS    XL1                                                              
DFRLNQ   EQU   *-DFRTABD                                                        
*                                                                               
OWRTABD  DSECT                                                                  
OWREST   DS    XL1                                                              
OWRLINE  DS    XL2                                                              
OWRDATE  DS    CL2                                                              
OWRSPNUM DS    XL1                                                              
OWRLNQ   EQU   *-OWRTABD                                                        
         EJECT                                                                  
       ++INCLUDE SPPAYWORK                                                      
         EJECT                                                                  
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    CL1                                                              
LOCKCLT  DS    CL3                                                              
LOCKSTA  DS    CL5                                                              
         ORG   LOCKSTA                                                          
LOCKEST  DS    CL3                                                              
         ORG                                                                    
*                                                                               
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'125SPPAY16   06/26/15'                                      
         END                                                                    
