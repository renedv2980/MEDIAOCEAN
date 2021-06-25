*          DATA SET SPPAY15    AT LEVEL 083 AS OF 10/23/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044603.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T21315C                                                                  
*===========================================================                    
* ===================>  IMPORTANT NOTE  <===================                    
* ===================>  IMPORTANT NOTE  <===================                    
* ===================>  IMPORTANT NOTE  <===================                    
* MHER 23OCT14                                                                  
* THIS PHASE IS **DEAD**. ALL CANADIAN CLEARANCES USE SPPAY16                   
* SPPAY01 FORCES 'SUPERPAY' OPTION IN SVPPROF+7                                 
*===========================================================                    
         TITLE 'T21315 - SPOTPAK PAY - CANAD NTWK'                              
T21315   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKDX-WORKD,T21315                                              
         LR    R8,RC                                                            
         USING WORKD,R8                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     R3,VTWA                                                          
         USING T213FFD,R3                                                       
*                                                                               
**********************************************************************          
*                                                                               
         MVI   PASS,1                INITIALIZE VARIABLES                       
         XC    KEY,KEY                                                          
         XC    LINCOUNT,LINCOUNT                                                
         XC    TOTG(52),TOTG                                                    
         MVI   SPOTSTAT,C'N'                                                    
         MVI   GSTCODE,0           RESET INITIAL VALUE                          
*                                                                               
         XC    STANUM,STANUM                                                    
         LA    R0,STATAB                                                        
         LHI   R1,STATABX-STATAB                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RE,BUYTAB             CLEAR BUYLINE TABLE                        
         LHI   RF,BUYTABX-BUYTAB                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         LA    RE,BUYTAB                                                        
         AHI   RE,BUYTABX-BUYTAB                                                
         MVI   0(RE),X'FF'                                                      
*                                                                               
**********************************************************************          
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
         LA    R2,PAYPRH             READHIGH FOR FIRST NETWORK                 
         MVI   ERRCD,NOBUYREC        BUYLINE                                    
         MVC   KEY(13),SVKEY                                                    
         MVC   KEY+11(2),SVRANLO                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE       NO BUYLINES                                
         BNE   PAYERR                                                           
         CLC   SVRANLO,SVRANHI                                                  
         BNE   PAY40                 IF PAYING OR UNPAYING ONLY ONE             
         CLC   KEY(13),KEYSAVE       BUYLINE ...                                
         BNE   PAYERR                INPUTTED NETWORK BUYLINE MUST              
         B     PAY40                 EXIST OR ERROR                             
*                                                                               
PAY30    CLC   SVRANLO,SVRANHI       AFTER FIRST NETWORK BUYLINE ...            
         BE    PAY115                READ SEQUENTIAL UNTIL CHANGE               
PAY32    MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                   OF ESTIMATE OR UNTIL HIGH RANGE OF         
         CLC   KEY(10),KEYSAVE       BUYLINES EXCEEDED                          
         BNE   PAY115                                                           
         CLC   KEY+11(2),SVRANHI                                                
         BH    PAY115                                                           
*                                                                               
PAY40    CLI   KEY+10,0              IGNORE SPILL POINTERS!                     
         BNE   PAY32                                                            
*                                                                               
         MVC   SVKEY+14(4),KEY+14    READ NETWORK BUYLINE INTO AREC2            
         L     RE,AREC2                                                         
         ST    RE,AREC                                                          
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R2,AREC2                                                         
         USING BUYRECD,R2                                                       
         MVI   BUYFOUND,C'N'                                                    
*                                                                               
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
         OC    SVID,SVID           TEST PAYING BY ID                            
         BZ    PAY50               NO                                           
         BRAS  RE,NEXTEL           GET ID ELEMENT                               
         BE    PAY42               GOT IT                                       
         CLC   =C'NONE ',SVID      'NONE' PAYS BUYS WITHOUT ID'S                
         BE    PAY50                                                            
*                                                                               
PAY42    OC    3(12,R6),SPACES     INSURE SPACES IN ELEMENT                     
         CLC   3(12,R6),SVID                                                    
         BNE   PAY100                                                           
*                                                                               
PAY50    MVI   ELEMNO,0                                                         
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
         USING REGELEM,R6            READ 0B AND 0C ELEMENTS FOR                
         LA    R6,BDELEM             NETWORK BUYLINE                            
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
PAY60    BAS   RE,NEXTEL                                                        
         BNE   PAY100                                                           
*                                                                               
         LLC   R0,ELEMNO             COUNT SPOT NUMBER WITHIN                   
         CLC   ELEMDT,2(R6)          DATE FOR DEFERRED TABLE                    
         BE    *+12                                                             
         MVC   ELEMDT,2(R6)                                                     
         SR    R0,R0                                                            
         TM    6(R6),X'80'                                                      
         BO    *+8                                                              
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
*                                                                               
         CLI   RLEN,10               TEST ALLOCATED                             
         BNH   PAY60                 NO - IGNORE                                
*                                                                               
PAY70    MVI   ERRCD,BADSPREP        SPECIAL REP CODE FROM NETWORK BUY          
         OC    SVRANHI,SVRANHI       TEST PAYING RANGE OF LINES                 
         BNE   PAY72                 YES                                        
         CLC   BDREP,SVSPREP         NO - MATCH SPEC REP                        
         BE    PAY74                                                            
         B     PAYERR                                                           
*                                                                               
PAY72    CLC   BDREP,SVSPREP         PAYING RANGE - SKIP IF NO MATCH            
         BNE   PAY100                GET NEXT                                   
*                                                                               
PAY74    BAS   RE,TESTPD                                                        
         BNZ   PAY60                                                            
*                                                                               
PAY80    MVC   RUNDATE,2(R6)         MOVE ELEMENT DATE                          
*                                                                               
         MVI   OWRDAYS,0                                                        
         LLC   R0,BDSEDAY                                                       
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         CR    R0,R1               TEST OUT-OF-WEEK ROT                         
         BNH   PAY84                                                            
         AHI   R1,7                CALC DAYS TO END OF ROT                      
         SR    R1,R0                                                            
         STC   R1,OWRDAYS          SET OUT-OF-WEEK ROT FLAG                     
*                                                                               
         CLI   OWRDAYS,0             TEST OOWR                                  
         BE    PAY84                 NO                                         
*                                                                               
         CLI   SVPPROF+3,C'Y'        TEST PAY ONLY IF MATCHED                   
         BNE   PAY84                 NO                                         
         TM    SVTSTOPT,X'80'        UNLESS OPTION TO SUPPRESS                  
         BO    PAY84                                                            
         TM    RSTATUS,X'C0'         TEST MINUS OR MINUSSED                     
         BNZ   PAY84                                                            
         LLC   R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   0(R7),X'10'           TEST AFFID FOLLOWS                         
         BNE   PAY82                                                            
                                                                                
* SPOT IS MATCHED - PAY IF AFFID DATE IN THIS MONTH *                           
                                                                                
         MVC   RUNDATE,2(R7)       SAVE AFFID DATE                              
         B     PAY84                                                            
                                                                                
* SPOT IS NOT MATCHED - ADVANCE TO LAST DAY OF ROT TO TEST IN MONTH *           
                                                                                
PAY82    CLC   RUNDATE,SVENDP      TEST SPOT AFTER MONTH                        
         BH    PAY94               YES - DEFERRED                               
*                                                                               
         GOTO1 VDATCON,DMCB,(2,2(R6)),WORK                                      
         LLC   R0,OWRDAYS                                                       
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(2,RUNDATE)                                  
*                                                                               
         CLC   RUNDATE,SVENDP      TEST SPOT AFTER PERIOD                       
         BH    PAY94               YES - DEFERRED                               
         CLC   RUNDATE,SVSTARTP    TEST SPOT BEFORE PERIOD                      
         BL    PAY60                                                            
                                                                                
* CLEAR UNMATCHED SPOTS IF HAVE MATCH=NO ON BUYLINE                             
                                                                                
         ST    R6,DUB              SAVE ELEMENT ADDRESS                         
         MVC   DUB+4(2),ELCDLO     SAVE ELEMENT ARGS                            
*                                                                               
         MVI   ELCDLO,X'66'        SET COMMENT ELEM CODE                        
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
PAY83A   BRAS  RE,NEXTEL                                                        
         BNE   PAY83ERR                                                         
         CLC   =C'MATCH=NO',3(R6)                                               
         BNE   PAY83A                                                           
*                                                                               
         CLI   SVPPROFB+6,C'Z'     TEST CLEAR IF $0 AND UNMATCHED               
         BNE   PAY83B              NO                                           
         OC    BDCOST,BDCOST       TEST $0 BUY                                  
         BE    PAY83X              YES - DON'T SET UNMATCHED FLAG               
         B     PAY83ERR                                                         
*                                                                               
PAY83B   CLI   SVPPROFB+6,C'N'     TEST MUST MATCH EVEN IF MATCH=NO             
         BNE   PAY83X              NO - SO DON'T SET UNMATCHED FLAG             
*                                                                               
PAY83ERR LA    R2,PAYMDH                                                        
         MVI   ERRCD,MSSNGAFD                                                   
         B     PAYERR                                                           
*                                                                               
PAY83X   L     R6,DUB              RESTORE ELEMENT ADDRESS                      
         MVC   ELCDLO(2),DUB+4     RESTORE ELCODE ARGS                          
*                                                                               
PAY84    CLC   RUNDATE,SVSTARTP      TEST IF PRIOR TO PERIOD                    
         BL    PAY60                                                            
         CLC   RUNDATE,SVENDP        TEST AFTER PERIOD                          
         BH    PAY60                                                            
*                                                                               
         CLC   RUNDATE,RDATE       TEST DOING OOWR                              
         BE    PAY90               NO                                           
                                                                                
* ADD SPOT TO OWRTAB OF SPOTS TO BE CLEARED                                     
                                                                                
         CLC   RDATE,SVSTARTP      IF IN PERIOD DON'T NEED TABLE ENTRY          
         BNL   PAY90                                                            
*                                                                               
         LA    R1,OWRTAB                                                        
         LHI   R0,(OWRTABX-OWRTAB)/L'OWRTAB                                     
         USING OWRTABD,R1                                                       
*                                                                               
PAY86A   OC    0(L'OWRTAB,R1),0(R1)                                             
         BZ    PAY86B                                                           
         LA    R1,L'OWRTAB(R1)                                                  
         BCT   R0,PAY86A                                                        
         DC    H'0'                                                             
*                                                                               
PAY86B   MVC   OWRLINE,KEY+11                                                   
         MVC   OWRDATE,ELEMDT                                                   
         MVC   OWRSPNUM,ELEMNO                                                  
         B     PAY92                 GO SET ACTIVITY FLAGS                      
         DROP  R1                                                               
*                                                                               
PAY90    CLI   SVPPROF+3,C'Y'        TEST PAY ONLY IF MATCHED                   
         BNE   PAY92                 NO                                         
         TM    SVTSTOPT,X'80'        UNLESS OPTION TO SUPPRESS                  
         BO    PAY92                                                            
         TM    RSTATUS,X'C0'         TEST MINUS OR MINUSSED                     
         BNZ   PAY92                                                            
         LLC   R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   0(R7),X'10'           TEST AFFID FOLLOWS                         
         BE    PAY92                                                            
* CLEAR UNMATCHED SPOTS IF HAVE MATCH=NO ON BUYLINE                             
         ST    R6,DUB              SAVE ELEMENT ADDRESS                         
         MVC   DUB+4(2),ELCDLO     SAVE ELEMENT ARGS                            
*                                                                               
         MVI   ELCDLO,X'66'        SET COMMENT ELEM CODE                        
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
PAY91A   BRAS  RE,NEXTEL                                                        
         BNE   PAY91ERR                                                         
         CLC   =C'MATCH=NO',3(R6)                                               
         BNE   PAY91A                                                           
*                                                                               
         CLI   SVPPROFB+6,C'Z'     TEST CLEAR IF $0 AND UNMATCHED               
         BNE   PAY91B              NO                                           
         OC    BDCOST,BDCOST       TEST $0 BUY                                  
         BE    PAY91X              YES - DON'T SET UNMATCHED FLAG               
         B     PAY91ERR                                                         
*                                                                               
PAY91B   CLI   SVPPROFB+6,C'N'     TEST MUST MATCH EVEN IF MATCH=NO             
         BNE   PAY91X              NO - SO DON'T SET UNMATCHED FLAG             
*                                                                               
PAY91ERR LA    R2,PAYMDH                                                        
         MVI   ERRCD,MSSNGAFD                                                   
         B     PAYERR                                                           
*                                                                               
PAY91X   L     R6,DUB              RESTORE ELEMENT ADDRESS                      
         MVC   ELCDLO(2),DUB+4     RESTORE ELCODE ARGS                          
*                                                                               
PAY92    MVI   BUYFOUND,C'Y'                                                    
         MVI   SPOTSTAT,C'Y'       SET SPOT FOUND                               
         B     PAY60                                                            
*                                                                               
PAY94    MVC   DFRLINE,KEY+11        IF DEFERRED NETWORK BUY, STORE             
         MVC   DFRDATE,ELEMDT        BUYLINE, BUY DATE, AND SPOT NUMBER         
         MVC   DFRSPNUM,ELEMNO       SPOT NUMBER WITHIN DATE                    
         LA    R4,DFRLNQ(R4)         NEXT TABLE ENTRY                           
         B     PAY60                                                            
         DROP  R6                                                               
*                                                                               
**********************************************************************          
*                                                                               
PAY100   CLI   BUYFOUND,C'N'                                                    
         BE    PAY30                 WHEN ALL 0B AND 0C ELEMENTS FOR            
         SR    RF,RF                                                            
         ICM   RF,3,KEY+11           NETWORK BUYLINE HAVE BEEN PRO-             
         LA    RE,BUYTAB             CESSED ... IF (ANY FALL WITHIN             
         BCTR  RF,0                  BROADCAST MONTH) OR (MATCH UNPAY           
         MHI   RF,L'BUYTAB           DATE) MARK THE APPROPRIATE BYTE            
         AR    RE,RF                 IN BUYLINE TABLE                           
         MVI   BUYMARK-BUYTABD(RE),1                                            
*                                                                               
         USING NTWKELEM,R6                                                      
         LA    R6,BDELEM                                                        
PAY110   LLC   R0,1(R6)              ... SAVE ALL OF THE NETWORK'S              
         AR    R6,R0                 MARKET-STATIONS (FROM THE 68               
         CLI   0(R6),0               ELEMENTS) INTO THE STATION TABLE           
         BE    PAY30                                                            
         CLI   0(R6),X'68'                                                      
         BNE   PAY110                                                           
*                                                                               
         L     R5,STANUM                                                        
         L     RF,VCOMFACS                                                      
         L     RF,CBINSRCH-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(01,NTWKMKST),STATAB,(R5),5,(2,3),60                   
         MVC   STANUM,8(R1)                                                     
         OC    DMCB(4),DMCB                                                     
         BNZ   PAY110                                                           
         DC    H'00'                                                            
*                                                                               
**********************************************************************          
*                                                                               
*                                    AFTER ALL BUYLINES FOR NETWORK             
*                                    HAVE BEEN PROCESSED ...                    
*                                                                               
PAY115   MVI   0(R4),X'FF'           MARK END OF DFRTAB                         
         DROP  R4                                                               
*                                                                               
         MVI   ERRCD,NOSPOTS         MUST FIND AT LEAST 1 NON-MINUS OB          
         CLI   SPOTSTAT,C'Y'         OR OC ELEMENT IN AT LEAST 1 NET-           
         BE    PAY120                WORK BUYLINE RECORD OR ERROR               
         LA    R2,PAYDTH                                                        
         B     PAYERR                                                           
*                                                                               
PAY120   LA    R4,STATAB             FOR EACH AFFILIATE IN THE STATION          
PAY130   XC    BUYTOTG(12),BUYTOTG   TABLE ...                                  
         XC    BUYTPST(40),BUYTPST   CLEAR TOTAL GROSS, TOTAL NET,              
         CLC   0(5,R4),=5X'00'       TOTAL GST AND TOTAL PST                    
         BE    PAY280                ACCUMULATORS                               
*                                                                               
         XC    KEY,KEY               READHIGH (OR READ SEQUENTIAL)              
         MVC   KEY(10),SVKEY         THE AFFILIATE'S FIRST (OR NEXT)            
         MVC   KEY+4(5),0(R4)        BUYLINE (UNTIL ESTIMATE NO                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                  LONGER MATCHES)                            
         B     PAY150                                                           
*                                                                               
PAY140   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PAY150   CLC   KEY(10),KEYSAVE                                                  
         BE    PAY170                                                           
*                                                                               
**********************************************************************          
*                                                                               
         CLI   SVLINST,C'L'          WHEN (NO MORE BUYLINES FOUND FOR           
         BE    PAY160                AFFILIATE'S ESTIMATE) AND IF               
         MVC   WORK,SPACES           (DISPLAYING TOTALS BY STATION) ...         
         L     RE,AREC                                                          
         LA    RE,4(RE)                                                         
         ST    RE,DMCB                                                          
         GOTO1 ,DMCB,,DUB,WORK                                                  
         BAS   RE,GOMSUNPK                                                      
*                                                                               
         CLI   SVSTALO+3,X'FF'        IF A STATION ALREADY DISPLAYED            
         BE    PAY153                 ... DISPLAY ALL AFTER IT                  
*                                                                               
         LA    RE,4                   IF NO STATIONS ALREADY DISPLAYED          
         CLI   SVSTALO+3,X'F0'        ... CURRENT STATION MUST EQUAL            
         BL    PAY151                 OR BE GREATER THAN STATION FILTER         
         LLC   RE,SVSTALO+3           OR DON'T DISPLAY                          
PAY151   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SVSTALO(0),WORK                                                  
         BH    PAY155                                                           
         MVI   SVSTALO+3,X'FF'                                                  
         CLC   SVTSTSTA,WORK                                                    
         BH    PAY160                                                           
*                                                                               
PAY153   TM    PASS,2                                                           
         BO    PAY155                CALL PAYEST TO DISPLAY TOTALS FOR          
         MVI   WORK+4,C' '           CURRENT AFFILIATE ...                      
         GOTO1 PAYTEST                                                          
*                                                                               
PAY155   BAS   RE,ACCTOTS            ACCUMULATE TOTAL GROSS, TOTAL NET,         
         BAS   RE,ACCTPST            TOTAL GST AND TOTAL PST                    
*                                                                               
*                                    BUMP TO NEXT AFFILIATE IN STATION          
PAY160   LA    R4,5(R4)              TABLE WHEN NO MORE BUYLINES FOUND          
         B     PAY130                FOR ESTIMATE                               
*                                                                               
**********************************************************************          
*                                                                               
*                                    IF (BUYLINE FOUND FOR ESTIMATE)            
PAY170   CLI   SVLINST,C'L'          AND (IF DISPLAYING TOTALS BY BUY-          
         BNE   PAY180                LINE) ... CLEAR TOTAL GROSS, TOTAL         
         XC    BUYTOTG(12),BUYTOTG   NET,TOTAL GST AND TOTAL PST ACC-           
         XC    BUYTPST(40),BUYTPST   UMULATORS                                  
*                                                                               
**********************************************************************          
*                                                                               
         USING BUYTABD,R5                                                       
PAY180   CLI   KEY+10,0              IGNORE SPILL POINTERS                      
         BNE   PAY250                                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,KEY+11           IF (BUYLINE FOUND FOR ESTIMATE)            
         LA    R5,BUYTAB             BUYLINE FOR AFFILIATE MUST BE IN           
         BCTR  RF,0                  BUYLINE TABLE OR READ SEQUENTIAL           
         MHI   RF,L'BUYTAB           FOR AFFILIATE'S NEXT BUYLINE               
         AR    R5,RF                                                            
         CLI   BUYMARK,1                                                        
         BNE   PAY140                                                           
*                                                                               
         L     R2,AREC1              GET BUYLINE REC INTO AREC1                 
         ST    R2,AREC                                                          
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,SETPST                                                        
*                                                                               
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
         OC    SVID,SVID           TEST PAYING BY ID                            
         BZ    PAY190              NO                                           
         BRAS  RE,NEXTEL           GET ID ELEMENT                               
         BE    PAY182              GOT IT                                       
         CLC   =C'NONE ',SVID      'NONE' PAYS BUYS WITHOUT ID'S                
         BE    PAY190                                                           
*                                                                               
PAY182   OC    3(12,R6),SPACES     INSURE SPACES IN ELEMENT                     
         CLC   3(12,R6),SVID                                                    
         BNE   PAY250                                                           
*                                                                               
**********************************************************************          
*                                                                               
PAY190   CLI   SVGSTVAL,0          IF ANY GST OVERRIDE                          
         BNE   *+14                ADD ELEMENT TO BUY                           
         OC    SVOVRPST,SVOVRPST   FIX THE AFFILIATE'S BUYLINE'S                
         BE    PAY200              PST CODE                                     
         GOTO1 PSTFIX                                                           
*                                                                               
**********************************************************************          
*                                                                               
         MVI   ELEMNO,0                                                         
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
         USING REGELEM,R6                                                       
PAY200   LA    R6,BDELEM             READ IN 0B AND 0C ELEMENTS FOR             
         MVI   ELCDLO,X'0B'          AFFILIATE'S BUYLINE                        
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
PAY202   BAS   RE,NEXTEL                                                        
         BNE   PAY250                                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,ELEMNO             COUNT SPOT NUMBER WITHIN                   
         CLC   ELEMDT,2(R6)          DATE FOR DEFERRED TABLE                    
         BE    *+12                                                             
         MVC   ELEMDT,2(R6)                                                     
         SR    R0,R0                                                            
         TM    6(R6),X'80'                                                      
         BO    *+8                                                              
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
*                                                                               
         CLI   RLEN,10                                                          
         BNH   PAY202                                                           
         CLC   RDATE,SVENDP        IF AFTER END DATE - DON'T PAY                
         BH    PAY202                                                           
         CLC   RDATE,SVSTARTP                                                   
         BNL   PAY210                                                           
                                                                                
* IF PRIOR TO START DATE, CHECK OWR PAY TABLE AND PAY IF IT'S THERE             
                                                                                
         LA    R1,OWRTAB                                                        
         USING OWRTABD,R1                                                       
PAY204A  CLC   OWRLINE,KEY+11                                                   
         BNE   PAY204B                                                          
         CLC   OWRDATE,ELEMDT                                                   
         BNE   PAY204B                                                          
         CLC   OWRSPNUM,ELEMNO                                                  
         BE    PAY210                                                           
*                                                                               
PAY204B  LA    R1,L'OWRTAB(R1)                                                  
         OC    0(L'OWRTAB,R1),0(R1)                                             
         BNZ   PAY204A                                                          
         B     PAY202                                                           
*                                                                               
PAY210   BAS   RE,TESTPD                                                        
         BNZ   PAY202                                                           
         OC    BUYMSTA(2),BUYMSTA    TEST EXPLODED BUY                          
         BZ    PAY213                                                           
         DROP  R1                                                               
*                                                                               
         USING DFRTABD,RE                                                       
         LA    RE,DFRTAB           IF EXPLODED BUY, CHECK IF                    
PAY211   CLI   0(RE),X'FF'         SPOT IS IN DEFERRED TABLE                    
         BE    PAY215              SPOT NOT IN TABLE, CONTINUE                  
         CLC   DFRLINE,KEY+11                                                   
         BNE   PAY212                                                           
         CLC   DFRDATE,ELEMDT                                                   
         BNE   PAY212                                                           
         CLC   DFRSPNUM,ELEMNO                                                  
         BE    PAY202              DEFERRED SPOT, DON'T PAY                     
PAY212   LA    RE,DFRLNQ(RE)       NEXT ENTRY                                   
         B     PAY211                                                           
         DROP  RE                                                               
*                                                                               
PAY213   CLI   SVPPROF+3,C'Y'        IF NETWORK BUY AND (PAY ONLY IF            
         BNE   PAY215                MATCHED) AND EITHER AFD OPTION ON          
         TM    SVTSTOPT,X'80'        OR (MINUS OR MINUSED) ... ELEMENT          
         BO    PAY215                AFTER THE 0B OR 0C ELEMENT MUST            
         TM    RSTATUS,X'C0'         BE AFFIDAVIT ELEMENT                       
         BNZ   PAY215                                                           
         LLC   R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   0(R7),X'10'                                                      
         BE    PAY215                                                           
* LOOK FOR MATCH=NO COMMENT ON BUYLINE                                          
         ST    R6,DUB              SAVE ELEMENT ADDRESS                         
         MVC   DUB+4(2),ELCDLO     SAVE ELEMENT ARGS                            
*                                                                               
         MVI   ELCDLO,X'66'        SET COMMENT ELEM CODE                        
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
PAY213A  BRAS  RE,NEXTEL                                                        
         BNE   PAY213ER                                                         
         CLC   =C'MATCH=NO',3(R6)                                               
         BNE   PAY213A                                                          
*                                                                               
         CLI   SVPPROFB+6,C'Z'     TEST CLEAR IF $0 AND UNMATCHED               
         BNE   PAY213B             NO                                           
         OC    BDCOST,BDCOST       TEST $0 BUY                                  
         BE    PAY213X             YES - DON'T SET UNMATCHED FLAG               
         B     PAY213ER                                                         
*                                                                               
PAY213B  CLI   SVPPROFB+6,C'N'     TEST MUST MATCH EVEN IF MATCH=NO             
         BNE   PAY213X             NO - SO DON'T SET UNMATCHED FLAG             
*                                                                               
PAY213ER LA    R2,PAYMDH                                                        
         MVI   ERRCD,MSSNGAFD                                                   
         B     PAYERR                                                           
*                                                                               
PAY213X  L     R6,DUB              RESTORE ELEMENT ADDRESS                      
         MVC   ELCDLO(2),DUB+4     RESTORE ELCODE ARGS                          
*                                                                               
**********************************************************************          
*                                                                               
PAY215   OI    PASS,X'40'          ON SECOND PASS THROUGH AFFILIATE'S           
         TM    PASS,2              BUY RECORDS ... SET AFFILIATE'S              
         BZ    PAY220              CLEARANCE SEQUENCE NUMBER ... AND            
         OI    PASS,X'80'          IF NOT UNPAYING ... MOVE PAY DATE            
         MVC   12(1,R6),STATSEQ    IN ... AND SET NO GST FOR DEC31/90           
         CLI   SVUNPAY,C'Y'        SPOTS                                        
         BE    *+10                                                             
         MVC   RPAY,TODAYP                                                      
                                                                                
*==================================================================             
* ON FIRST PASS THROUGH AFFILIATE'S BUY RECORD                                  
* ALSO, IF HST OR QST ON NETWORK STATION REC, OVERRIDE LOCAL DATA               
*==================================================================             
*                                                                               
PAY220   TM    PASS,2                                                           
         BO    PAY202                                                           
*                                                                               
         XC    XCHDATA,XCHDATA                                                  
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),BUYREC,(C'Z',(R6)),          X        
               (C'C',XCHDATA)                                                   
*                                                                               
         L     R0,GROSS              ADD AFFILIATE'S GROSS TO BUYLINE'S         
         A     R0,BUYTOTG            TOTAL GROSS                                
         ST    R0,BUYTOTG                                                       
         L     R0,NET                ADD AFFILIATE'S NET TO BUYLINE'S           
         A     R0,BUYTOTN            TOTAL NET                                  
         ST    R0,BUYTOTN                                                       
         L     R0,XGSTAMT            ADD AFFILIATE'S GST TO BUYLINE'S           
         A     R0,BUYTGST            TOTAL GST                                  
         ST    R0,BUYTGST                                                       
*                                                                               
         CLI   GSTCODE,0           TEST FIRST TIME                              
         BNE   *+10                                                             
         MVC   GSTCODE,XGSTCODE      ADD AFFILIATE'S PST TO BUYLINE'S           
*                                                                               
         CLI   SVUNPAY,C'Y'        NO ERROR ON UNPAY                            
         BE    PAY221                                                           
*                                                                               
         CLC   GSTCODE,XGSTCODE      MAKE SURE SAME GST CODE                    
         BNE   GSTERR                                                           
PAY221   BAS   RE,ACCPST             TOTAL PST                                  
*                                                                               
         CLI   SVLINST,C'L'                                                     
         BNE   PAY202                                                           
         L     R0,GROSS              IF (DISPLAYING TOTALS BY BUYLINE)          
         ICM   RF,15,BUYGRS          ... ADD AFFILIATE'S GROSS TO               
         AR    RF,R0                 CURRENT BUYLINE'S TOTAL GROSS ...          
         STCM  RF,15,BUYGRS          AND ADD AFFILIATE'S NET TO CURR-           
         L     R0,NET                ENT BUYLINE'S TOTAL NET.                   
         ICM   RF,15,BUYNET          BUYLINE'S TOTAL GROSS AND NET ARE          
         AR    RF,R0                 STORED IN BUYLINE TABLE                    
         STCM  RF,15,BUYNET                                                     
         B     PAY202                                                           
*                                                                               
GSTERR   MVC   NERRCD,=Y(DIFFGST)                                               
         MVI   ERRCD,NEWERRS                                                    
         SR    R0,R0                                                            
         ICM   R0,3,BUYREC+10        GET LINE NUMBER                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRTEXT(3),DUB                                                   
         GOTO1 ERROR                                                            
*                                                                               
**********************************************************************          
*                                                                               
PAY250   TM    PASS,2                AT CONCLUSION OF THE SECOND PASS           
         BZ    PAY260                THROUGH AFFILIATE'S 0B AND 0C              
         TM    PASS,X'80'            ELEMENTS ... IF AFFILIATE RECORD           
         BZ    PAY140                HAS BEEN UPDATED, PUT IT AND               
         NI    PASS,X'7F'            PROCESS NEXT AFFILIATE                     
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 PUTREC                                                           
         B     PAY140                                                           
*                                                                               
**********************************************************************          
*                                                                               
*                                    AT CONCLUSION OF THE FIRST PASS            
*                                    THROUGH AFFILIATE'S 0B AND 0C              
*                                    ELEMENTS ...                               
*                                                                               
PAY260   CLI   SVLINST,C'L'          IF (DISPLAYING TOTALS BY BUYLINE)          
         BNE   PAY270                ...                                        
         BAS   RE,ACCTOTS            ACCUMULATE TOTAL GROSS, NET, GST           
         BAS   RE,ACCTPST            ACCUMULATE TOTAL PST                       
*                                                                               
**********************************************************************          
*                                                                               
PAY270   TM    SVTSTOPT,X'10'        TEST PAID OPTION                           
         BO    PAY140                                                           
         OC    SVTSTEST,SVTSTEST     TEST REQUESTED VIA NTEST                   
         BNZ   PAY140                                                           
         OC    SVTSTSTA,SVTSTSTA     OR VIA ST=                                 
         BZ    PAY140                                                           
*                                                                               
**********************************************************************          
*                                                                               
PAY280   TM    PASS,2               ON FIRST PASS ... AFTER ALL BUY-            
         BO    PAY370               LINES HAVE BEEN PROCESSED ...               
*                                                                               
         CLI   SVLINST,C'L'         IF (DISPLAYING TOTALS BY BUYLINE)           
         BNE   PAY320               ... MOVE EACH BUYLINE AND IT'S              
         USING BUYTABD,R5           TOTALS TO SCREEN                            
         LA    R5,BUYTAB                                                        
PAY290   LH    RF,LINCOUNT                                                      
         LA    RF,1(RF)                                                         
         STH   RF,LINCOUNT                                                      
         CLI   0(R5),X'FF'                                                      
         BE    PAY320                                                           
         CLI   BUYMARK,1                                                        
         BE    PAY310                                                           
PAY300   LA    R5,L'BUYTAB(R5)                                                  
         B     PAY290                                                           
*                                                                               
PAY310   MVC   BUYTOTG,BUYGRS        TO PREPARE FOR PAYTEST, GET TOTALS         
         MVC   BUYTOTN,BUYNET        FROM BUYLINE TABLE AND STORE IN            
         LLC   R0,BUYREC+9           BUYTOTG AND BUYTOTN...STORE EST            
         CVD   R0,DUB                AND BUYLINE # IN WORK.                     
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         MVI   WORK+3,C'-'                                                      
         CLC   LINCOUNT,SVSTALO                                                 
         BL    PAY300                                                           
         LH    R0,LINCOUNT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+4(3),DUB                                                    
         GOTO1 PAYTEST                                                          
         B     PAY300                                                           
*                                                                               
**********************************************************************          
*                                                                               
PAY320   MVI   ERRCD,ALLPAID        ON FIRST PASS ... AFTER ALL BUY-            
         TM    SVTSTOPT,X'10'       LINES HAVE BEEN PROCESSED ...               
         BZ    *+8                  TEST PAID OPTION ACTIVE                     
         MVI   ERRCD,ALLUNPD                                                    
         TM    PASS,X'40'           TEST SPOTS IN PERIOD                        
         BO    *+12                                                             
         LA    R2,PAYDTH                                                        
         B     PAYERR                                                           
*                                                                               
         CLI   SVUNPAY,C'Y'        ALLOW UNPAY TO GET THROUGH HERE              
         BE    PAY324                                                           
         CLI   SVPPROFB+3,C'Y'     ALLOW $0 CLEARANCES                          
         BE    PAY324              YES - THEN ALLOW THIS                        
         OC    TOTG,TOTG           TEST $0 CLEARANCE                            
         BNZ   PAY324                                                           
         OC    TOTGST,TOTGST       TEST NON-ZERO TAX                            
         BNZ   PAY322ER                                                         
         OC    TOTPST,TOTPST                                                    
         BNZ   PAY322ER                                                         
         B     PAY324                                                           
*                                                                               
PAY322ER MVC   NERRCD,=Y(PAY0ERR)                                               
         MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
*                                                                               
PAY324   L     R0,TOTG              DEPENDING ON SVPPROF, USE GROSS             
         CLI   SVPPROF+0,C'G'       OR NET ... ADD TOTAL GST AND                
         BE    *+8                  TOTAL PST                                   
         L     R0,TOTN                                                          
         A     R0,TOTGST                                                        
         LA    RE,TOTPST                                                        
         LA    R4,10                                                            
PAY330   A     R0,0(RE)                                                         
         LA    RE,4(RE)                                                         
         BCT   R4,PAY330                                                        
*                                                                               
         CLI   SVPPROFB+5,1        TEST PAY INVOICE AMOUNT                      
         BNH   PAY332              NO                                           
                                                                                
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
         B     PAY334              WITHIN PERCENTAGE TOLERANCE                  
*                                                                               
PAY332   C     R0,TOTAMTLO          IF (LESS THAN TOTALAMTLO) OR                
         BL    PAY360               (GREATER THAN TOTALAMTHI) ...               
         C     R0,TOTAMTHI          CALL PAYICL AND EXIT                        
         BH    PAY360                                                           
         CLI   SVPPROFB+5,1        TEST PAY INV AMT BUT CHECK $                 
         BL    PAY336                                                           
*                                                                               
PAY334   L     RE,TOTAMT                                                        
         SR    RE,R0                                                            
         ST    RE,FILEDIFF         SAVE DIFF BETWEEN FILE/INVOICES              
*                                                                               
PAY336   MVI   STATSEQ,0            IF IN RANGE ... RESET CLEARANCE             
         CLI   SVUNPAY,C'Y'         IF UNPAYING ....                            
         BE    PAY350               CONTINUE TO PASS 2                          
         TM    SVTSTOPT,X'10'       IF PAID OPTION ... CALL PAYICL              
         BO    PAY360               AND EXIT                                    
*                                                                               
         BRAS  RE,TSTLOCK           TEST DATA LOCKED                            
         BE    PAY340                                                           
         MVC   NERRCD,=AL2(DATALOCK)                                            
         MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
*                                                                               
PAY340   BAS   RE,UPDINV            MARK INVOICE PAID                           
         GOTO1 BLDSTAT              UPDATE CLEARANCE SEQ RECORD                 
*                                                                               
PAY350   MVI   PASS,2                                                           
         MVI   DMINBTS,X'80'        SET READ FOR UPDATE                         
         B     PAY120                                                           
*                                                                               
PAY360   GOTO1 PAYICL                                                           
         B     EXIT                  END OF PASS1                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
PAY370   CLI   SVUNPAY,C'Y'        TEST UNPAY REQUEST                           
         BNE   PAY380              NO                                           
         CLC   =C'REVCHECK',PAYER  TEST TO ISSUE UNPAY CHECK                    
         BNE   PAY390              NO                                           
*                                                                               
PAY380   GOTO1 PAYCHK               ON 2ND PASS, WRITE CHECK REQUESTS           
*                                                                               
**********************************************************************          
*                                                                               
PAY390   XC    KEY,KEY               AT CONCLUSION OF SECOND PASS ...           
         MVC   KEY(10),SVKEY         READ NETWORK RECORD FOR EACH               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                  BUYLINE IN BUYLINE TABLE INTO              
         B     PAY392                AREC2                                      
*                                                                               
PAY391   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PAY392   CLC   KEY(10),KEYSAVE                                                  
         BNE   PAY396                                                           
         CLI   KEY+10,0            IGNORE SPILL POINTERS                        
         BNE   PAY391                                                           
*                                                                               
         USING BUYTABD,R5                                                       
         SR    RF,RF                                                            
         ICM   RF,3,KEY+11                                                      
         LA    R5,BUYTAB                                                        
         BCTR  RF,0                                                             
         MHI   RF,L'BUYTAB                                                      
         AR    R5,RF                                                            
         CLI   BUYMARK,1                                                        
         BNE   PAY391                                                           
         L     R2,AREC2                                                         
         ST    R2,AREC                                                          
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,SETPST                                                        
*                                                                               
         USING REGELEM,R6            READ 0B AND 0C ELEMENTS FOR NET-           
         LA    R6,BDELEM             WORK BUYLINE                               
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         XC    ELEMDT,ELEMDT                                                    
*                                                                               
PAY393   BAS   RE,NEXTEL                                                        
         BNE   PAY395                                                           
         CLI   RLEN,10                                                          
         BNH   PAY393                                                           
*                                                                               
         LLC   R0,ELEMNO             COUNT SPOT NUMBER WITHIN                   
         CLC   ELEMDT,2(R6)                                                     
         BE    *+12                                                             
         MVC   ELEMDT,2(R6)                                                     
         SR    R0,R0                                                            
         TM    6(R6),X'80'                                                      
         BO    *+8                                                              
         AHI   R0,1                                                             
         STC   R0,ELEMNO                                                        
*                                                                               
         CLC   RDATE,SVENDP        TEST AFTER PAY PERIOD END                    
         BH    PAY393                                                           
         CLC   RDATE,SVSTARTP      TEST AFTER PAY PERIOD START                  
         BNL   PAY394              YES - PAY IT                                 
                                                                                
* IF PRIOR TO START DATE, CHECK OWR PAY TABLE AND PAY IF IT'S THERE             
                                                                                
         LA    R1,OWRTAB                                                        
         USING OWRTABD,R1                                                       
PAY393A  CLC   OWRLINE,KEY+11                                                   
         BNE   PAY393B                                                          
         CLC   OWRDATE,ELEMDT                                                   
         BNE   PAY393B                                                          
         CLC   OWRSPNUM,ELEMNO                                                  
         BE    PAY394                                                           
*                                                                               
PAY393B  LA    R1,L'OWRTAB(R1)                                                  
         OC    0(L'OWRTAB,R1),0(R1)                                             
         BNZ   PAY393A                                                          
         B     PAY393                                                           
*                                                                               
PAY394   BAS   RE,TESTPD                                                        
         BNZ   PAY393                IF COMPRESSED BUY DATE IS IN               
         MVC   12(1,R6),STATSEQ      SET CLEARANCE SEQUENCE NUMBER ...          
         CLI   SVUNPAY,C'Y'          AND IF NOT UNPAYING ... MOVE PAY           
         BE    PAY393                DATE INTO ELEMENT ... GET NEXT             
         MVC   RPAY,TODAYP           0B OR 0C ELEMENT                           
         B     PAY393                                                           
*                                                                               
PAY395   MVI   RDUPDATE,C'Y'         AFTER ALL 0B AND 0C ELEMENTS PRO-          
         GOTO1 PUTREC                AFTER ALL 0B AND 0C ELEMENTS PRO-          
         B     PAY391                CESSED PUT RECORD                          
*                                                                               
**********************************************************************          
*                                                                               
PAY396   CLI   SVUNPAY,C'Y'                                                     
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
NEXTX    BR    RE                                                               
PAYERR   GOTO1 ERROR                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*====================================================================*          
*        ACCUMULATE PST TOTALS/BUY                                              
*====================================================================*          
         SPACE 1                                                                
ACCPST   NTR1                                                                   
         LA    R6,BUYTPST                                                       
         LA    R1,XPSTPROV                                                      
         LA    R2,PSTCODE                                                       
         LA    R4,10               COUNTER                                      
*                                                                               
ACCPST10 CLI   2(R1),C' '          IF NO PST CODE                               
         BNH   ACCPST20            SKIP                                         
         L     R0,0(R6)            ADD IN AMOUNT FOR PROVINCE                   
         A     R0,8(R1)                                                         
         ST    R0,0(R6)                                                         
         MVC   0(1,R2),2(R1)       SET CODE FOR PROVINCE                        
*                                                                               
ACCPST20 LA    R2,1(R2)            BUMP                                         
         LA    R6,4(R6)                                                         
         LA    R1,XPSTLEN(R1)                                                   
         BCT   R4,ACCPST10                                                      
         XIT1                                                                   
         EJECT                                                                  
*====================================================================*          
*        ACCUMULATE GROSS AND NET TOTALS                                        
*====================================================================*          
         SPACE 1                                                                
ACCTOTS  NTR1                                                                   
         L     R0,BUYTOTG                                                       
         A     R0,TOTG               ADD CURRENT BUYLINE'S GROSS, NET,          
         ST    R0,TOTG               AND GST TO TOTAL GROSS, TOTAL NET          
         L     R0,BUYTOTN            AND TOTAL GST FOR ALL BUYLINES             
         A     R0,TOTN                                                          
         ST    R0,TOTN                                                          
         L     R0,BUYTGST                                                       
         A     R0,TOTGST                                                        
         ST    R0,TOTGST                                                        
         C     R0,=F'10000000'       (TOTAL GST CANNOT EXCEED 99999.99)         
         BL    ACCTOTSX                                                         
         B     ACCTOTSX   <<<<NOT AN ERROR ANY MORE>>>>>                        
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(GST2MUCH)                                            
         B     PAYERR                                                           
ACCTOTSX XIT1                                                                   
         EJECT                                                                  
*====================================================================*          
*        ACCUMULATE PST TOTALS/BUY                                              
*====================================================================*          
         SPACE 1                                                                
ACCTPST  NTR1                                                                   
         LA    R6,BUYTPST                                                       
         LA    R1,TOTPST                                                        
         LA    R4,10                 COUNTER                                    
*                                                                               
ATPST10  L     R0,0(R1)                                                         
         A     R0,0(R6)              ADD IN AMOUNT FOR PROVINCE                 
         ST    R0,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R6,4(R6)                                                         
         BCT   R4,ATPST10                                                       
         XIT1                                                                   
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
         BZ    TESTPD4             YES - IGNORE DATE                            
         OC    4(2,R6),4(R6)       TEST PAID                                    
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
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* FOR CLEARANCE MOS JUL/10 AND SUBSEQUENT                                       
* OVERRIDE LOCAL STATION PST ELEMENT WITH VALUES FROM NETWORK                   
* RECORD ON STATION MASTER FILE                                                 
*=============================================================                  
                                                                                
SETPST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,SVSTART,(3,DUB)                                     
         CLC   DUB(3),=X'6E061C'   TEST PER STARTS AFTER JUN27/10               
         BL    SETPSTX                                                          
                                                                                
         ICM   RF,3,ELCDLO              SAVE ELCODES                            
*                                                                               
         MVC   SVPSTEL+2(10),SVNETPST    MOVE NETWORK PST VALUES                
         OC    SVOVRPST(10),SVOVRPST     TEST ANY PST OVRDS ENTERED             
         BZ    *+10                                                             
         MVC   SVPSTEL+2(10),SVOVRPST    THEY WIN OVER NETWORK PST              
                                                                                
* SEE IF ANY PAID SPOTS THIS BUYLINE                                            
                                                                                
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         MVI   BYTE,C'N'           SET NO PAID SPOTS FLAG                       
*                                                                               
SETPST2  BRAS  RE,NEXTEL                                                        
         BNE   SETPST4                                                          
         OC    4(2,R6),4(R6)                                                    
         BZ    SETPST2                                                          
         MVI   BYTE,C'Y'           SET PAID SPOTS FOUND                         
*                                                                               
SETPST4  MVI   ELCDLO,X'6B'        SEARCH FOR PST ELEMENT                       
         MVI   ELCDHI,X'6B'                                                     
*                                                                               
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    SETPST10                                                         
*                                                                               
         CLI   BYTE,C'Y'           TEST PAID SPOTS FOUND                        
         BE    SETPSTER                                                         
*                                                                               
         OC    SVPSTEL,SVPSTEL     BUT IF THERE IS NO PST                       
         BZ    SETPSTX             THEN EXIT                                    
*                                                                               
         MVI   ELEM,X'6B'          BUILD NEW PST ELEMENT                        
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(10),SVPSTEL+2                                             
         GOTO1 VRECUP,DMCB,(C'S',BUYREC),ELEM,(R6)                              
         B     SETPSTX                                                          
                                                                                
* PST ELEMENT FOUND IN BUY                                                      
                                                                                
SETPST10 CLI   BYTE,C'Y'           TEST PAID SPOTS FOUND                        
         BNE   SETPST12                                                         
         CLC   2(10,R6),SVNETPST   YES - PST VALUES CAN'T CHANGE                
         BNE   SETPSTER                                                         
*                                                                               
SETPST12 MVC   2(10,R6),SVPSTEL+2  OVERRIDE LOCAL PST VALUES                    
*                                                                               
SETPSTX  STCM  RF,3,ELCDLO         RESTORE ELCODES                              
         XIT1                                                                   
*                                                                               
SETPSTER MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADPSTPD)                                              
         GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*====================================================================*          
*        UPDATE INVOICE - MARK PAID                                             
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
**       MVI   ERRAREA,X'FF'                                                    
**       GOTO1 ERROR                                                            
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
*====================================================================*          
*        SAVED STORAGE DSECT                                                    
*====================================================================*          
         SPACE 1                                                                
WORKD    DSECT                                                                  
STANUM   DS    XL4                                                              
FIRSTBUY DS    XL4                                                              
BUYFOUND DS    XL1                                                              
SPOTSTAT DS    CL1                                                              
LINCOUNT DS    H                                                                
*                                                                               
         DS    0D                                                               
DFRTAB   DS    100XL5                                                           
         DS    XL1                                                              
DFRTABX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
OWRTAB   DS    100XL5                                                           
         DS    XL1                                                              
OWRTABX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
STATAB   DS    CL300                                                            
         DS    XL5                 SPARE FOR E-O-L                              
STATABX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
BUYTAB   DS    500XL9                                                           
BUYTABX  DS    X                                                                
WORKDX   EQU   *                                                                
*                                                                               
BUYTABD  DSECT                                                                  
BUYMARK  DS    CL1                                                              
BUYGRS   DS    XL4                                                              
BUYNET   DS    XL4                                                              
*                                                                               
DFRTABD  DSECT                                                                  
DFRLINE  DS    XL2                                                              
DFRDATE  DS    CL2                                                              
DFRSPNUM DS    XL1                                                              
DFRLNQ   EQU   *-DFRTABD                                                        
*                                                                               
OWRTABD  DSECT                                                                  
OWRLINE  DS    XL2                                                              
OWRDATE  DS    CL2                                                              
OWRSPNUM DS    XL1                                                              
OWRLNQ   EQU   *-OWRTABD                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPPAYWORK                                                      
         EJECT                                                                  
*                                                                               
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
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083SPPAY15   10/23/14'                                      
         END                                                                    
