*          DATA SET PPREPBC02  AT LEVEL 022 AS OF 07/17/18                      
*PHASE PPBC02A                                                                  
*INCLUDE OFFOUT                                                                 
*INCLUDE PPBVAL                                                                 
         SPACE 1                                                                
*********************************************************************           
* CHANGE LOG                                                        *           
*                                                                               
*  SMUR SPEC-17729  NEW MEDIA D FOR DIGITAL MEDIA (18.3)                        
*                                                                               
*  BPLA   06/15  CHANGES FOR NEW MEDIA CODES B, V, W                            
*                                                                               
*  BPLA   11/13  CHANGE TO SUPPORT 2 CHAR SYSTEMS                               
*                                                                               
*  BPLA   2/10   SORT AND DISPLAY OFFICE IF $* IS IN QCLIENT                    
*                                                                               
*  BPLA   8/04   IGNORE BILLS FOR AGENCY UB AND CLIENT AFX                      
*                IF PRODUCED BEFORE AUG01/04                                    
*                 - ADSERVE FIX DATA                                            
*                                                                   *           
*********************************************************************           
         TITLE 'PP0BC-02  PRINT ACROSS AGENCY BILLING REPORT'                   
*                                                                               
*        QOPT2   C= CLOSE-OUT RUN                                               
*                QOPT5 SHOULD BE SE TO Y TO INCLUDE ALL BILLS                   
*                WITH THIS OPTION                                               
*        QOPT3   T= INCLUDE TEST AGENCIES                                       
*        QOPT4   X= EXCULDE CANADIAN AGENCIES                                   
*                C= CANADIAN ONLY                                               
*                T= TEST ONLY                                                   
*                B= EITHER CANADIAN OR TEST ONLY                                
*        QOPT5   Y= DO ALL PRIOR INSTEAD OF Y-T-D                               
*                                                                               
         EJECT                                                                  
PPBC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPBC02                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING PPBCWRKD,RC                                                      
         L     R9,PPFILEC                                                       
         USING PPFILED,R9                                                       
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BE    FIRST                                                            
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
         CLI   MODE,FBUYCLI      FIRST BILL FOR CLIENT                          
         BE    CFIRST                                                           
         CLI   MODE,PROCBIL                                                     
         BE    DOBILL                                                           
         CLI   MODE,FBUYREQ                                                     
         BNE   EXIT                                                             
*                                                                               
         MVI   FCRDCLOS,C'N'                                                    
         CLI   QOPT2,C'C'        CLOSE-OUT RUN?                                 
         BNE   *+8                                                              
         MVI   FCRDCLOS,C'Y'                                                    
*                                                                               
         BAS   RE,PROCAGYM                                                      
         B     EXIT                                                             
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
FIRST    DS    0H                                                               
*                                                                               
*        GET A(OFFICER)                                                         
*                                                                               
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVC   DUB,SPACES          GET OFFICER                                  
         MVC   DUB(6),=C'T00A38'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         MVC   VOFFICER,4(R1)      SAVE ADDRESS                                 
*                                                                               
*                                  INITIALIZATION                               
INIT     CLI   FRSTIM,0                                                         
         BNE   INIT2                                                            
         MVI   FRSTIM,1                                                         
*                                  RELOCATE ACONS                               
         RELOC (R4)                                                             
         LA    R0,(ACONX-ACONS)/4                                               
         LA    R1,ACONS                                                         
         LA    R2,VBTOTS                                                        
INIT0    DS    0H                                                               
         L     R3,0(R1)                                                         
         AR    R3,R4                                                            
         ST    R3,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,INIT0                                                         
         MVC   TODAY+0(2),RCDATE+6      YR                                      
         MVC   TODAY+2(2),RCDATE+0      MO                                      
         MVC   TODAY+4(2),RCDATE+3      DA                                      
*                                                                               
         GOTO1 DATCON,DMCB,TODAY,(3,BTODAY)                                     
         GOTO1 DATCON,DMCB,(3,BTODAY),(0,TODAY)                                 
         GOTO1 DATCON,DMCB,(0,TODAY),(9,CURMTH)                                 
*                                                                               
*                                                                               
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         SPACE 1                                                                
INIT2    DS    0H                  **DONE FOR EACH REQUEST**                    
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'                                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACES                                                         
         MVC   BQSTART(6),=X'000000FFFFFF'    PASS ALL BILLS                    
*                                                                               
         L     RE,AAGYTAB                                                       
         ST    RE,ANXTAM                                                        
*                                                                               
INITX    DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
CFIRST   DS    0H                                                               
         MVC   SAVQCLT,QCLIENT        SAVE REQUESTED QCLIENT                    
         CLC   QCLIENT(2),=C'$*'      ALL OFFICES                               
         BNE   EXIT                                                             
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         MVC   SAVCOFF(1),PCLTOFF    SAVE OFFICE FOR HEADLINES                  
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'P'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,QAGENCY                                              
         MVC   OFFD.OFCPMED,QMEDIA                                              
         MVC   OFFD.OFCOFC,PCLTOFF                                              
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK),(0,VCOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   BCCLTF0                                                          
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
BCCLTF0  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
LAST     DS    0H                                                               
         L     R5,ANXTAM      SET DDS ENTRY IN AGYTAB                           
         MVC   0(3,R5),=C'ZZB'          MOBILE                                  
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZD'          DIGITAL AUDIO                           
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZI'          INTERACTIVE                             
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZL'          SOCIAL                                  
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZM'           MAGAZINES                              
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZN'           NEWSPAPERS                             
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZO'           OUT-OF-HOME                            
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZS'           SUPPLEMENTS                            
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZT'            TRADE                                 
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZV'            NATIONAL VIDEO                        
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZW'            LOCAL VIDEO                           
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(3,R5),=C'ZZZ'            ALL MEDIA                             
         MVC   4(6,R5),=C'PRINT '                                               
         MVC   10(2,R5),SYSNAID                                                 
         OC    2(33,R5),SPACES                                                  
         LA    R5,39(R5)                                                        
         MVC   0(5,R5),=5X'FF'         SET END OF TABLE                         
         MVI   TOTTYP,X'01'        AGENCY TOTALS                                
         GOTO1 VBTOTS,DMCB,(RA)                                                 
         MVI   TOTTYP,X'02'        FILE TOTALS                                  
         MVI   FORCEHED,C'Y'       JUST IN CASE                                 
         GOTO1 VBTOTS,DMCB,(RA)                                                 
         MVI   MODE,RUNLAST                                                     
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
*                    PROCESS AN AGY/MED (HERE FOR EACH REQ)                     
PROCAGYM NTR1                                                                   
*                                                                               
         MVC   SVQOPT5,QOPT5       SAVE FOR REPORT                              
         MVC   SVQOPT4,QOPT4       SAVE FOR REPORT                              
         MVC   SVQOPT2,QOPT2       SAVE FOR REPORT                              
*                                                                               
         CLC   PAGYKAGY,=C'ZZ'     DON'T ADD THESE TO TABLE                     
         BE    EXIT                                                             
         CLI   QOPT3,C'T'          SEE IF INCLUDING TEST AGYS                   
         BE    PROCA2                                                           
         TM    PAGYSTAT,X'01'                                                   
         BO    SKIPAGY                                                          
*                                                                               
PROCA2   CLI   QOPT4,C'T'        TEST AGENCIES ONLY?                            
         BNE   PROCA5                                                           
         TM    PAGYSTAT,X'01'                                                   
         BNO   SKIPAGY                                                          
         B     PROCA8                                                           
*                                                                               
PROCA5   CLI   QOPT4,C'X'        SEE IF EXCLUDING CANADIAN AGYS                 
         BNE   PROCA6                                                           
         CLI   PAGYNAT,C'C'                                                     
         BE    SKIPAGY                                                          
         B     PROCA8                                                           
*                                                                               
PROCA6   CLI   QOPT4,C'C'       CANADIAN ONLY?                                  
         BNE   PROCA7                                                           
         CLI   PAGYNAT,C'C'                                                     
         BNE   SKIPAGY                                                          
         B     PROCA8                                                           
*                                                                               
PROCA7   CLI   QOPT4,C'B'     REPORT IF EITHER CANADIAN OR TEST                 
         BNE   PROCA8                                                           
         CLI   PAGYNAT,C'C'                                                     
         BE    PROCA8                                                           
         TM    PAGYSTAT,X'01'                                                   
         BO    PROCA8                                                           
         B     SKIPAGY                                                          
*                                                                               
PROCA8   BAS   RE,ADDAGY           ADD TO AGYTAB                                
*                                                                               
         B     EXIT                                                             
*                                                                               
SKIPAGY  MVI   MODE,LBUYREQ                                                     
         B     EXIT                                                             
*                                                                               
         SPACE                                                                  
         EJECT                                                                  
DOBILL   DS    0H                  PROCESS A BILL                               
         CLC   PBILKAGY,=C'UB'     SEE IF CARAT                                 
         BNE   DOBL10                                                           
         CLC   PBILKCLT,=C'AFX'    ADSERVE FIX DATA                             
         BNE   DOBL10                                                           
         CLC   PBILLDAT,=X'FAF4F0F8F0F1'    SEE IF BEFORE AUG01/04              
         BL    EXIT                IGNORE                                       
*                                                                               
DOBL10   CLI   QOPT2,C'C'          TEST CLOSE OUT RUN                           
         BNE   DOBL11              NO                                           
*                                                                               
         TM    KEY+25,X'C0'        MUST CHECK KEY-NOT RECORD                    
         BO    DOBL12                                                           
         B     EXIT                                                             
*                                                                               
******   TM    PBILLCTL,X'C0'       YES- MUST BE CLOSED OUT                     
******   BO    DOBL12                                                           
******   B     EXIT                                                             
*                                                                               
DOBL11   DS    0H                                                               
         TM    PBILLCTL,X'80'      SKIP DELETES                                 
         BNZ   EXIT                                                             
*                                                                               
DOBL12   DS    0H                                                               
*                                                                               
DOBL13   DS    0H                                                               
         MVI   CLTACT,C'Y'                                                      
         CLI   PBRETAIL,X'41'       SKIP CORP SUMMARY BILLS                     
         BE    EXIT                                                             
*                                                                               
DOBL15   BAS   RE,ADDUP                                                         
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
         EJECT                                                                  
ADDUP    NTR1                                                                   
         SPACE 1                                                                
*                                                                               
* BILL RECORDS *                                                                
         SPACE                                                                  
*                                                                               
         CLC   PBILLDAT(6),TODAY   SEE IF AFTER TODAY                           
         BH    ADDUPX              SKIP                                         
*                                                                               
         CLI   QOPT5,C'Y'        SEE IF SKIPPING CURRENT YEAR CHK               
         BE    ADDUP3                                                           
*                                                                               
         CLC   PBILLDAT(2),TODAY   SEE IF IN THIS YESR                          
         BL    ADDUPX              SKIP                                         
*                                                                               
ADDUP3   GOTO1 VPPBVAL,DMCB,(C'B',PBILLREC),PPBVALD                             
         ZAP   WGRS,PPBVEBG                                                     
*                                                                               
         SPACE                                                                  
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'01'                                                     
         MVC   BUFAM,PBILKAGY     AGY/MED                                       
         CLC   QCLIENT(2),=C'$*'   ALL OFFICE REQUEST                           
         BNE   *+10                                                             
         MVC   BUFOFF,SAVCOFF                                                   
*                                                                               
         MVC   BUFCLT,PBILKCLT                                                  
         ZAP   BUFYTD,WGRS         YTD GROSS                                    
         ZAP   BUFCM,=P'0'         CLEAR CURRENT MONTH GROSS                    
         ZAP   BUFYTDM,=P'0'       CLEAR YTD MANUAL                             
         ZAP   BUFCMM,=P'0'        CLEAR CURRENT MONTH MANUAL                   
         CLC   PBILLDAT(4),TODAY   SEE IF CURRENT MONTH                         
         BNE   *+10                                                             
         ZAP   BUFCM,WGRS                                                       
         CLI   PBILLTYP,C'M'       SEE IF A MANUAL BILL                         
         BNE   ADDUP5                                                           
         ZAP   BUFYTDM,WGRS         YTD MANUAL                                  
         CLC   PBILLDAT(4),TODAY   SEE IF CURRENT MONTH                         
         BNE   *+10                                                             
         ZAP   BUFCMM,WGRS          CURRENT MONTH MANUAL                        
ADDUP5   GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFCLT,=X'FFFFFF'    FOR AGY/MED/OFF TOTALS                      
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         CLC   QCLIENT(2),=C'$*'    SEE IF ALL OFFICE REQUEST                   
         BNE   ADDUP10                                                          
         MVC   BUFOFF,=X'FFFF'      FOR AGY/MED TOTALS                          
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
ADDUP10  MVI   BUFAM+2,C'Z'         FOR AGY TOTALS                              
         XC    BUFOFF,BUFOFF        CLEAR OFFICE                                
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*    FILE TOTALS                                                                
         MVI   BUFTYP,X'02'                                                     
         MVC   BUFAM(3),=C'ZZ'                                                  
         MVC   BUFAM+2(1),PBILKMED                                              
         XC    BUFOFF,BUFOFF        CLEAR OFFICE                                
         MVC   BUFCLT,=X'FFFFFF'    FOR AGY/MED TOTALS                          
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFAM+2,C'Z'         FOR AGY TOTALS                              
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         B     ADDUPX                                                           
ADDUPX   EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
********************************************************************            
* SUBROUTINE TO BUILD LIST OF AGENCIES                                          
********************************************************************            
         SPACE                                                                  
ADDAGY   NTR1                                                                   
         L     R5,ANXTAM                                                        
         MVC   0(3,R5),PAGYKAGY    AGY/MEDIA                                    
         MVC   4(33,R5),PAGYNAME                                                
         MVC   37(1,R5),PAGYSTAT   SAVE STATUS                                  
         MVC   38(1,R5),PAGYNAT    AND NATIONALITY                              
         LA    R5,39(R5)                                                        
         ST    R5,ANXTAM                                                        
         XIT1                                                                   
         EJECT                                                                  
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
*                                                                               
ACONS    DS    0F                                                               
         DC    A(BTOTALS)                                                       
         DC    A(PRINTIT)                                                       
         DC    V(PPBVAL)                                                        
         DC    A(BUFFALOC)                                                      
         DC    A(AGYTAB)                                                        
ACONX    EQU   *                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
BTOTALS  CSECT                                                                  
         NMOD1 0,BTOTALS                                                        
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING PPWORKD,RA,R9                                                    
         LA    R8,BTOTALS+4095                                                  
         LA    R8,1(R8)                                                         
         USING BTOTALS+4096,R8                                                  
*                                                                               
         LA    RC,SPACEND                                                       
         USING PPBCWRKD,RC                                                      
         XC    BUFREC,BUFREC                                                    
         MVC   BUFTYP,TOTTYP                                                    
*                                                                               
BTOT2    DS    0H                                                               
         XC    LASTAM,LASTAM                                                    
         MVI   AGYPSW,0                                                         
         MVI   MEDPSW,0                                                         
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     BTOT10                                                           
BTOT5    GOTO1 BUFFALO,DMCB,=C'SEQ',(TOTTYP,BUFFBUFF),BUFREC,0                  
*                                                                               
BTOT10   CLI   DMCB+8,X'80'        END                                          
         BE    BTOTX                                                            
BTOT20   DS    0H                                                               
         CLC   LASTAM,BUFAM        SEE IF FIRST/NEW AGY/MED                     
         BE    BTOT20C                                                          
*                                                                               
         CLC   BUFAM(2),=C'ZZ'   FILE TOTAL?                                    
         BNE   BTOT20A                                                          
         CLI   BUFAM+2,C'I'        MEDIA I (FIRST MEDIA)                        
         BH    BTOT20A4            IF NOT- SKIP AGY NAME                        
*                                                                               
BTOT20A  MVI   FORCEHED,C'Y'                                                    
         CLI   BUFAM+2,C'Z'       AGENCY TOTAL?                                 
         BNE   BTOT20A2                                                         
*                                 CAN'T LOOK FOR AGENCY NAME                    
         MVI   FORCEHED,C'N'      NO NEW PAGE                                   
         B     BTOT20A7                                                         
*                                                                               
BTOT20A2 BAS   RE,BTAGY           FIND AGENCY NAME                              
         MVC   P(36),WORK         RETURNED IN WORK                              
         MVC   PSECOND(15),WORK+45                                              
         MVC   SVNAGY,P           SAVE AGENCY NAME FOR NEW PAGE                 
         MVC   SVTCAGY,PSECOND    SAVE AGENCY TEST/CANADIAN                     
         GOTO1 VPRINTIT,DMCB,(RA)                                               
         MVI   AGYPSW,1           SET AGY NAME PRINTED                          
*                                                                               
BTOT20A4 CLI   BUFAM+2,C'Z'       SEE IF AGENCY TOTAL                           
         BE    BTOT20A7                                                         
         BAS   RE,BTAGY           FIND MEDIA CODE                               
         MVC   P(11),=CL11'MOBILE'                                              
         CLI   WORK+40,C'B'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=CL11'DIG. AUDIO'                                          
         CLI   WORK+40,C'D'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=C'INTERACTIVE'                                            
         CLI   WORK+40,C'I'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=CL11'SOCIAL'                                              
         CLI   WORK+40,C'L'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=CL11'MAGAZINES'                                           
         CLI   WORK+40,C'M'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=CL11'NEWSPAPERS'                                          
         CLI   WORK+40,C'N'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=CL11'OUT-OF-HOME'                                         
         CLI   WORK+40,C'O'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=CL11'SUPPLEMENTS'                                         
         CLI   WORK+40,C'S'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=CL11'TRADE'                                               
         CLI   WORK+40,C'T'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=CL11'NAT. VIDEO'                                          
         CLI   WORK+40,C'V'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=CL11'LOC. VIDEO'                                          
         CLI   WORK+40,C'W'                                                     
         BE    BTOT20A5                                                         
         DC    H'0'          UNKNOWN MEDIA                                      
BTOT20A5 DS    0H                                                               
         MVC   SVNMED,P              SAVE MEDIA FOR NEW PAGE                    
         MVI   SPACING,2             SKIP A LINE                                
         GOTO1 VPRINTIT,DMCB,(RA)                                               
         MVC   P,SAVEP                                                          
         MVC   P(6),SPACES        P LINE GETS PRINTED LATER                     
BTOT20A7 MVC   LASTAM,BUFAM                                                     
         MVI   MEDPSW,1                                                         
         B     BTOT20F                                                          
*                                                                               
BTOT20B  MVC   LASTAM,BUFAM                                                     
*                                                                               
BTOT20C  DS    0H                                                               
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   BTOT20F                                                          
         MVC   SAVEP,P                                                          
         MVC   P,SPACES                                                         
         MVC   P(L'SVNAGY),SVNAGY                                               
         MVC   PSECOND(L'SVTCAGY),SVTCAGY                                       
         GOTO1 VPRINTIT,DMCB,(RA)  PRINT THE AGENCY STUFF                       
*                                                                               
         MVC   P(L'SVNMED),SVNMED                                               
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RA)  PRINT THE MEDIA DESCRIPTION                  
         MVC   P,SAVEP             RESTORE THE PRINTLINE                        
*                                                                               
BTOT20F  DS    0H                                                               
         CLC   SAVQCLT(2),=C'$*'    ALL OFFICE REQUEST                          
         BNE   BTOT20G                                                          
         CLC   BUFOFF,=X'FFFF'                                                  
         BNE   BTOT20F3                                                         
         MVC   P(3),=C'ALL'                                                     
         B     BTOT20F5                                                         
*                                                                               
BTOT20F3 MVC   P(2),BUFOFF                                                      
*                                                                               
BTOT20F5 JIF   BUFCLT,NE,=3X'FF',BTOT20F8,JUMP=N                                
         MVC   P+3(3),=C'ALL'                                                   
         B     BTOT20FX                                                         
*                                                                               
BTOT20F8 MVC   P+3(3),BUFCLT                                                    
*                                                                               
BTOT20FX DS    0H                                                               
         B     BTOT20I                                                          
*                                                                               
BTOT20G  JIF   BUFCLT,NE,=3X'FF',BTOT20H,JUMP=N                                 
         MVC   P(3),=C'ALL'                                                     
         B     BTOT20I                                                          
*                                                                               
BTOT20H  MVC   P(3),BUFCLT                                                      
*                                                                               
BTOT20I  DS    0H                                                               
         ZAP   DUB,BUFYTD                                                       
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+20(18),WORK+1                                                  
         ZAP   DUB,BUFCM                                                        
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+40(18),WORK+1                                                  
         ZAP   DUB,BUFYTDM                                                      
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+59(18),WORK+1                                                  
         ZAP   DUB,BUFCMM                                                       
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+78(18),WORK+1                                                  
         MVI   TOTSW,0                                                          
         CLC   BUFCLT,=3X'FF'     AGY/MED TOTAL                                 
         BNE   BTOT24                                                           
         MVI   AGYPSW,0         SO AGENCY NAME WILL REPRINT                     
         MVI   TOTSW,1                                                          
         CLC   SAVQCLT(2),=C'$*'     ALL OFFICE REQUEST?                        
         BNE   BTOT23                                                           
         CLC   BUFOFF,=X'FFFF'                                                  
         BE    BTOT23                                                           
         OC    BUFOFF,BUFOFF                                                    
         BZ    BTOT23                                                           
         MVC   P(12),=C'OFFICE TOTAL'                                           
         B     BTOT23B                                                          
                                                                                
BTOT23   MVC   P(11),=C'MEDIA TOTAL'                                            
****ED   MVI   P+34,C'*'                                                        
****ED   MVI   P+54,C'*'                                                        
BTOT23B  CLI   BUFAM+2,C'Z'       AGENCY TOTAL                                  
         BNE   BTOT24                                                           
         MVC   P(12),=C'AGENCY TOTAL'                                           
****ED   MVI   P+35,C'*'                                                        
***ED    MVI   P+55,C'*'                                                        
         CLC   BUFAM,=C'ZZZ'      DDS TOTAL                                     
         BNE   BTOT24                                                           
         MVC   P(12),=C'FILE TOTAL  '                                           
****ED   MVI   P+36,C'*'                                                        
****ED   MVI   P+56,C'*'                                                        
*                                                                               
BTOT24   GOTO1 VPRINTIT,DMCB,(RA)                                               
         CLI   TOTSW,1                                                          
         BNE   BTOT5                                                            
         MVC   SVMAXL,MAXLINES                                                  
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT,DMCB,(RA)     SKIP A LINE                               
         MVC   MAXLINES,SVMAXL                                                  
         B     BTOT5                                                            
*                                                                               
BTOTX    XIT1                                                                   
*                                                                               
BTAGY    NTR1                                                                   
         MVC   WORK(60),SPACES                                                  
         L     R1,AAGYTAB                                                       
BTAG2    CLC   0(5,R1),=5X'FF'   END OF TABLE                                   
         BNE   *+6                                                              
         DC    H'0'            SOMETHING SCREWY                                 
*                                                                               
         CLC   BUFAM,0(R1)                                                      
         BE    BTAG5                                                            
         LA    R1,39(R1)     NEXT ENTRY                                         
         B     BTAG2                                                            
*                                                                               
BTAG5    MVC   WORK(2),0(R1)   AGY CODE                                         
         MVC   WORK+3(33),4(R1)  NAME                                           
         CLC   WORK(2),=C'ZZ'    SEE IF DDS TOTALS                              
         BNE   BTAG8                                                            
         MVC   WORK(39),SPACES                                                  
         MVC   WORK(33),4(R1)    JUST MOVE NAME                                 
BTAG8    MVC   WORK+40(1),2(R1)  RETURN MEDIA CODE                              
         LA    R3,WORK+45                                                       
         MVC   WORK+45(4),=C'TEST'                                              
         TM    37(R1),X'01'                                                     
         BNO   BTAG10                                                           
         LA    R3,6(R3)                                                         
*                                                                               
BTAG10   MVC   0(4,R3),SPACES                                                   
         CLI   38(R1),C'C'                                                      
         BNE   BTAG12                                                           
         MVC   0(8,R3),=C'CANADIAN'                                             
*                                                                               
BTAG12   DS    0H                                                               
         XIT1                                                                   
                                                                                
         LTORG                                                                  
*                             PRINT ROUTINE                                     
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING PPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING PPBCWRKD,RC                                                      
*                                                                               
         MVC   QUESTOR(5),=C'PRINT'                                             
         MVC   QUESTOR+6(2),SYSNAID                                             
*                                                                               
         CLI   SVQOPT2,C'C'     CLOSE-OUT RUN?                                  
         BNE   PRNT1                                                            
         MVC   H4+56(17),=C'CLOSED-OUT $ ONLY'                                  
*                                                                               
PRNT1    MVC   H3+54(15),=C'CURRENT MONTH -'                                    
         MVC   H3+70(6),CURMTH                                                  
*                                                                               
         CLI   SVQOPT4,C'C' CANADIAN AGYS ONLY?                                 
         BNE   PRNT2                                                            
         MVC   H4+1(28),=C'** CANADIAN AGENCIES ONLY **'                        
         B     PRNT5                                                            
*                                                                               
PRNT2    CLI   SVQOPT4,C'T' TEST AGENCIES ONLY?                                 
         BNE   PRNT3                                                            
         MVC   H4+1(24),=C'** TEST AGENCIES ONLY **'                            
         B     PRNT5                                                            
*                                                                               
PRNT3    CLI   SVQOPT4,C'B' CANADIAN OR TEST ONLY                               
         BNE   PRNT5                                                            
         MVC   H4+1(37),=C'** TEST AND CANADAIN AGENCIES ONLY **'               
         B     PRNT5                                                            
*                                                                               
PRNT5    DS    0H                                                               
*                                                                               
         MVI   RCSUBPRG,10                                                      
*                                                                               
         CLI   SVQOPT5,C'Y'                                                     
         BNE   *+8                                                              
         MVI   RCSUBPRG,11                                                      
*                                                                               
         CLI   TOTTYP,X'01'                                                     
         BE    PRNTX                                                            
         MVI   RCSUBPRG,30                                                      
*                                                                               
         CLI   SVQOPT5,C'Y'                                                     
         BNE   *+8                                                              
         MVI   RCSUBPRG,31                                                      
*                                                                               
         CLI   TOTTYP,X'02'                                                     
         BE    PRNTX                                                            
*                                                                               
PRNTX    GOTO1 REPORT                                                           
*                                                                               
PRNTITX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                  WORK AREA DSECT                              
PPBCWRKD DSECT                                                                  
*                                                                               
TODAY    DS    CL6                                                              
CURMTH   DS    CL6           CURRENT MONTH (MMM/YY)                             
SVQOPT4  DS    CL1           SAVED FOR REPORT                                   
SVQOPT2  DS    CL1           SAVED FOR REPORT                                   
SVQOPT5  DS    CL1           SAVED FOR REPORT                                   
SAVCOFF  DS    CL1                                                              
SAVQCLT  DS    CL3                                                              
*                                                                               
X        DS    F                                                                
ANXTAM   DS    F                                                                
TOTTYP   DS    XL1                                                              
LASTAM   DS    CL3                                                              
TOTSW    DS    X                                                                
SVMAXL   DS    X                                                                
FRSTIM   DS    X                                                                
*                                  1 = NO CD                                    
CLTACT   DS    X                                                                
CURMED   DS    C                                                                
BTODAY   DS    XL3                                                              
ELCODE   DS    XL1                                                              
BUFFIO   DS    A                                                                
VOFFICER DS    A                                                                
         DS    F                                                                
VBTOTS   DS    A              ACONS                                             
VPRINTIT DS    A                                                                
VPPBVAL  DS    A                                                                
BUFFBUFF DS    A                                                                
AAGYTAB  DS    A                                                                
         DS    A             SPARE                                              
DDSSW    DS    CL1                                                              
*                                                                               
SAVEP    DS    CL132                                                            
SVNMED   DS    CL11                                                             
SVNAGY   DS    CL36                                                             
SVTCAGY  DS    CL15                                                             
AGYPSW   DS    CL1                                                              
MEDPSW   DS    CL1                                                              
*                                                                               
WGRS     DS    D                                                                
*                                  BUFFALO RECORD                               
         DS    0D                                                               
BUFREC   DS    0CL41                                                            
BUFKEY   DS    0CL9                                                             
BUFTYP   DS    CL1                 TYPE X'01'                                   
BUFAM    DS    CL3                 AGYMED                                       
BUFOFF   DS    CL2                 OFFICE                                       
BUFCLT   DS    CL3                 CLIENT                                       
*                                                                               
BUFYTD   DS    PL8                 YTD GROSS                                    
BUFCM    DS    PL8                 CURENT MTH GROSS                             
BUFYTDM  DS    PL8                 YTD MANUAL                                   
BUFCMM   DS    PL8                 CURENT MTH MANUAL                            
*                                                                               
       ++INCLUDE PPBVALD                                                        
         EJECT                                                                  
         SPACE 2                                                                
AGYTAB   CSECT                                                                  
         DS    1000XL39'00'       ROOM FOR 1000 AGYMEDS                         
AGYTABX  EQU   *                                                                
*                                  BYTE  1-2    AGY                             
*                                  BYTE  3      MEDIA                           
*                                  BYTE  4      NOT USED                        
*                                  BYTES 5-37   AGY NAME                        
*                                  BYTE  38     PAGYSTAT                        
*                                  BYTE  39     PAGYNAT                         
*                                                                               
*                                                                               
MAXAGYS  EQU   1000                                                             
*                                                                               
*                                                                               
         BUFF  LINES=9000,ROWS=1,COLUMNS=4,FLAVOR=PACKED,KEYLIST=(9,A)          
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         DS    0D                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022PPREPBC02 07/17/18'                                      
         END                                                                    
