*          DATA SET REREP7802  AT LEVEL 120 AS OF 04/16/03                      
*PHASE RE7802C,+0                                                               
*INCLUDE XSORT                                                                  
***********************************************************************         
* HISTORY LOST                                                                  
*                                                                               
* MAY11/92   (SKU) ADD OFFICE FILTER                                            
*                                                                               
* NOV10/92   (SKU) PRINT FAX NUMBER.  AUTOMATIC SORT BY OFFICE CODE             
*                                                                               
* SEP16/93   (BU ) DI/HN MERGER LISTING FOR REQUESTOR=$MERGER                   
*                                                                               
* NOV01/93   (BU ) DELETE REFERENCE TO DI FOR 16SEP93 CHANGES                   
*                                                                               
* DEC06/93   (BU ) ADD SALESPERSON LEAVE DATE TO REPORT.  MERGER HAS            
*               BEEN RUN, SO $MERGER CODE IS NO LONGER NEEDED, BUT              
*               HAS BEEN LEFT IN ANYWAY.                                        
*                                                                               
* DEC07/93   (BU ) ADD MANAGER FLAG TO REPORT.                                  
*                                                                               
* AUG18/93   (SM ) ADD OPTIONS TO RUN ALL OR SELECTED SEQUENCES                 
*                                                                               
* FEB26/97 (DBU)  ACTIVITY DATE FOR INDIVIDUAL RECORD MUST BE                   
*                 WITHIN REQUEST DATES FOR RECORD TO BE DISPLAYED               
*                                                                               
* FEB03/98 (ROB)  DOWNLOAD                                                      
*                                                                               
* FEB20/98 (JRD)  4K CONTRACTS                                                  
*                                                                               
* JUN15/98 (BU )  EXPAND TABLE FROM 2000 ENTRIES                                
*                                                                               
* JUN21/02 (HQ )  PRINT SALES EMAIL ADDRESS AND SALES ASSISTANT ADDRESS         
*                                                                               
* SEP26/02 (HQ )  SALES ASSISTANT INFO DOWNLOAD BUG FIX                         
*                                                                               
*                                                                               
*                                                                               
*               ***  END TOMBSTONE  ***                                         
***********************************************************************         
         TITLE 'SALESMAN LISTING PROGRAM'                                       
RE7802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE7802,RR=R5                                                 
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*              CHECK MODE SETTINGS                                              
         CLC   =C'$MERGER',QUESTOR SPECIAL REQUEST?                             
         BE    SALMERGE            YES                                          
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   SALE0020                                                         
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    SALE0015                                                         
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
****>>>> XC    CNT,CNT             CAN'T INITIALIZE CNT UNDER REQFRST           
*                                     MUST BE UNDER RUNFRST!!                   
SALE0015 DS    0H                                                               
         LA    R0,ASALTB                                                        
         ST    R0,ASALAST                                                       
         B     SALEEXIT                                                         
*                                                                               
SALE0020 CLI   MODE,PROCMAN                                                     
         BNE   SALE0360                                                         
*                                                                               
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BNE   *+14                                                             
         CLI   QOPTION1,C'C'       MUST BE OPTION 'C'                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   QSTART(6),SPACES                                                 
         BE    SALE0030                                                         
                                                                                
         CLC   RSALLCD(2),=C'00'   ANY DATE?                                    
         BE    SALEEXIT            NO                                           
         GOTO1 DATCON,DMCB,(2,RSALLCD),(3,DATE)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(3,STDATE)                                
         GOTO1 DATCON,DMCB,(0,QEND),(3,ENDDATE)                                 
         CLC   STDATE,DATE         IS DATE WITHIN REQUEST DATES?                
         BH    SALEEXIT            NO - SKIP IT                                 
         CLC   ENDDATE,DATE        IS DATE WITHIN REQUEST DATES?                
         BL    SALEEXIT            NO - SKIP IT                                 
*                                                                               
SALE0030 EQU   *                                                                
         CLC   QTEAM,SPACES                                                     
         BE    SALE0040                                                         
         CLC   RSALTEAM+1(1),QTEAM                                              
         BNE   SALEEXIT                                                         
         SPACE 1                                                                
SALE0040 CLC   QDIV,SPACES                                                      
         BE    SALE0060                                                         
         CLC   RSALTEAM(1),QDIV                                                 
         BNE   SALEEXIT                                                         
         SPACE 1                                                                
SALE0060 EQU   *                                                                
         CLC   QOFFICE,SPACES                                                   
         BE    SALE0080                                                         
         CLC   QOFFICE,RSALOFF                                                  
         BNE   SALEEXIT                                                         
SALE0080 EQU   *                                                                
         XC    TEMPREP,TEMPREP     SAVE OFF INFO FOR 2ND LINE                   
         MVC   TEMPREP(2),RSALPOWR                                              
         OC    TEMPREP,SPACES                                                   
         CLC   RSALPOWR,SPACES                                                  
         BNH   SALE0082                                                         
         TM    RSALPFLG,X'80'      'MINUS' REP?                                 
         BNO   SALE0082            NO                                           
         MVI   TEMPREP+8,C'Y'      YES - SET INDICATOR                          
SALE0082 EQU   *                                                                
         MVC   P+1(3),RSALKSAL                                                  
         MVC   P+16(16),RSALNAME                                                
         MVI   P+34,C'Y'           SET 'USE FOR EDI'                            
         TM    RSALFLG,X'20'       BLOCK EDI USE SET?                           
         BNO   SALE0085            NO                                           
         MVI   P+34,C'N'           SET 'NOT FOR EDI'                            
SALE0085 EQU   *                                                                
         MVI   P+120,C'Y'          SET 'FAX PREFERENCE = YES'                   
         TM    RSALFLG,X'02'       FAX PREFERENCE SET?                          
         BO    SALE0087            YES                                          
         MVI   P+120,C'N'          SET 'FAX PREFERENCE = NO'                    
SALE0087 EQU   *                                                                
         MVC   P+38(12),RSALTEL                                                 
         MVC   P+54(2),RSALTEAM                                                 
         MVC   P+59(10),RTEMDVNM                                                
         MVC   P+70(10),RTEMNAME                                                
         MVC   P+83(2),RSALPROF                                                 
         OC    RSALLEAV,RSALLEAV   ANY LEAVE DATE?                              
         BZ    SALE0090            NO                                           
         GOTO1 DATCON,DMCB,(3,RSALLEAV),(5,P+088)                               
SALE0090 EQU   *                                                                
         MVC   P+105(12),RSALFAX                                                
         CLI   RSALMGR,C'Y'        IS THIS A MANAGER?                           
         BNE   SALE0095            NO                                           
         MVC   P+101(3),=C'YES'                                                 
SALE0095 EQU   *                                                                
         CLC   =C'EQUIVS',QUESTOR  SPECIAL REQUEST?                             
         BNE   SALE0096            NO                                           
         OC    RSALMRG,RSALMRG     YES - ANY EQUIV CODE?                        
         BZ    SALE0096            NO                                           
         EDIT  RSALMRG,(5,P+117),FILL=0                                         
SALE0096 EQU   *                                                                
*                                  BUILD LIST BY SALESMAN NAME                  
         XR    R2,R2               FOR LAST PERIOD IN NAME                      
         XR    R3,R3               FOR LAST SPACE WITHIN NAME                   
         XR    R4,R4               FOR NEXT TO LAST SPACE WITHIN NAME           
         XR    R5,R5               FOR LAST NON-SPACE                           
         LA    R6,20                                                            
         LA    R7,P+30             END OF NAME                                  
         SPACE 1                                                                
SALE0100 CLI   0(R7),C'.'                                                       
         BE    SALE0160                                                         
         CLI   0(R7),C' '                                                       
         BE    SALE0180                                                         
SALE0120 LTR   R5,R5               DO I ALREADY HAVE LAST NON-SPACE             
         BNZ   *+6                 BRANCH IF YES                                
         LR    R5,R7               SAVE ADDRESS OF LAST NON-SPACE               
SALE0140 BCTR  R7,0                BACK OFF 1 CHARACTER                         
         BCT   R6,SALE0100                                                      
         B     SALE0200                                                         
         SPACE 1                                                                
SALE0160 LTR   R2,R2               DO I HAVE PERIOD                             
         BNZ   SALE0140            BRANCH IF YES                                
         LR    R2,R7               SAVE ADDRESS OF PERIOD                       
         B     SALE0120                                                         
         SPACE 1                                                                
SALE0180 LTR   R5,R5               ANY SIGNIFICANT DATA                         
         BZ    SALE0140            NO, MUST BE TRAILING BLANKS                  
         LR    R4,R3               MOVE LAST TO NEXT TO LAST                    
         LR    R3,R7               SAVE LAST                                    
         LTR   R4,R4               DO I HAVE NEXT TO LAST                       
         BZ    SALE0140            NO, SO CONTINUE                              
         SPACE 1                                                                
SALE0200 LA    R6,P+16             FIRST DATA                                   
         LA    R7,19               LENGTH                                       
         XR    RE,RE               LAST DATA                                    
         XR    RF,RF               LENGTH                                       
         SPACE 1                                                                
         CR    R2,R5               END IN PERIOD                                
         BE    SALE0300            YES, LEAVE IT ALONE                          
         LTR   R2,R2                                                            
         BNZ   SALE0220            FOUND PERIOD                                 
         LTR   R3,R3               ANY EMBEDDED SPACES                          
         BZ    SALE0300            NO, SO LEAVE IT ALONE                        
         B     SALE0240                                                         
         SPACE 1                                                                
SALE0220 LR    R6,R2                                                            
         LA    R6,1(R6)            LAST IS PERIOD +1 OR BLANK +1                
         CLI   0(R6),C' '          UNTIL NOT BLANK                              
         BE    *-8                                                              
         LR    R7,R5               LAST TO R7                                   
         SR    R7,R6               LENGTH = LAST - LAST NOT BLANK               
         SPACE 1                                                                
         LA    RE,P+16             FIRST NAME                                   
         LR    RF,R2               PERIOD OR BLANK                              
         SR    RF,RE               LENGTH = PERIOD OR BLANK - P+16              
         B     SALE0300                                                         
         SPACE 1                                                                
SALE0240 LTR   R4,R4                                                            
         BNZ   SALE0280            TWO BLANKS                                   
SALE0260 LR    R2,R3               LAST SPACE SAME AS PERIOD                    
         B     SALE0220                                                         
         SPACE 1                                                                
SALE0280 LR    RE,R4               LAST SPACE                                   
         SR    RE,R3               - NEXT TO LAST                               
         CH    RE,=H'3'                                                         
         BNH   SALE0260                                                         
         LR    R2,R4               FOR MAC LA VAN ETC                           
         B     SALE0220                                                         
         SPACE 1                                                                
SALE0300 L     R2,ASALAST                                                       
         MVC   0(LSALNTRY,R2),SPACES                                            
         MVC   0(LPSALNTR,R2),P                                                 
         MVC   132(12,R2),TEMPREP  INSERT ADDITIONAL DATA                       
         MVC   16(16,R2),SPACES                                                 
         MVC   26(3,R2),P+1        INITIALS                                     
         MVC   SVINIT,P+1                                                       
         MVC   0(16,R2),SPACES                                                  
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),0(R6)       LAST NAME                                    
         SPACE 1                                                                
         LA    R3,3(R7,R2)                                                      
         LTR   RE,RE                                                            
         BZ    SALE0320                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(RE)       FIRST NAME                                   
         SPACE 1                                                                
SALE0320 LA    R2,LSALNTRY(R2)                                                  
         ST    R2,ASALAST                                                       
         L     R3,CNT                                                           
         LA    R3,1(R3)                                                         
         ST    R3,CNT                                                           
*                                                                               
         CLC   CNT,=F'2000'        CK FOR TABLE OVERFLOW                        
         BL    SALE0340                                                         
         DC    F'0'                IF OVERFLOW THEN DUMP                        
         SPACE 1                                                                
SALE0340 CLI   QOPTION1,C'N'       BRANCH AROUND PRINT OUTPUT                   
         BE    SALE0410             HERE IF ONLY (N)AME OR                      
         CLI   QOPTION1,C'O'        (O)FFICE LISTINGS REQUESTED                 
         BE    SALE0410                                                         
***      MVI   SPACING,2                                                        
***>     GOTO1 REPORT                                                           
         BAS   RE,LOCALREP                                                      
*                                                                               
*   FOR 'CODE' SEQUENCE, TEMPREP SHOULD HAVE THE REP/MINUS VALUES               
*        AT THIS POINT.  DON'T RESTORE THEM.                                    
*                                                                               
         BAS   RE,SALASST          DISPLAY SALES ASST EMAIL,NAME,FLAG           
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    SALE0350            YES - NO ADDITIONAL SPACING                  
         GOTO1 REPORT              NO  - ADD ADDITIONAL LINE                    
SALE0350 EQU   *                                                                
         L     R3,PRT                                                           
         LA    R3,1(R3)                                                         
         ST    R3,PRT                                                           
         B     SALEEXIT                                                         
         SPACE 1                                                                
SALE0360 CLI   MODE,REQLAST                                                     
         BNE   SALEEXIT                                                         
         LA    R2,ASALTB                                                        
         L     R3,CNT                                                           
*                                                                               
         PRINT GEN                                                              
         GOTO1 =V(XSORT),DMCB,(0,(R2)),(R3),LSALNTRY,20,1,RR=RELO               
         PRINT NOGEN                                                            
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
*                                                                               
SALE0380 MVC   P(LPSALNTR),0(R2)                                                
         CLI   QOPTION1,C'C'       BRANCH AROUND PRINT OUTPUT (EXIT)            
         BE    SALE0410             IF ONLY (C)ODE LISTING REQUESTED            
         CLI   QOPTION1,C'O'       BRANCH TO (O)FFICE LISTING                   
         BE    SALE0390             IF ONLY (O)FFICE LISTING REQUESTED          
****     MVI   SPACING,2                                                        
***>     GOTO1 REPORT                                                           
         MVC   SVINIT,P+26                                                      
         BAS   RE,LOCALREP                                                      
         OI    MYFLAG,PRNNAME      OFFICE FILTER                                
         MVC   TEMPREP,132(R2)     GET EXTRA DATA FOR DISPLAY                   
         BAS   RE,SALASST                                                       
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    SALE0390            YES - NO ADDITIONAL SPACING                  
         GOTO1 REPORT              NO  - ADD ADDITIONAL LINE                    
*                                                                               
SALE0390 MVC   SVOFF,83(R2)                                                     
         MVC   SVLINE,0(R2)                                                     
         MVC   0(LPSALNTR,R2),SPACES                                            
         MVC   1(2,R2),SVLINE+83                                                
         MVC   4(83,R2),SVLINE                                                  
         MVC   86(46,R2),SVLINE+86 MOVE LAST PORTION OF LINE                    
*                                                                               
*   TEST DUMP                                                                   
***      DC    H'0'                                                             
*   TEST DUMP                                                                   
*                                                                               
*                                     IF ANYTHING IS ADDED TO LINE              
*                                        THIS LENGTH MUST BE CHANGED            
*                                                                               
         LA    R2,LSALNTRY(R2)                                                  
         BCT   R3,SALE0380                                                      
*                                                                               
         CLI   QOPTION1,C'N'       BRANCH AROUND PRINT OUTPUT (EXIT)            
         BE    SALE0410             IF ONLY (N)AME LISTING REQUESTED            
*                                                                               
         LA    R2,ASALTB                                                        
         L     R3,CNT                                                           
         GOTO1 =V(XSORT),DMCB,(0,(R2)),(R3),LSALNTRY,23,1,RR=RELO               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
*                                                                               
SALE0400 MVC   P(LPSALNTR),0(R2)                                                
****     MVI   SPACING,2                                                        
***>     GOTO1 REPORT                                                           
         MVC   SVINIT,P+30                                                      
*                                                                               
*   TEST                                                                        
***      DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         BAS   RE,LOCALREP                                                      
         OI    MYFLAG,PRNOFF       OFFICE FILTER                                
         MVC   TEMPREP,132(R2)     GET EXTRA DATA FOR DISPLAY                   
         BAS   RE,SALASST                                                       
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    SALE0405            YES - NO ADDITIONAL SPACING                  
         GOTO1 REPORT              NO  - ADD ADDITIONAL LINE                    
*                                                                               
SALE0405 EQU   *                                                                
         LA    R2,LSALNTRY(R2)                                                  
         BCT   R3,SALE0400                                                      
*                                                                               
SALE0410 MVC   P(LPSALNTR),SPACES  CLEAR PRINT LINE                             
*                                                                               
         NI    MYFLAG,X'FF'-PRNNAME-PRNOFF                                      
SALEEXIT XMOD1 1                                                                
         EJECT                                                                  
         GETEL R6,34,ELCODE                                                     
SALMERGE EQU   *                                                                
         CLI   MODE,REQFRST                                                     
         BNE   SALEEXIT            ENTIRE REPORT PROCESSED                      
*                                      UNDER REQFRST                            
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,3                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           INSERT RECORD TYPE                           
         MVC   KEY+22(2),RCREPFL   INSERT REP CODE                              
         GOTO1 HIGH                READ FIRST KEY                               
         CLC   KEY(24),KEYSAVE     FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                NO  - NO SALESPERSON?                        
         GOTO1 GETSAL              RETRIEVE THE RECORD                          
         B     SMER0040                                                         
SMER0020 EQU   *                                                                
         GOTO1 SEQ                                                              
         CLC   KEY(24),KEYSAVE     FOUND?                                       
         BNE   SALEEXIT            NO  - FINISHED                               
         GOTO1 GETSAL              YES - RETRIEVE THE RECORD                    
*                                                                               
SMER0040 EQU   *                                                                
         CLC   QTEAM,SPACES                                                     
         BE    SMER0060                                                         
         CLC   RSALTEAM+1(1),QTEAM                                              
         BNE   SALEEXIT                                                         
         SPACE 1                                                                
SMER0060 CLC   QDIV,SPACES                                                      
         BE    SMER0080                                                         
         CLC   RSALTEAM(1),QDIV                                                 
         BNE   SALEEXIT                                                         
         SPACE 1                                                                
SMER0080 EQU   *                                                                
         CLC   QOFFICE,SPACES                                                   
         BE    SMER0100                                                         
         CLC   QOFFICE,RSALOFF                                                  
         BNE   SALEEXIT                                                         
SMER0100 EQU   *                                                                
         MVC   P+1(3),RSALKSAL                                                  
         MVC   P+16(20),RSALNAME                                                
         MVC   P+38(12),RSALTEL                                                 
         MVC   P+54(2),RSALTEAM                                                 
         MVC   P+59(10),RTEMDVNM                                                
         MVC   P+70(10),RTEMNAME                                                
         MVC   P+83(2),RSALPROF                                                 
         MVC   P+88(12),RSALFAX                                                 
         OC    RSALMRG,RSALMRG     ANY MERGER CODE?                             
         BZ    SMER0120            NO                                           
         EDIT  RSALMRG,(5,P+104),FILL=0                                         
*****>   MVC   P+104(3),RSALMRG    YES - INSERT MERGE CODE                      
*****>   BAS   RE,MERGCODE         YES - RETRIEVE MERGER NAME                   
         MVC   P+110(20),RSALNAME  INSERT SALESPERSON NAME                      
SMER0120 EQU   *                                                                
****     MVI   SPACING,2                                                        
***>     GOTO1 REPORT                                                           
         BAS   RE,LOCALREP                                                      
         BAS   RE,SALASST                                                       
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    SMER0020            YES - NO ADDITIONAL SPACING                  
         GOTO1 REPORT              NO  - ADD ADDITIONAL LINE                    
         B     SMER0020            GO BACK FOR NEXT                             
         EJECT                                                                  
MERGCODE NTR1                                                                   
         MVC   KEYMERG,KEY         SAVE KEY FOR RESTART                         
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'06'           INSERT RECORD TYPE                           
         MVC   KEY+22(2),=C'DI'    INSERT DI CODE                               
         MVC   KEY+24(3),RSALMRG   INSERT MERGE CODE                            
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                MUST BE THERE                                
         GOTO1 GETSAL              RETRIEVE RECORD                              
         MVC   P+110(20),RSALNAME  INSERT SALESPERSON NAME                      
         MVC   KEY(27),KEYMERG     RESET ORIGINAL KEY                           
         GOTO1 HIGH                RESTART KEY                                  
         XIT1                                                                   
        EJECT                                                                   
GETSAL   LA    RF,RSALREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION?               
         BZ    DM020                                                            
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
MODEEXIT EQU   *                                                                
         XIT1                                                                   
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    MODEEXIT                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
**********************************************************************          
* DISPLAY ASSISTANT INFORMATION + SALES EMAIL ADDRESS                           
**********************************************************************          
SALASST  NTR1                                                                   
**TEST   MVC   P(3),SVINIT                                                      
**TEST   GOTO1 REPORT                                                           
*                                                                               
*  TEST                                                                         
***      MVC   P+1(72),0(R2)                                                    
***      GOTO1 REPORT                                                           
***      MVC   P+1(72),72(R2)                                                   
***      GOTO1 REPORT                                                           
***      GOTO1 REPORT                                                           
*  TEST                                                                         
*                                                                               
         USING PRINLINE,P                                                       
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
K        USING RSA2REC,KEY                                                      
         MVI   KEY,X'46'                                                        
         MVC   K.RSA2KREP,RSALKREP                                              
         MVC   K.RSA2KSAL,SVINIT                                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BNE   SALASSTX                                                         
         DROP  K                                                                
*                                                                               
         LA    RF,RADVREC       USE ADV SPACE TO READ SAL2 REC                  
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                               PRINT SALES EMAIL ADDRESS                       
SALA0010 EQU   *                                                                
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   SALA0020                                                         
*                                                                               
         USING RSALEMEM,R6                                                      
         ZIC   RF,RSALEMLN                                                      
         SHI   RF,3             1 ELT CODE + 1 LENGTH + 1 EX                    
         CHI   RF,0                                                             
         BL    SALA0020                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PSALEML(0),RSALEMAL                                              
         OI    MYFLAG,HASSAL                                                    
*                                                                               
SALA0020 EQU   *                PRINT SALES ASSISTANT NAME+EMAIL+FLAG           
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRTLINE                                                          
*                                                                               
         USING RSASEMEM,R6                                                      
         MVC   PSASEMNM,RSASEMNM                                                
         CLI   RSASEMLN,L'RSASEMNM+2                                            
         BNH   PRTLINE                                                          
*                                                                               
         MVC   PSASEMFL,=C'YES'                                                 
         TM    RSASEMFL,X'80'                                                   
         BO    *+10                                                             
         MVC   PSASEMFL,=C'NO '                                                 
*                                                                               
         CLI   RSASEMLN,L'RSASEMNM+L'RSASEMFL+2                                 
         BNH   PRTLINE                                                          
         ZIC   RF,RSASEMLN                                                      
         SHI   RF,L'RSASEMNM+L'RSASEMFL+2+1                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   PSASEAML(0),RSASEAML                                             
         OI    MYFLAG,HASSAS                                                    
PRTLINE  EQU   *                                                                
         TM    MYFLAG,HASSAL+HASSAS                                             
         BZ    SALASSTX                                                         
         TM    MYFLAG,PRNOFF                                                    
         BZ    PRT0010                                                          
         USING PRINLIN2,TEMPLINE   REALIGN FOR OFFICE FILTER                    
         XC    TEMPLINE,TEMPLINE                                                
         MVC   PSALEML2,PSALEML                                                 
         MVC   PSASEMN2,PSASEMNM                                                
         MVC   PSASEAM2,PSASEAML                                                
         MVC   PSASEMF2,PSASEMFL                                                
         MVC   PSALERP2+3(12),TEMPREP                                           
         XC    P,P                                                              
         MVC   P,TEMPLINE                                                       
         B     OUT0100                                                          
PRT0010  EQU   *                                                                
         TM    MYFLAG,PRNNAME                                                   
         BZ    OUT0030                                                          
         USING PRINLIN3,TEMPLINE   REALIGN FOR NAME FILTER                      
         XC    TEMPLINE,TEMPLINE                                                
         MVC   PSALEML3,PSALEML                                                 
         MVC   PSASEMN3,PSASEMNM                                                
         MVC   PSASEAM3,PSASEAML                                                
         MVC   PSASEMF3,PSASEMFL                                                
         MVC   PSALERP3+2(12),TEMPREP                                           
         XC    P,P                                                              
         MVC   P,TEMPLINE                                                       
         B     OUT0100                                                          
*                                                                               
OUT0030  EQU   *                                                                
         MVC   PSALEREP(12),TEMPREP                                             
OUT0100  EQU   *                                                                
         NI    MYFLAG,X'FF'-HASSAL-HASSAS                                       
         BAS   RE,LOCA2REP                                                      
SALASSTX EQU   *                                                                
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         XIT1                                                                   
**********************************************************************          
* LOCAL REP - DOWNLOAD HANDLING                                                 
**********************************************************************          
LOCALREP NTR1                                                                   
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BNE   LOCALNO             NO - JUST GOTO REPORT                        
                                                                                
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
                                                                                
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         LA    R1,DWNDEF           R1 -> DOWNOAD DEFINITION                     
         DROP  R5                                                               
                                                                                
LOCAL010 DS    0H                                                               
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LOCAL020                                                         
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LOCAL010                                                         
                                                                                
LOCAL020 DS    0H                                                               
         XC    DWNBUFF,DWNBUFF                                                  
         MVC   DWNBUFF+00(03),P+1    CODE/INITIALS                              
         MVC   DWNBUFF+03(16),P+16   NAME                                       
         MVC   DWNBUFF+19(01),P+34   EDI FLAG                                   
         MVC   DWNBUFF+20(12),P+38   TELEPHONE NUMBER                           
         MVC   DWNBUFF+32(02),P+54   TEAM                                       
         MVC   DWNBUFF+34(21),P+59   DIVISION                                   
         MVC   DWNBUFF+55(02),P+83   OFFICE                                     
         MVC   DWNBUFF+57(08),P+88   LEAVE DATE                                 
         MVC   DWNBUFF+65(03),P+101  MGR FLAG                                   
         MVC   DWNBUFF+68(12),P+105  FAX NUMBER                                 
         MVC   DWNBUFF+80(01),P+120  FAX FLAG                                   
*                                                                               
*  TEST                                                                         
         LA    RF,P                                                             
         LA    RE,DWNBUFF                                                       
***      DC    H'0'                                                             
*   TEST END                                                                    
*                                                                               
         XC    P,P                                                              
         MVC   P(L'DWNBUFF),DWNBUFF                                             
*                                                                               
         MVI   LINE,1              NEVER PAGE BREAK                             
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
         B     LOCALX                                                           
*                                                                               
LOCALNO  DS    0H                                                               
         GOTO1 REPORT                                                           
                                                                                
LOCALX   DS    0H                                                               
         XIT1                                                                   
**********************************************************************          
*  LOCAL REP FOR SALES ASSISTANT- DOWNLOAD HANDLING                             
**********************************************************************          
LOCA2REP NTR1                                                                   
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BNE   LOCA2NO             NO - JUST GOTO REPORT                        
                                                                                
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
                                                                                
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         LA    R1,DWNDEF1          R1 -> DOWNOAD DEFINITION                     
         DROP  R5                                                               
                                                                                
LOCA2010 DS    0H                                                               
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LOCA2020                                                         
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LOCA2010                                                         
LOCA2020 DS    0H                                                               
         XC    DWNBUFF,DWNBUFF     CHECK HERE                                   
         MVC   DWNBUFF+00(02),PSALEREP                                          
         MVC   DWNBUFF+02(01),PSALEREP+8                                        
         LA    R4,PSALEML                                                       
         LA    R5,PSASEMNM                                                      
         LA    R6,PSASEAML                                                      
         LA    R7,PSASEMFL                                                      
*                                                                               
*  ONLY THE 'CODE' OPTION DOWNLOADS.  THE REST OF THIS CODE                     
*        IS CRAP.  WHOEVER DID IT DIDN'T READ THE REST OF THE                   
*        PROGRAM.                                                               
*                                                                               
         TM    MYFLAG,PRNOFF                                                    
         BZ    LOCA2030                                                         
         USING PRINLIN2,P                                                       
         LA    R4,PSALEML2                                                      
         LA    R5,PSASEMN2                                                      
         LA    R6,PSASEAM2                                                      
         LA    R7,PSASEMF2                                                      
*                                                                               
LOCA2030 DS    0H                                                               
         TM    MYFLAG,PRNNAME                                                   
         BZ    LOCA2040                                                         
         USING PRINLIN3,P                                                       
         LA    R4,PSALEML3                                                      
         LA    R5,PSASEMN3                                                      
         LA    R6,PSASEAM3                                                      
         LA    R7,PSASEMF3                                                      
*                                                                               
LOCA2040 DS    0H                                                               
         MVC   DWNBUFF+03(32),0(R4)                                             
         MVC   DWNBUFF+35(20),0(R5)                                             
         MVC   DWNBUFF+55(30),0(R6)                                             
         MVC   DWNBUFF+85(03),0(R7)                                             
*                                                                               
         XC    P,P                                                              
         MVC   P(L'DWNBUFF),DWNBUFF                                             
*                                                                               
         MVI   LINE,1              NEVER PAGE BREAK                             
         GOTO1 REPORT,DMCB,WORKC,=C'PRINT'                                      
         B     LOCA2X                                                           
*                                                                               
LOCA2NO  DS    0H                                                               
         GOTO1 REPORT                                                           
                                                                                
*        NI    MYFLAG,X'FF'-PRNOFF-PRNNAME                                      
LOCA2X   DS    0H                                                               
         XIT1                                                                   
                                                                                
DWNDEF   DC    C'T',AL1(03)        CODE/INITIALS                                
         DC    C'T',AL1(16)        NAME                                         
         DC    C'T',AL1(01)        EDI FLAG                                     
         DC    C'T',AL1(12)        TELEPHONE #                                  
         DC    C'T',AL1(02)        TEAM                                         
         DC    C'T',AL1(21)        DIVISION                                     
         DC    C'T',AL1(02)        OFFICE                                       
         DC    C'T',AL1(08)        LEAVE DATE                                   
         DC    C'T',AL1(03)        MANAGER FLAG                                 
         DC    C'T',AL1(12)        FAX NUMBER                                   
         DC    C'T',AL1(01)        FAX FLAG                                     
         DC    X'0000'                                                          
         EJECT                                                                  
DWNDEF1  EQU   *                                                                
         DC    C'T',AL1(02)   +00  POWER CODE                                   
         DC    C'T',AL1(01)   +02  MINUS FLAG                                   
         DC    C'T',AL1(32)   +03  S/P EMAIL ADDR                               
         DC    C'T',AL1(20)   +35  S/A NAME                                     
         DC    C'T',AL1(30)   +55  S/A EMAIL ADDR                               
         DC    C'T',AL1(03)   +85  S/A PREFERENCE?                              
         DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
***>     GOTO1 REPORT                                                           
         BAS   RE,LOCALREP                                                      
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
***>     GOTO1 REPORT                                                           
         BAS   RE,LOCALREP                                                      
         B     MODEEXIT                                                         
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
SVOFF    DS    CL2                 OFFICE                                       
SVLINE   DS    CL(LPSALNTR)        TEMP FOR RE-ARRANGING PRINT LINE             
CNT      DS    F                                                                
PRT      DS    F                                                                
RELO     DS    A                                                                
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
KEYMERG  DS    CL27                                                             
SAVEKEY  DS    CL27                                                             
DATE     DS    CL3                 LAST CHANGED DATE IN BINARY                  
STDATE   DS    CL3                 FROM: REQUEST DATE                           
ENDDATE  DS    CL3                 TO: REQUEST DATE                             
DWNBUFF  DS    CL96                DOWNLOAD LINE BUFFER                         
ELCODE   DS    XL1                                                              
SVINIT   DS    CL3                                                              
MYFLAG   DS    XL1                                                              
PRNOFF   EQU   X'80'               FILTERED BY OFFICE?                          
HASSAL   EQU   X'40'               HAS SALES EMAIL ADDRESS?                     
HASSAS   EQU   X'20'               HAS SALES ASSISTANT ADDRESSS?                
PRNNAME  EQU   X'10'               FILTERED BY OFFICE?                          
TEMPLINE DS    XL132                                                            
TEMPREP  DS    CL12                                                             
         LTORG                                                                  
ASALAST  DS    F                                                                
*                                                                               
ASALTB   DS    (LSALNTRY*2500)CL1  SALESMAN TABLE                               
*                                                                               
LPSALNTR EQU   132                 LENGTH OF PRINT LINE                         
LSALNTRY EQU   144                 LENGTH OF SALESMAN TABLE ENTRY               
*                                                                               
                                                                                
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
PRINLINE DSECT                                                                  
         DS    CL01                CODE OR DEFAULT SEQUENCE                     
PSALEREP DS    CL15                REP POWER CODE                               
PSALEML  DS    CL32                                                             
         DS    CL11                                                             
PSASEMNM DS    CL20                                                             
         DS    CL9                                                              
PSASEAML DS    CL30                                                             
         DS    CL2                                                              
PSASEMFL DS    CL3                                                              
*                                                                               
PRINLIN2 DSECT                                                                  
         DS    CL5                                                              
PSALEML2 DS    CL34                OFFICE SEQUENCE                              
PSALERP2 DS    CL24                                                             
PSASEMN2 DS    CL20                                                             
         DS    CL5                                                              
PSASEAM2 DS    CL30                                                             
         DS    CL2                                                              
PSASEMF2 DS    CL3                                                              
*                                                                               
PRINLIN3 DSECT                                                                  
         DS    CL1                 NAME SEQUENCE                                
PSALEML3 DS    CL35                                                             
PSALERP3 DS    CL23                                                             
PSASEMN3 DS    CL20           ALIGN WITH TEL NUM                                
         DS    CL09                                                             
PSASEAM3 DS    CL30                                                             
         DS    CL2                                                              
PSASEMF3 DS    CL3                                                              
*                                                                               
GENSAL2  DSECT                                                                  
       ++INCLUDE REGENSAL2                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'120REREP7802 04/16/03'                                      
         END                                                                    
