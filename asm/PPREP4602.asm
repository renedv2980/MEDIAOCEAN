*          DATA SET PPREP4602  AT LEVEL 015 AS OF 10/13/08                      
*PHASE PP4602A                                                                  
*INCLUDE PSIZEVAL                                                               
*INCLUDE PRNTOFC                                                                
         PRINT NOGEN                                                            
         TITLE 'PUBLICATION INFORMATION SHEET'                                  
*                                                                               
*  SMYE 10/07/08 USE PRNTOFC TO PRINT CLIENT OFFICES                            
*                                                                               
*  BPLA 05/09/08 EXPAND POOL                                                    
*                                                                               
*  SMYE 03/01/01 FIX BUG IN E-MAIL HANDLING FOR 1-POSITION ADDRESS              
*                                                                               
*  KWAN 04/20/00 CHANGES FOR PPNEWFILE (LARGER PCONREC, ETC.)                   
*                                                                               
*  SMYE 03/03/00 INCLUDE CLIENT CODE IN PREMIUMS (PRTPREM)                      
*                                                                               
*  SMYE 11/99    DISPLAY EXCLUSIONS FIELD FOR ALL MEDIA (NOT JUST N)            
*                                                                               
*  KWAN 07/99    REMOVE ELEM LEN CHKING FOR PUB ADDR RECS                       
*                                                                               
*  SMYE 03/99    DISPLAY E-MAIL ADDRESS INFO. (IN ADDR...)                      
*                AND FIX BUG IN ADDR.. - WAS NOT RESTORING SEQ. READ            
*                  OF PUB ADDRESS RECORDS AFTER LOOKING FOR CONTROL             
*                  FILE FAX NUMBERS                                             
*                                                                               
*  SMYE 11/98    DISPLAY WEB SITE ELEMENT INFO.                                 
*                                                                               
*  SMYE 09/98    DISPLAY PUB AD SIZING INFORMATION (PRTSZE PROC)                
*                                                                               
*  SMYE 10/97    PRTPGA MOVED TO ITS OWN CSECT                                  
*                INITIALIZED 64 BYTE FIELD CALLLED PPGKEY IN PU4P PROC.         
*                THIS FIELD WAS NEVER INITIALIZED WITH ANY VALUES IN            
*                THE PAST BUT WAS BEING USED TO "RESTORE PPG'S KEYS".           
*                SET ALLOWLIN TO 2 BEFORE EVERY LINE TO BE UNDERSCORED          
*                TO PREVENT SPLITTING OF THESE LINES IF AT PAGE END.            
*                                                                               
*  BPLA 09/97    FIX FAX AND TELEPHONE DISPLAYS IN HEADS                        
*                                                                               
*  SMYE 08/97    ADDED ROUTINE (GETFAX) TO GET FAX FROM CONTROL FILE            
*                                                                               
*  SMYE 05/97    USE EITHER PUB ADDRESS RECORDS OR ADDRESS ELEMENTS             
*                AT ADDR..                                                      
*                                                                               
*  SMYE 09/16/96 DISPLAY PUB GROUP ASSIGNMENTS (PRTPGA PROC)                    
*                                                                               
*  SMYE 12/13/95 CHANGED DTCNV TO DATCON WITH NEW PARAM'S                       
*                                                                               
*  SMYE 11/20/95 ADDED CODE TO PRTREPS (PRINT REP DETAILS) TO                   
*                PRINT ADDITIONAL INFO. FROM EXPANDED ELEM. X'14'               
*                                                                               
*  BPLA 10/6/95  DISPLAY LANGUAGE (PUBLANG)                                     
*                ALSO ADD QOPT5 (LANGUAGE FILTER)                               
*                PRTPROD MOVED TO ITS OWN CSECT                                 
*                                                                               
*  BPLA 11/30/93 DISPLAY PST CODES                                              
*                                                                               
*  BPLA 2/24/93  EXPAND POOL                                                    
*                                                                               
*  BPLA 11/1/91  SHIPP ADDRESS ADDED                                            
*                                                                               
*   11/15/90 ROSA  ADD GST TAX CODE                                             
*                                                                               
*   12/17/89 ROSA  ADD PHONE AND FAX TELEPHONE NUMBERS                          
*                                                                               
PP4602   CSECT                                                                  
         NMOD1 0,**PP4602,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R9,PPFILEC                                                       
         LA    R8,2048(R9)                                                      
         LA    R8,2048(R8)                                                      
         USING PPFILED,R9,R8                                                    
         LA    RC,PUBREC                                                        
         DROP  R8                                                               
         USING PUBREC,RC                                                        
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING PP4602,RB,R9                                                     
*                                                                               
*******  L     RF,PPWORK2C                                                      
****     USING PPWORK2D,RF                                                      
******   MVC   ACONIO1,ACONIO                                                   
******   DROP  RF                                                               
*                                                                               
         LA    R2,PUBNAMEL                                                      
         SR    R3,R3                                                            
         L     R8,=A(POOL)                                                      
         A     R8,RELO                                                          
         EJECT                                                                  
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   PU2                                                              
*                                                                               
         L     R6,=V(PRNTOFC)                                                   
         A     R6,RELO                                                          
         ST    R6,VPRNTOFC         SAVE ADDRESS OF PRNTOFC                      
*                                                                               
         XC    DMCB(4),DMCB        NEED ADDRESS OF OFFICER                      
         MVC   DMCB+4(4),=X'D9000A38'                                           
         L     R6,VCOMFACS                                                      
         L     R6,(CCALLOV-COMFACSD)(R6)                                        
         GOTOR (R6),DMCB                                                        
         MVC   VOFFICER,DMCB       SAVE ADDRESS OF OFFICER                      
*                                                                               
         MVC   DUB,SPACES          GET PSTVAL                                   
         MVC   DUB(6),=C'T00A6B'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   APSTVAL,4(R1)          SAVE ITS ADDR                             
         MVI   GRPSW,2             SET 1ST TIME - GRP ASSGN SWITCH              
*                                                                               
         SPACE 2                                                                
PU2      CLI   MODE,PROCPUB                                                     
         BNE   PUEXT                                                            
*                                                                               
         CLI   QOPT5,C' '         SEE IF LANGUAGE FILTER SPECIFIED              
         BE    PU2C                                                             
         OI    PUBLANG,C' '                                                     
         CLI   PUBLANG,C' '                                                     
         BNE   *+8                                                              
         MVI   PUBLANG,C'E'    SO SPACE WILL ALSO MATCH "E" - ENGLISH           
         CLC   PUBLANG,QOPT5                                                    
         BNE   PUEXT                                                            
                                                                                
PU2C     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         CLI   QUESTOR,C' '                                                     
         BNE   PU3                                                              
         CLC   QUESTOR,SPACES                                                   
         BE    PU3                                                              
         PACK  STPUB(6),QUESTOR+1(11)   QUESTOR+1 = START PUB                   
         MVC   STPUB+5(1),QUESTOR+11                                            
         CLI   QUESTOR+11,C' '                                                  
         BNE   *+8                                                              
         MVI   STPUB+5,0                                                        
         CLC   STPUB(6),PUBKPUB                                                 
         BH    PUEXT                                                            
         SP    RCSPECNO,=P'1'      RCSPECNO = NO. TO PRINT                      
         BM    PUEXT                                                            
PU3      EQU   *                                                                
***                                                                             
***   NOTE KILL DATE, TELEPHONE AND FAX WILL ONLY                               
***      APPEAR IN HEADLINES ON FIRST PAGE OF PUB DATA                          
***                                                                             
         MVI   FORCEHED,C'Y'                                                    
         OC    PUBKILL,PUBKILL                                                  
         BZ    NOKILL                                                           
         MVC   HEAD7+76(10),=C'KILL DATE-'                                      
         GOTO1 DATCON,DMCB,(3,PUBKILL),(5,HEAD7+86)                             
*                                                                               
NOKILL   LA   RF,PUBREC+33                                                      
         MVC   HEAD4+49(12),SPACES                                              
         MVC   HEAD5+49(12),SPACES                                              
         CLI  0(RF),X'11'                                                       
         BE   IS11EL                                                            
NOKILLA  ZIC  RE,1(RF)                                                          
         AR    RF,RE                                                            
         CLI   0(RF),0                                                          
         BE    PU4                                                              
         CLI   0(RF),X'11'                                                      
         BNE   NOKILLA                                                          
         USING PPDUMD11,RF                                                      
*                                                                               
IS11EL   DS    0H                                                               
         MVC   HEAD4+43(6),=C'PHONE-'                                           
         MVC   HEAD5+45(4),=C'FAX-'                                             
         MVC   HEAD4+49(12),PUBTEL                                              
         MVC   HEAD5+49(12),PUBSFAXN                                            
         CLC   =C'FX=',PUBSFAXN    FAX IN CONTROL FILE ?                        
         BNE   PU4                 NO                                           
*                               LOOK FOR FAX NUMBER IN CONTROL FILE             
         MVC   FRSVFAX,PUBSFAXN                                                 
         BAS   RE,GETFAX                                                        
         MVC   HEAD6+45(7),=C'FAX NO='                                          
         MVC   HEAD6+52(16),TOFAX                                               
*                                                                               
PU4      DS    0H                                                               
         CLI   PUBLANG,C'F'          ONLY IF FRENCH FOR NOW                     
         BNE   WEBTST                                                           
         MVC   HEAD7+40(9),=C'LANGUAGE-'                                        
         MVC   HEAD7+49(6),=C'FRENCH'                                           
         DROP  RF                                                               
*                                    DISPLAY WEB SITE ELEMENT INFO.             
WEBTST   LA    RF,PUBREC+33                                                     
WEBLUP   CLI   0(RF),X'70'         WEB SITE ELEMENT ?                           
         BE    WEBOUT              YES                                          
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         CLI   0(RF),0             EOR ?                                        
         BE    PU4P                YES                                          
         B     WEBLUP              TEST NEXT ELEMENT                            
WEBOUT   MVC   HEAD9+1(18),=C'WEBSITE ADDRESS = '                               
         ZIC   RE,1(RF)            ELEMENT LENGTH (VARIABLE)                    
         SH    RE,=H'3'            PREP FOR EXECUTED MOVE                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   HEAD9+19(0),2(RF)   EXECUTED MOVE                                
         SPACE 2                                                                
PU4P     OC    PUBPLSH,PUBPLSH       CHECK FOR PUBLISHER                        
         BZ    PU5                                                              
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   P+1(9),=C'PUBLISHER'                                             
         MVC   PSECOND+1(9),=20C'-'                                             
         MVC   P+11(4),PUBPLSH                                                  
         MVC   PPGKEY,KEY         SAVE PPG KEY                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),PUBKAGY                                                   
         MVC   KEY+2(1),PUBKMED                                                 
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),PUBPLSH                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE                                                  
         BE    PU4C                                                             
         MVC   P+20(26),=C'** NOT FOUND - CALL DDS **'                          
         GOTO1 REPORT                                                           
         MVC   KEY(64),PPGKEY      RESTORE PPGKEY                               
         B     PU5                                                              
*                                                                               
PU4C     MVC   FULL,AREC          STORE PPG'S AREC                              
         L     R6,PPFILEC                                                       
         LA    R7,2048(R6)                                                      
         LA    R7,2048(R7)                                                      
         USING PPFILED,R6,R7                                                    
         LA    R0,PREPREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   P+20(30),PREPNAME                                                
         MVC   PSECOND+20(30),PREPLIN1                                          
         GOTO1 REPORT                                                           
         MVC   P+20(30),PREPLIN2                                                
         CLI   PREPELEM+1,152                                                   
         BL    NOTELEP                                                          
         MVC   P+60(6),=C'PHONE-'                                               
         MVC   P+66(12),PREPTEL                                                 
NOTELEP  DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P+20(26),PREPLIN3                                                
         CLI   PREPELEM+1,164                                                   
         BL    NOFAXNO                                                          
         MVC   P+60(4),=C'FAX-'                                                 
         MVC   P+64(12),PREPFAX                                                 
         CLC   =C'FX=',PREPFAX                                                  
         BNE   NOFAXNO                                                          
*                               LOOK FOR FAX NUMBER IN CONTROL FILE             
         XC    P+74(2),P+74                                                     
         MVC   FRSVFAX,PREPFAX                                                  
         BAS   RE,GETFAX                                                        
         MVI   P+75,C'('                                                        
         MVC   P+76(16),TOFAX                                                   
         MVI   P+92,C')'                                                        
*                                                                               
NOFAXNO  DS    0H                                                               
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   AREC,FULL                                                        
         MVC   KEY(64),PPGKEY                                                   
         DROP  R6                                                               
         DROP  R7                                                               
*                                                                               
PU5      DS    0H                                                               
         BAS   RE,PRODPRT                                                       
         BAS   RE,PRTCIRC                                                       
         SPACE 2                                                                
         CLI   QMEDIA,C'N'                                                      
         BNE   *+8                                                              
         BAS   RE,PRTCLE                                                        
PU6      BAS   RE,PRTRATE                                                       
         BAS   RE,PRTGST                                                        
         BAS   RE,PRTPST                                                        
         BAS   RE,PRTTAX                                                        
         BAS   RE,PRTPREM                                                       
         BAS   RE,PRTREPS                                                       
         BAS   RE,PRTDIST                                                       
         BAS   RE,PRTPADR                                                       
         BAS   RE,PRTTADR                                                       
         BAS   RE,PRTCADR                                                       
         BAS   RE,PRTSADR                                                       
         BAS   RE,PRTCOM                                                        
         BAS   RE,PGAPRT                                                        
         BAS   RE,PSZPRT                                                        
         SPACE 2                                                                
PUEXT    XMOD1 1                                                                
*                                                                               
         DC    F'0'                                                             
PRODPRT  ST    RE,PRODPRT-4                                                     
         GOTO1 =A(PRTPROD),DMCB,(RC)                                            
         L     RE,PRODPRT-4                                                     
         BR    RE                                                               
*                                                                               
*                                                                               
         DC    F'0'                                                             
PGAPRT   ST    RE,PGAPRT-4                                                      
         GOTO1 =A(PRTPGA),DMCB,(RC)                                             
         L     RE,PGAPRT-4                                                      
         BR    RE                                                               
*                                                                               
         DC    F'0'                                                             
PSZPRT   ST    RE,PSZPRT-4                                                      
         GOTO1 =A(PRTSZE),DMCB,(RC)                                             
         L     RE,PSZPRT-4                                                      
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
         SPACE 3                                                                
PRTGST   NTR1                                                                   
         SPACE 2                                                                
*                                                                               
         CLI   PUBGST,0                                                         
         BE    XIT                                                              
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   P+1(12),=C'GST TAX CODE '                                        
         MVC   P+20(1),PUBGST                                                   
         MVC   PSECOND+1(12),=20C'-'                                            
*                                                                               
         GOTO1 REPORT                                                           
         B     XIT                                                              
*                                                                               
         SPACE 3                                                                
*                                                                               
*        DISPLAY PST CODES                                                      
*                                                                               
PRTPST   NTR1                                                                   
*                                                                               
         XC    WORK(64),WORK                                                    
*                                                                               
         LA    R2,PUBREC+33                                                     
PRTPST2  CLI   0(R2),X'90'                                                      
         BE    PRTPST5                                                          
         CLI   0(R2),0             END OF REC                                   
         BE    XIT                                                              
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     PRTPST2                                                          
*                                                                               
PRTPST5  LA    R1,2(R2)             POINT R1 TO PSTCODES                        
         LA    R2,PSTBLK                                                        
         USING PSTBLKD,R2                                                       
         XC    0(PSTLNQ,R2),0(R2)     CLEAR INTERFACE BLOCK                     
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         ST    R1,PSTADIN          INPUT ADDRESS                                
         LA    R1,WORK                                                          
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         GOTO1 APSTVAL,DMCB,(R2)                                                
         CLI   WORK,C' '         SEE IF I HAVE PST CODES                        
         BNH   XIT                                                              
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   P+1(3),=C'PST'                                                   
         MVC   PSECOND+1(3),=20C'-'                                             
         MVC   P+5(49),WORK                                                     
         DROP  R2                                                               
         GOTO1 REPORT                                                           
         B     XIT                                                              
*                                                                               
         SPACE 2                                                                
*              PRINT CIRCULATION DETAILS                                        
         SPACE 3                                                                
PRTCIRC  NTR1                                                                   
         SPACE 2                                                                
         MVI   CIRCSW,0                                                         
         LA    R2,PUBREC+33                                                     
CI2      CLI   0(R2),X'30'                                                      
         BE    CI4                                                              
         CLI   0(R2),X'00'                                                      
         BE    CI12                                                             
CI3      ZIC   R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     CI2                                                              
CI4      CLI   CIRCSW,1                                                         
         BE    CI5                                                              
         SPACE 2                                                                
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   P+1(11),=C'CIRCULATION'                                          
         B     CI5B                                                             
*                                                                               
CI5      GOTO1 REPORT                                                           
CI5B     MVC   P+20(18),=C'CIRCULATION DATE ='                                  
         USING PUBCIREL,R2                                                      
         OC    PUBCDAT,PUBCDAT                                                  
         BZ    CI6                                                              
         CLI   PUBCDAT+2,0         CHK FOR DAY                                  
         BNE   CI5D                                                             
         GOTO1 DATCON,DMCB,(3,PUBCDAT),(6,P+39)                                 
         B     CI6                                                              
*                                                                               
CI5D     GOTO1 DATCON,DMCB,(3,PUBCDAT),(5,P+39)                                 
CI6      GOTO1 REPORT                                                           
         SPACE 2                                                                
         CLI   CIRCSW,1                                                         
         BE    *+10                                                             
         MVC   P+1(11),=20C'-'                                                  
         MVC   P+20(19),=C'TOTAL CIRCULATION ='                                 
         EDIT  PUBCIR1,(10,P+40),ALIGN=LEFT,COMMAS=YES                          
CI8      GOTO1 REPORT                                                           
         SPACE 2                                                                
         CLC   PUBCSRC,SPACES                                                   
         BNH   CI11                                                             
         MVC   P+20(20),=C'CIRCULATION SOURCE ='                                
         MVC   P+41(4),PUBCSRC                                                  
         SPACE 2                                                                
*                                                                               
         GOTO1 REPORT                                                           
CI11     MVI   CIRCSW,1                                                         
         B     CI3                                                              
*                                                                               
         SPACE 2                                                                
CI12     DS    0H                                                               
         CLI   CIRCSW,0            NO CIRCS                                     
         BE    XIT                                                              
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
PRTCLE   NTR1                                                                   
         MVI   CLESW,0                                                          
         SR    R7,R7                                                            
         LA    R4,P+20                                                          
         LA    R2,PUBREC+33                                                     
CLE1     CLI   0(R2),X'40'                                                      
         BE    CLE4                                                             
         CLI   0(R2),0             END OF REC                                   
         BE    CLE12                                                            
*                                                                               
CLE3     ZIC   R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     CLE1                                                             
*                                                                               
CLE4     DS    0H                                                               
         CLI   CLESW,1                                                          
         BE    CLE5                                                             
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   P+1(17),=C'SPACE-LINES EQUIV'                                    
         MVC   PSECOND+1(17),=20C'-'                                            
         MVC   P+20(14),=C'SPACE    LINES'                                      
         MVC   P+40(14),=C'SPACE    LINES'                                      
         MVC   P+60(14),=C'SPACE    LINES'                                      
         MVC   PSECOND+20(5),=20C'-'                                            
         MVC   PSECOND+29(5),=20C'-'                                            
         MVC   PSECOND+40(5),=20C'-'                                            
         MVC   PSECOND+49(5),=20C'-'                                            
         MVC   PSECOND+60(5),=20C'-'                                            
         MVC   PSECOND+69(5),=20C'-'                                            
         MVI   CLESW,1                                                          
         GOTO1 REPORT                                                           
*                                                                               
CLE5     AR    R4,R7                                                            
         MVC   0(6,R4),2(R2)                                                    
         EDIT  (B2,8(R2)),(7,7(R4)),0,COMMAS=YES                                
         AH    R7,=H'20'                                                        
         CH    R7,=H'40'                                                        
         BNH   CLE6                                                             
         GOTO1 REPORT                                                           
         SR    R7,R7                                                            
CLE6     LA    R4,P+20             RESET R4                                     
         B     CLE3                                                             
*                                                                               
CLE12    CLI   CLESW,1                                                          
         BNE   XIT                                                              
         CLC   P+20(6),SPACES                                                   
         BE    *+8                                                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*              PRINT RATE INFORMATION                                           
         SPACE 3                                                                
PRTRATE  NTR1                                                                   
         LA    R2,PUBREC+33                                                     
         BAS   RE,CLRPOOL                                                       
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   1(5,R8),=C'RATES'                                                
         MVC   111(5,R8),=20C'-'                                                
         XC    SVDATE,SVDATE                                                    
         SPACE 2                                                                
RT2      CLI   0(R2),0                                                          
         BNE   RT4                                                              
         CLI   20(R8),C' '                                                      
         BE    XIT                                                              
         BAS   RE,PRTPOOL                                                       
         B     XIT                                                              
         SPACE 2                                                                
RT4      CLI   0(R2),X'50'                                                      
         BNE   RT10                                                             
         USING PUBRATEL,R2                                                      
         CLI   20(R8),C' '                                                      
         BE    *+8                                                              
         BAS   RE,PRTPOOL                                                       
         LA    R7,60(R8)                                                        
         LA    R4,20(R8)                                                        
         CLC   SVDATE,PUBRSTRT     SEE IF SAME EFF DATE                         
         BE    RT4B                YES - SKIP PRINTING IT                       
         MVC   0(16,R4),=C'EFFECTIVE DATE ='                                    
         GOTO1 DATCON,DMCB,(3,PUBRSTRT),(5,17(R4))                              
         MVC   SVDATE,PUBRSTRT                                                  
         LA    R4,110(R4)                                                       
RT4B     CLI   PUBRSPCE,C' '                                                    
         BNH   RT5                                                              
         CLC   PUBRSPCE(2),=C'R='             IS IT RATE CODE                   
         BE    PRTCDS                                                           
*                                                                               
         LAY   R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
         LA    R0,PUBRSPCE                                                      
         ST    R0,PBYOINPT                                                      
         MVC   PBYOINPT(1),QMEDIA                                               
         MVI   PBYOCTL,X'80'       ONLY SPACE INPUT                             
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
         MVC   0(20,R4),PBYOSPC                                                 
         CLI   QMEDIA,C'N'         NEWS PAPERS                                  
         BNE   RT4F                                                             
*                                                                               
         LA    R3,PUBREC+33        FIND CLE FOR THIS SPACE                      
RT4C     CLI   0(R3),0                                                          
         BE    RT4F                                                             
         CLI   0(R3),X'40'                                                      
         BE    RT4E                                                             
RT4D     ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     RT4C                                                             
*                                                                               
RT4E     CLC   2(6,R3),PUBRSPCE                                                 
         BNE   RT4D                                                             
         EDIT  (B2,8(R3)),(6,9(R4)),0,BRACKET=YES                               
         MVI   15(R4),C'L'                                                      
         MVI   16(R4),C')'                                                      
         B     RT4F                                                             
*                                                                               
RT4F     DS    0H                                                               
         MVC   24(5,R4),=C'RATE='                                               
         EDIT  (P5,PUBRATE),(10,30(R4)),2,ALIGN=LEFT                            
         LA    R4,110(R4)                                                       
         CLC   PBYOSPC2,SPACES                                                  
         BNH   RT4H                                                             
         MVC   0(20,R4),PBYOSPC2                                                
         LA    R4,110(R4)                                                       
*                                                                               
RT4H     EQU   *                                                                
         LA    R7,40(R4)           SO X'51' ELEMS WIILL BE NEXT TO              
*                                  BASIC RATE                                   
         CLI   QMEDIA,C'M'         MAG RATES CAN HAVE DISCOUNTS                 
         BE    RT8A                                                             
         ZIC   R3,1(R2)            SEE IF NEXT RATE ELEM HAS SAME DATE          
         AR    R2,R3                                                            
         CLI   0(R2),X'50'                                                      
         BNE   RT2                                                              
         CLC   SVDATE,PUBRSTRT                                                  
         BE    RT4B                SKIP PRTPOOL                                 
         B     RT4                                                              
*                                                                               
PRTCDS   MVC   24(5,R4),=C'RATE='               MOVE TO PRINT LINE.             
         CLI   PUBRTYP,0                        IS TYPE: TOTAL                  
         BE    RT32                                                             
         EDIT  (P5,PUBRATE),(10,30(R4)),5,ALIGN=LEFT    LINES,INCHES            
         B     RT33                                                             
RT32     EDIT  (P5,PUBRATE),(10,30(R4)),2,ALIGN=LEFT    TOTAL                   
RT33     MVC   0(3,R4),PUBRSPCE+2            MOVE TO PRINT LINE, CODE.          
         MVC   7(12,R4),PUBRSPCE+5                               DESCR.         
         CLI   PUBRTYP,0                     IF TYPE TOTAL BRANCH TO            
         BE    RT35C                         NEXT 50 ELEMENT.                   
         LA    R6,30(R4)                                                        
RT34     CLI   0(R6),C' '                    FIND FIRST SPACE AFTER             
         BNH   RT35                          RATE IN PRINT LINE.                
         LA    R6,1(R6)                                                         
         B     RT34                                                             
RT35     TM    PUBRTYP,X'80'                 IF TYPE IS LINES MOVE IN           
         BNO   RT35A                         '/L' AFTER RATE.                   
         MVI   1(R6),C'L'                                                       
         B     RT35B                                                            
RT35A    TM    PUBRTYP,X'20'                 IF TYPE IS INCHES MOVE IN          
         BNO   RT35C                         '/I' AFTER RATE.                   
         MVI   1(R6),C'I'                                                       
RT35B    MVI   0(R6),C'/'                                                       
RT35C    LA    R4,110(R4)                    BUMP TO NEXT PRINT LINE.           
*                                                                               
RT36     EQU   *                                                                
         LA    R7,40(R4)           SO X'51' ELEMS WIILL BE NEXT TO              
*                                  BASIC RATE                                   
         CLI   QMEDIA,C'M'         MAG RATES CAN HAVE DISCOUNTS                 
         BE    RT8A                                                             
         ZIC   R3,1(R2)            SEE IF NEXT RATE ELEM HAS SAME DATE          
         AR    R2,R3                                                            
         CLI   0(R2),X'50'                                                      
         BNE   RT2                                                              
         CLC   SVDATE,PUBRSTRT                                                  
         BE    RT4B                SKIP PRTPOOL                                 
         B     RT4                                                              
*                                                                               
RT5      MVC   0(16,R4),=C'BASIC RATE     ='                                    
         EDIT  (P5,PUBRATE),(10,18(R4))                                         
         MVC   17(5,R4),18(R4)                                                  
         MVI   22(R4),C'.'                                                      
         MVC   29(8,R4),=C'PER INCH'                                            
         CLI   PUBRTYP,X'20'                                                    
         BE    RT6                                                              
         MVC   29(14,R4),=C'PER AGATE LINE'                                     
         CLI   PUBRTYP,X'40'                                                    
         BE    RT6                                                              
         MVC   29(14,R4),=CL14'PER LINE'    PUBRTYP X'80' OR X'00'              
         SPACE                                                                  
RT6      CLI   17(R4),C' '                                                      
         BNE   RT8                                                              
         MVC   17(30,R4),18(R4)                                                 
         B     RT6                                                              
         SPACE 2                                                                
RT8      LA    R4,110(R4)                                                       
RT8A     MVC   0(32,R4),=C'DISCOUNT TYPES =      RATE/TIMES'                    
         TM    PUBDRTYP,X'40'                                                   
         BZ    *+10                                                             
         MVC   17(09,R4),=C'PCT. DISC'                                          
         CLI   PUBDRTYP,0          NO DR TYP MEANS NO DISCOUNTS                 
         BNE   RT8C                                                             
         MVC   17(04,R4),=C'NONE'                                               
         MVC   21(11,R4),SPACES                                                 
         B     RT16                                                             
RT8C     EQU   *                                                                
         CLI   PUBDLTYP,C'X'                                                    
         BE    RT16                                                             
         MVC   27(5,R4),=C'LINES'                                               
         CLI   PUBDLTYP,C'L'                                                    
         BE    RT16                                                             
         MVC   27(6,R4),=C'INCHES'                                              
         CLI   PUBDLTYP,C'I'                                                    
         BE    RT16                                                             
         MVC   27(6,R4),=C'DOLLAR'                                              
         CLI   PUBDLTYP,C'$'                                                    
         BE    RT16                                                             
         MVC   27(6,R4),=C'PAGES '                                              
         B     RT16                                                             
         SPACE 2                                                                
RT10     CLI   0(R2),X'51'                                                      
         BNE   RT16                                                             
         USING PUBDSCEL,R2                                                      
         MVC   0(21,R7),=C'DISCOUNT LEVEL/RATE ='                               
         EDIT  (P5,PUBDSCLV),(9,22(R7)),COMMAS=YES                              
         LA    R4,31(R7)                                                        
         EDIT  (P5,PUBDSCRT),(9,2(R4))                                          
         MVI   0(R4),C'/'                                                       
         MVC   1(4,R4),2(R4)                                                    
         MVI   5(R4),C'.'          PUT IN DEC POINT                             
         OC    6(5,R4),=5C'0'                                                   
         SPACE 2                                                                
RT12     CLI   1(R4),C' '                                                       
         BNE   RT14                                                             
         MVC   1(10,R4),2(R4)      LEFT ALIGN                                   
         B     RT12                                                             
         SPACE 2                                                                
RT14     LA    R7,110(R7)                                                       
         SPACE 2                                                                
RT16     ZIC   R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     RT2                                                              
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
         EJECT                                                                  
*              PRINT TAX                                                        
PRTTAX   NTR1                                                                   
         LA    R2,PUBREC+33                                                     
         BAS   RE,CLRPOOL                                                       
TAX1     CLI   0(R2),X'22'                                                      
         BE    TAX5                                                             
         CLI   0(R2),0             END OF REC                                   
         BE    TAXXIT                                                           
         ZIC   R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     TAX1                                                             
*                                                                               
TAX5     MVC   1(9,R8),=C'TAX RATES'                                            
         MVC   111(9,R8),=20C'-'                                                
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         LR    R7,R8                                                            
*                                                                               
         USING PUBTAXEL,R2                                                      
         LA    R5,PUBTAX1                                                       
         LA    R6,3                                                             
TAX8     LA    R3,44(R7)                                                        
         LA    R4,20(R7)                                                        
         CLI   3(R5),0                                                          
         BE    TAX20                                                            
*                                                                               
TAX10    MVC   0(6,R4),=C'RATE= '                                               
         EDIT  (3,0(R5)),(7,6(R4)),4                                            
         MVC   0(17,R3),=C'EFFECTIVE DATE = '                                   
         GOTO1 DATCON,DMCB,(3,3(R5)),(5,17(R3))                                 
*                                                                               
         LA    R7,110(R7)                                                       
         LA    R4,20(R4)                                                        
         LA    R3,44(R3)                                                        
         LA    R5,6(R5)                                                         
         BCT   R6,TAX8                                                          
*                                                                               
TAX20    CLI   20(R8),C' '                                                      
         BE    TAXXIT                                                           
         BAS   RE,PRTPOOL                                                       
*                                                                               
TAXXIT   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              PRINT PREMIUMS                                                   
         SPACE 3                                                                
PRTPREM  NTR1                                                                   
         LA    R2,PUBREC+33                                                     
         BAS   RE,CLRPOOL                                                       
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   1(8,R8),=C'PREMIUMS'                                             
         MVC   111(8,R8),=20C'-'                                                
         SPACE 2                                                                
PM2      CLI   0(R2),0                                                          
         BNE   PM4                                                              
         CLI   20(R8),C' '                                                      
         BE    XIT                                                              
         BAS   RE,PRTPOOL                                                       
         B     XIT                                                              
         SPACE 2                                                                
PM4      CLI   0(R2),X'60'                                                      
         BNE   PM8                                                              
         USING PUBPRMEL,R2                                                      
         CLI   20(R8),C' '                                                      
         BE    *+8                                                              
         BAS   RE,PRTPOOL                                                       
*NOP*    LA    R7,60(R8)                                                        
         LA    R6,20(R8)                                                        
*NEW 3/3/00                                                                     
         MVC   0(13,R6),=C'CLIENT CODE ='                                       
         MVC   14(3,R6),=C'ALL'                                                 
         CLI   PUBPCLT,C' '                                                     
         BNH   *+10                                                             
         MVC   14(3,R6),PUBPCLT                                                 
         LA    R6,110(R6)                                                       
         LA    R7,40(R6)     POINT R7 TO LINE UP WITH "EFFECTIVE DATE"          
*NEW 3/3/00                                                                     
         MVC   0(16,R6),=C'EFFECTIVE DATE ='                                    
         GOTO1 DATCON,DMCB,(3,PUBPSTRT),(5,17(R6))                              
         MVC   27(3,R6),PUBPCOD                                                 
         LA    R6,110(R6)                                                       
         MVC   0(16,R6),=C'AVAILABLE DAYS ='                                    
         LA    R5,17(R6)                                                        
         LA    R4,PUBPDAYS                                                      
         BAS   RE,DAYEDIT                                                       
         LA    R6,110(R6)                                                       
         MVC   0(21,R6),=C'MINIMUM SIZE/CHARGE ='                               
         LA    R0,4                                                             
         MVC   22(4,R6),=C'PAGE'                                                
         CLC   PUBPMINS,=C'PAGE'                                                
         BE    PM4A                                                             
         MVC   22(4,R6),=C'NONE'                                                
         CP    PUBPMINS,=P'0'                                                   
         BE    PM4A                                                             
         EDIT  (P4,PUBPMINS),(8,22(R6)),ALIGN=LEFT                              
PM4A     EQU   *                                                                
         LA    R4,22(R6)                                                        
         AR    R4,R0                                                            
         MVI   0(R4),C'/'                                                       
         MVC   1(4,R4),=C'NONE'                                                 
         CP    PUBPMINC,=P'0'                                                   
         BE    PM4B                                                             
         EDIT  (P5,PUBPMINC),(10,1(R4)),ALIGN=LEFT                              
PM4B     EQU   *                                                                
         LA    R6,110(R6)                                                       
         MVC   0(21,R6),=C'TYPE OF CHARGE = FLAT'                               
*        B     PM5                                                              
*MYBYTE   DS    CL1                                                             
***                                                                             
*M5      DS    0H                                                               
*        MVC   MYBYTE,PUBPTYPC                                                  
***                                                                             
         TM    PUBPTYPC,X'80'                                                   
         BO    PM6                                                              
         MVC   17(10,R6),=C'B/W + FLAT'                                         
         TM    PUBPTYPC,X'40'                                                   
         BO    PM6                                                              
         MVC   23(4,R6),=C'PCT.'                                                
         TM    PUBPTYPC,X'20'                                                   
         BO    PM6                                                              
         MVC   23(4,R6),=C'LINE'                                                
         SPACE 2                                                                
PM6      LA    R6,110(R6)                                                       
         MVC   0(19,R6),=C'CLOSING MONTH/DAY ='                                 
         LA    R4,PUBPCLMO                                                      
         LA    R5,20(R6)                                                        
         BAS   RE,MDEDIT                                                        
         B     PM14                                                             
         SPACE 2                                                                
PM8      CLI   0(R2),X'61'                                                      
         BNE   PM14                                                             
         USING PUBPTBEL,R2                                                      
         MVC   0(22,R7),=C'PREMIUM LINEAGE/CHARGE'                              
         CP    PUBPTBLN,=P'0'                                                   
         BNE   PM9                                                              
         MVC   23(10,R7),=C'SPOT COLOR'                                         
         LA    R4,33(R7)                                                        
         B     PM9A                                                             
         SPACE 2                                                                
PM9      DS    0H                                                               
         EDIT  (P5,PUBPTBLN),(10,23(R7))                                        
         CLC   24(9,R7),=C'999999999'                                           
         BNE   *+10                                                             
         MVC   24(9,R7),=C'UNLIMITED'                                           
         LA    R4,33(R7)                                                        
         SPACE 2                                                                
PM9A     DS    0H                                                               
         MVI   0(R4),C'/'                                                       
         CLI   PUBPTBCH,X'00'                                                   
*        EDIT  (P5,PUBPTBCH),(10,1(R4)),5,ALIGN=LEFT,DROP=3                     
*        TM    MYBYTE,B'11000000'                                               
*        BZ    PM9B                                                             
*        XC    DUB,DUB                                                          
*        MVC   DUB+2(6),PUBPTBCH                                                
*        DP    DUB,=P'1000'                                                     
*        EDIT  (P5,DUB),(10,8(R5)),2,ALIGN=LEFT                                 
*        B     PM12                                                             
*M9B     DS    0H                                                               
         EDIT  (P6,PUBPTBCH),(12,1(R4)),5,ALIGN=LEFT,DROP=3                     
         SPACE 2                                                                
PM12     LA    R7,110(R7)                                                       
         SPACE 2                                                                
PM14     IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     PM2                                                              
         EJECT                                                                  
*              PRINT REP DETAILS                                                
         SPACE 3                                                                
PRTREPS  NTR1                                                                   
         LA    R2,PUBREC+33                                                     
         BAS   RE,CLRPOOL                                                       
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   1(15,R8),=C'REPRESENTATIVES'                                     
         MVC   111(15,R8),=20C'-'                                               
         LR    R7,R8                                                            
         SPACE 2                                                                
PREP2    CLI   0(R2),0                                                          
         BNE   PREP4                                                            
         CLI   20(R8),C' '                                                      
         BE    XIT                                                              
         BAS   RE,PRTPOOL                                                       
         B     XIT                                                              
         SPACE 2                                                                
PREP4    CLI   0(R2),X'14'                                                      
         BNE   PREP6                                                            
         USING PUBREPEL,R2                                                      
         MVC   20(22,R7),=C'APPLIES TO ALL CLIENTS'                             
         MVC   60(13,R7),=C'TRAFFIC REP ='                                      
         MVC   74(4,R7),PUBTRREP                                                
         MVC   130(12,R7),=C'PAYING REP ='                                      
         MVC   143(4,R7),PUBPAREP                                               
         MVC   170(14,R7),=C'CONTRACT REP ='                                    
         MVC   185(4,R7),PUBCNREP                                               
*                                                                               
         CLI   PUBCVEN,C' '                                                     
         BNH   PREP5                                                            
         MVC   191(9,R7),=C'VEN NO. ='                                          
         MVC   201(12,R7),PUBCVEN                                               
*                                                                               
PREP5    DS    0H                                                               
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    PREP5A                                                           
         MVC   31(11,R7),=C'CLIENT XYZ '                                        
         MVC   38(3,R7),PUBRPOFF                                                
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   PREP5A                                                           
         MVC   31(11,R7),=C'OFFICE X   '                                        
******   MVC   38(2,R7),PUBRPOFF+1                                              
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,PUBRPOFF+1,38(R7),VOFFICER,QAGENCY,       X        
               VCOMFACS                                                         
*                                                                               
***** NEW REP CODE BEGINS HERE *********************************                
*                                                                               
PREP5A   LA    R7,220(R7)          BUMP TO NEXT LINE                            
         CLI   1(R2),X'2D'         IF ELEMENT HAS < 45 BYTES                    
         BL    PREP5X                 NO MORE TO PRINT FOR ELEMENT              
*                                                                               
         CLI   PUBCSCC1,0                                                       
         BE    PREP5B              NO STND COMM 1                               
         MVC   20(30,R7),=C'STANDARD COMMENT FOR INSERTION'                     
         TM    PUBCSCC1,X'C0'                                                   
         BNO   PREP5A2                                                          
         MVC   51(23,R7),=C'ORDERS AND CONTRACTS IS'                            
         MVC   75(6,R7),PUBCSC1    STND COMM 1                                  
         B     PREP5A9             END COMMENT 1                                
PREP5A2  TM    PUBCSCC1,X'80'                                                   
         BNO   PREP5A4             MUST BE X'40'                                
         MVC   51(9,R7),=C'ORDERS IS'                                           
         MVC   61(6,R7),PUBCSC1    STND COMM 1                                  
         B     PREP5A9             END COMMENT 1                                
PREP5A4  MVC   41(12,R7),=C'CONTRACTS IS'                                       
         MVC   54(6,R7),PUBCSC1    STND COMM 1                                  
PREP5A9  LA    R7,110(R7)          BUMP TO NEXT LINE                            
*                                                                               
PREP5B   CLI   PUBCSCC2,0                                                       
         BE    PREP5C              NO STND COMM 2                               
         MVC   20(30,R7),=C'STANDARD COMMENT FOR INSERTION'                     
         TM    PUBCSCC2,X'C0'                                                   
         BNO   PREP5B2                                                          
         MVC   51(23,R7),=C'ORDERS AND CONTRACTS IS'                            
         MVC   75(6,R7),PUBCSC2    STND COMM 2                                  
         B     PREP5B9             END COMMENT 2                                
PREP5B2  TM    PUBCSCC2,X'80'                                                   
         BNO   PREP5B4             MUST BE X'40'                                
         MVC   51(9,R7),=C'ORDERS IS'                                           
         MVC   61(6,R7),PUBCSC2    STND COMM 2                                  
         B     PREP5B9             END COMMENT 2                                
PREP5B4  MVC   41(12,R7),=C'CONTRACTS IS'                                       
         MVC   54(6,R7),PUBCSC2    STND COMM 2                                  
PREP5B9  LA    R7,110(R7)          BUMP TO NEXT LINE                            
*                                                                               
PREP5C   TM    PUBCCTL,X'01'                                                    
         BNO   PREP5D              GO TEST PAY CONTROL                          
         MVC   20(31,R7),=C'PAY ONLY IF CASH RECEIVED - YES'                    
         LA    R7,110(R7)          BUMP TO NEXT LINE                            
*                                                                               
PREP5D   CLI   1(R2),X'35'                                                      
         BL    PREP5X              ELEMENT HAS < 53 BYTES                       
         CLI   PUBPCTL1,C'Y'                                                    
         BNE   PREP5E                                                           
         MVC   20(36,R7),=C'PAY ONLY IF TEARSHEET APPROVED - YES'               
         LA    R7,110(R7)          BUMP TO NEXT LINE                            
*                                                                               
PREP5E   CLI   PUBPCTL2,C'Y'                                                    
         BE    PREP5E2                                                          
         CLI   PUBPCTL2,C'O'                                                    
         BNE   PREP5G              $PAY - PAY CONTROL NOT Y OR O                
PREP5E2  MVC   20(13,R7),=C'PAY CONTROL  '                                      
         MVC   35(1,R7),PUBPCTL2                                                
         MVC   38(21,R7),=C'PAY ONLY MATCHED BUYS'                              
         CLI   PUBPCTL2,C'O'                                                    
         BE    PREP5E9                                                          
         MVC   38(24,R7),=C'ALL BUYS MUST BE MATCHED'                           
PREP5E9  LA    R7,110(R7)          BUMP TO NEXT LINE                            
*                                                                               
PREP5G   CLI   PUBPCTL3,C'I'                                                    
         BE    PREP5G2                                                          
         CLI   PUBPCTL3,C'O'                                                    
         BE    PREP5G2                                                          
         CLI   PUBPCTL3,C'Y'                                                    
         BNE   PREP5X              $MAT - PAY CONTROL NOT I OR O OR Y           
PREP5G2  MVC   20(13,R7),=C'PAY VIA MATCH'                                      
         MVC   35(1,R7),PUBPCTL3                                                
         MVC   38(14,R7),=C'PAY BY INVOICE'                                     
         CLI   PUBPCTL3,C'I'                                                    
         BE    PREP5G9                                                          
         MVC   38(21,R7),=C'PAY ONLY MATCHED BUYS'                              
         CLI   PUBPCTL3,C'O'                                                    
         BE    PREP5G9                                                          
         MVC   38(24,R7),=C'ALL BUYS MUST BE MATCHED'                           
PREP5G9  LA    R7,110(R7)          BUMP TO NEXT LINE                            
*                                                                               
PREP5X   DS    0H                                                               
         MVI   20(R7),0            SO PRTPOOL WILL SKIP A LINE                  
         LA    R7,110(R7)          BUMP TO NEXT LINE                            
         SPACE 2                                                                
PREP6    IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     PREP2                                                            
         EJECT                                                                  
*              PRINT CLIENT DISTRICT DETAILS                                    
         SPACE 3                                                                
PRTDIST  NTR1                                                                   
         L     R2,ALTLREC                                                       
         LA    R2,33(R2)                                                        
         BAS   RE,CLRPOOL                                                       
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   1(16,R8),=C'CLIENT DISTRICTS'                                    
         MVC   111(16,R8),=20C'-'                                               
         LR    R7,R8                                                            
         MVC   20(22,R7),=C'CLI/DIV/REG/DST/ SHARE'                             
         MVC   80(22,R7),=C'CLI/DIV/REG/DST/ SHARE'                             
         LA    R7,110(R7)                                                       
         SPACE 2                                                                
PD2      CLI   0(R2),0                                                          
         BNE   PD4                                                              
         CLI   130(R8),C' '                                                     
         BE    XIT                                                              
         BAS   RE,PRTPOOL                                                       
         B     XIT                                                              
         SPACE 2                                                                
PD4      CLI   0(R2),X'71'                                                      
         BNE   PD8                                                              
         USING PUBDSTEL,R2                                                      
         LA    R6,20(R7)                                                        
         CLI   0(R6),C' '                                                       
         BE    PD6                                                              
         LA    R6,80(R7)                                                        
         CLI   0(R6),C' '                                                       
         BE    PD6                                                              
         LA    R7,110(R7)                                                       
         LA    R6,20(R7)                                                        
         SPACE 2                                                                
PD6      MVC   0(15,R6),=C'CCC/DDD/RRR/DDD/'                                    
         MVC   00(3,R6),PUBDCLT                                                 
         MVC   04(3,R6),PUBDDIV                                                 
         MVC   08(3,R6),PUBDREG                                                 
         MVC   12(3,R6),PUBDDST                                                 
         OC    PUBDSHR(2),PUBDSHR                                               
         BZ    PD8                                                              
         MVI   15(R6),C'/'                                                      
         EDIT  (B2,PUBDSHR),(6,16(R6)),2                                        
*                                                                               
         SPACE 2                                                                
PD8      IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     PD2                                                              
         EJECT                                                                  
PRTPADR  NTR1                                                                   
         BAS   RE,CLRPOOL                                                       
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   1(16,R8),=C'PAYING ADDRESSES'                                    
         MVC   111(16,R8),=20C'-'                                               
         MVI   BYTE,X'08'                                                       
         B     ADDR                                                             
*                                                                               
PRTTADR  NTR1                                                                   
         BAS   RE,CLRPOOL                                                       
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   1(17,R8),=C'TRAFFIC ADDRESSES'                                   
         MVC   111(17,R8),=20C'-'                                               
         MVI   BYTE,X'09'                                                       
         B     ADDR                                                             
*                                                                               
PRTCADR  NTR1                                                                   
         BAS   RE,CLRPOOL                                                       
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   1(18,R8),=C'CONTRACT ADDRESSES'                                  
         MVC   111(18,R8),=20C'-'                                               
         MVI   BYTE,X'0A'                                                       
         B     ADDR                                                             
         SPACE 2                                                                
PRTSADR  NTR1                                                                   
         BAS   RE,CLRPOOL                                                       
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   1(18,R8),=C'SHIPPING ADDRESSES'                                  
         MVC   111(18,R8),=20C'-'                                               
         MVI   BYTE,X'0B'                                                       
         B     ADDR                                                             
         SPACE 2                                                                
ADDR     LR    R7,R8               FOR PRINT POSITIONING                        
         MVC   FULL,AREC           SAVE PPG'S AREC                              
         MVI   ADDRSW,C' '         RECORD OR ELEMENT ADDRESS SWITCH             
         XC    KEY,KEY                                                          
         MVC   KEY(PUBKCOD-PUBKEY),PUBREC                                       
         MVI   KEY+PUBKCOD-PUBKEY,X'82'      PUB ADDRESS RECORD CODE            
         MVC   KEY+10(1),BYTE                ADDRESS TYPE                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PUBDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(11),KEYSAVE     MED/PUB(6)/AGY/COD/TYP                       
         BE    ADDR3               ADDRESS RECORD FOUND                         
         LA    R2,PUBREC+33        NOT FOUND                                    
         B     ADDR60              TRY FOR ADDRESS ELEMENT                      
ADDR2    GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'PUBDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(11),KEYSAVE     MED/PUB(6)/AGY/COD/TYP                       
         BE    ADDR3                                                            
ADDR2D   CLI   20(R8),C' '                                                      
         BE    ADDR90              DONE WITH THIS ADDRESS TYPE                  
         BAS   RE,PRTPOOL                                                       
         B     ADDR90                                                           
*                                                                               
ADDR3    DS    0H                  GET RECORD TO PRINT ADDRESS                  
         L     R6,PPFILEC                                                       
         USING PPFILED,R6                                                       
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'PUBFILE',KEY+27,PPRDREC, X        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ADDRSW,C'R'         I HAVE AN ADDRESS RECORD                     
         LA    R2,PPRDREC+33       PROD REC AREA USED FOR I/O                   
*                                                                               
ADDR4    DS    0H                                                               
         USING PUBAOVEL,R2                                                      
         MVC   20(22,R7),=C'APPLIES TO ALL CLIENTS'                             
         MVC   60(30,R7),PUBAONAM                                               
         MVC   170(30,R7),PUBAOLN1                                              
         MVC   280(30,R7),PUBAOLN2                                              
         MVC   390(5,R7),=C'ATTN-'                                              
         MVC   396(20,R7),PUBAOATN                                              
         MVC   418(6,R7),=C'PHONE-'                                             
         MVC   424(12,R7),PUBAOTEL                                              
*                                                                               
         OC    PUBAOFAX,PUBAOFAX                                                
         BZ    ADDR45              DONE WITH FAX, CHK E-MAIL                    
         CLC   PUBAOFAX,SPACES     IS THERE REALLY A FAX ENTRY ?                
         BNH   ADDR45              NO                                           
         MVC   500(4,R7),=C'FAX-'                                               
         MVC   506(12,R7),PUBAOFAX                                              
         CLC   =C'FX=',PUBAOFAX                                                 
         BNE   ADDR45                                                           
*                               LOOK FOR FAX NUMBER IN CONTROL FILE             
         XC    500(18,R7),500(R7)                                               
         MVC   FRSVFAX,PUBAOFAX                                                 
*                                                                               
******   MVC   ADRKEYS,KEY         SAVE PUB ADDRESS RECORD KEYS                 
         MVC   WORK(64),KEY        SAVE PUB ADDRESS RECORD KEYS                 
         BAS   RE,GETFAX                                                        
******   MVC   KEY(64),ADRKEYS     RESTORE PUB ADDRESS RECORD KEYS AND          
         MVC   KEY(64),WORK        RESTORE PUB ADDRESS RECORD KEYS AND          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PUBDIR',KEY,KEY      SEQ          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   500(4,R7),=C'FAX-'                                               
         MVC   506(10,R7),PUBAOFAX                                              
         MVI   517(R7),C'('                                                     
         MVC   518(16,R7),TOFAX                                                 
         MVI   534(R7),C')'                                                     
*                                                                               
*        **  E-MAIL ADDRESS TESTING AND MANIPULATION  **                        
*                                                                               
ADDR45   DS    0H     (IF LONGER THAN 44 CHARACTERS MUST USE 2 LINES)           
*                                                                               
         OC    PUBAOEAD,PUBAOEAD                                                
         BZ    ADDR50              NO EMAIL - DONE WITH ELEMENT                 
         LA    RE,PUBAOEAD         BEGINNING OF E-MAIL ADDRESS                  
         LA    RF,PUBAOEAD+59      END       OF E-MAIL ADDRESS                  
ADDR45B  CLI   0(RF),C' '          BLANK ?                                      
         BH    *+10                NO                                           
         BCTR  RF,0                MOVE "LEFT" 1                                
         B     ADDR45B             TEST NEXT POSITION                           
         SR    RF,RE                                                            
*NOP*    BP    *+6                                                              
         BNM   *+6                                                              
         DC    H'0'                MUST BE POSITIVE VALUE                       
         LA    RF,1(RF)            ACTUAL LENGTH OF E-MAIL ADDRESS              
*                                                                               
         CLC   500(2,R7),SPACES    IS THERE A FAX LINE ?                        
         BH    ADDR45B2            YES                                          
         MVC   500(6,R7),=C'EMAIL-'                                             
         CHI   RF,44               ADDRESS LONGER THAN 44 CHARACTERS ?          
         BH    ADDR45L             YES - USE TWO LINES                          
         MVC   506(44,R7),PUBAOEAD NO - ONE LINE IS ENOUGH                      
         B     ADDR50              FINISH ELEMENT                               
*                                                                               
ADDR45B2 DS    0H                                                               
         MVC   610(6,R7),=C'EMAIL-'                                             
         CHI   RF,44               ADDRESS LONGER THAN 44 CHARACTERS ?          
         BH    ADDR45L             YES - USE TWO LINES                          
         MVC   616(44,R7),PUBAOEAD NO - ONE LINE IS ENOUGH                      
         B     ADDR50              FINISH ELEMENT                               
*                                                                               
ADDR45L  DS    0H                                                               
         MVC   MID1,SPACES         USE MID1 AND MID2 FOR WORK AREAS             
         MVC   MID2,SPACES                                                      
         LA    R1,44               MAX # OF CHARACTERS TO TEST                  
         LA    RE,PUBAOEAD+43      START TESTING HERE                           
ADDR45L2 CLI   0(RE),C'.'          "SPLIT" CHARACTER ?                          
         BE    ADDR45L6            YES                                          
         CLI   0(RE),C'/'          "SPLIT" CHARACTER ?                          
         BE    ADDR45L6            YES                                          
         CLI   0(RE),C'@'          "SPLIT" CHARACTER ?                          
         BE    ADDR45L6            YES                                          
         CLI   0(RE),C'_'          "SPLIT" CHARACTER ?                          
         BE    ADDR45L6            YES                                          
         CLI   0(RE),C'-'          "SPLIT" CHARACTER ?                          
         BE    ADDR45L6            YES                                          
         BCTR  RE,0                MOVE TO "LEFT"                               
         BCT   R1,ADDR45L2         TEST NEXT CHARACTER                          
*                                                                               
ADDR45L4 DS    0H         SPLIT IN MIDDLE (NO "SPLIT" CHARACTER FOUND)          
         MVC   MID1(30),PUBAOEAD                                                
         MVC   MID2(30),PUBAOEAD+30                                             
         B     ADDR45X             GO PUT IN OUTPUT BLOCK                       
*                                                                               
ADDR45L6 DS    0H                  SPLIT THE ENTRY                              
         SR    RF,R1       RF=TOTAL ENTRY LENGTH  R1="1ST" LINE LENGTH          
         CHI   RF,45               2ND LINE LENGTH GREATER THAN 44 ?            
         BNL   ADDR45L4            YES - TOO LONG -SPLIT IN MIDDLE              
         BCTR  R1,0                PREP FOR 1ST LINE                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MID1(0),PUBAOEAD                                                 
         BCTR  RF,0                PREP FOR 2ND LINE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MID2(0),1(RE)       RE AT SPLIT CHARACTER IN ENTRY               
ADDR45X  DS    0H                  SET THE OUTPUT BLOCK                         
         CLC   500(4,R7),=C'FAX-'  IS THERE A FAX LINE ?                        
         BE    ADDR45X4            YES                                          
         MVC   506(44,R7),MID1                                                  
         MVC   616(44,R7),MID2                                                  
         B     ADDR50              FINISH ELEMENT                               
ADDR45X4 MVC   616(44,R7),MID1                                                  
         MVC   726(44,R7),MID2                                                  
*                                                                               
ADDR50   DS    0H                                                               
         CLC   PUBAOFF,=3X'FF'                                                  
         BE    ADDR50P                                                          
         MVC   31(11,R7),=C'CLIENT XXX'                                         
         MVC   38(3,R7),PUBAOFF                                                 
         CLI   PUBAOFF,X'FF'                                                    
         BNE   ADDR50P                                                          
         MVC   31(11,R7),=C'OFFICE X   '                                        
******   MVC   38(2,R7),PUBAOFF+1                                               
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,PUBAOFF+1,38(R7),VOFFICER,QAGENCY,        X        
               VCOMFACS                                                         
*                                                                               
ADDR50P  DS    0H                                                               
         CLC   726(2,R7),SPACES    IS THERE AN EMAIL LINE 2 ?                   
         BH    ADDR50P2            YES                                          
         CLC   616(2,R7),SPACES    IS THERE AN EMAIL LINE 2 ?                   
         BH    ADDR50P4            YES                                          
         CLC   610(2,R7),SPACES          OR AN EMAIL LINE 1 ?                   
         BH    ADDR50P4            YES                                          
         CLC   500(2,R7),SPACES    IS THERE AN EMAIL OR A FAX LINE ?            
         BH    ADDR50P6            YES                                          
*                                                                               
         MVI   460(R7),0           SO PRTPOOL WILL SKIP A LINE                  
         LA    R7,550(R7)                                                       
         B     ADDR50PX                                                         
ADDR50P2 MVI   790(R7),0           SO PRTPOOL WILL SKIP A LINE                  
         LA    R7,880(R7)                                                       
         B     ADDR50PX                                                         
ADDR50P4 MVI   680(R7),0           SO PRTPOOL WILL SKIP A LINE                  
         LA    R7,770(R7)                                                       
         B     ADDR50PX                                                         
ADDR50P6 MVI   570(R7),0           SO PRTPOOL WILL SKIP A LINE                  
         LA    R7,660(R7)                                                       
*****    B     ADDR50PX                                                         
ADDR50PX CLI   ADDRSW,C'R'         USING RECORD ADDRESS ?                       
         BE    ADDR2               YES - NEXT RECORD                            
         B     ADDR60B             NO  - NEXT ELEMENT                           
*                                                                               
ADDR60   DS    0H                  ELEMENT SEARCHING                            
         CLI   0(R2),0             END OF RECORD ?                              
         BE    ADDR2D              YES - FINISH THIS ADDRESS TYPE               
         CLC   0(1,R2),BYTE        ADDRESS ELEMENT ?                            
         BE    ADDR4               YES - GO PRINT IT                            
ADDR60B  ZIC   RE,1(R2)            NEXT ELEMENT                                 
         AR    R2,RE                                                            
         B     ADDR60                                                           
*                                                                               
ADDR90   DS    0H                                                               
         MVC   AREC,FULL           RESTORE PPG'S AREC                           
         MVC   KEY(64),PPGKEY      RESTORE PPG'S KEYS                           
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
PRTCOM  NTR1                                                                    
         MVI   COMSW,0                                                          
         LA    R2,PUBREC+33                                                     
*                                                                               
COM2     CLI   0(R2),X'66'                                                      
         BE    COM4                                                             
         CLI   0(R2),0                                                          
         BE    COM12                                                            
COM3     ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     COM2                                                             
*                                                                               
COM4     CLI   1(R2),2                                                          
         BE    COM3                NO ZERO LENGHT COMMENTS                      
         CLI   COMSW,2             SEE IF TITLE + UNDERLINE PRINTED             
         BE    COM7                YES                                          
         CLI   COMSW,0                                                          
         BNE   COM6                                                             
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   P+1(7),=C'COMMENT'      DO TITLE                                 
         MVI   COMSW,1                                                          
         B     COM7                                                             
*                                                                               
COM6     MVC   P+1(7),=20C'-'          DO UNDERLINE                             
         MVI   COMSW,2                                                          
*                                                                               
COM7     ZIC   R3,1(R2)                                                         
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+20(0),2(R2)                                                    
         GOTO1 REPORT                                                           
         B     COM3                                                             
*                                                                               
COM12    CLI   COMSW,0                                                          
         BE    COM13               NO COMMENTS                                  
         CLI   COMSW,2                                                          
         BE    COM13                                                            
         MVC   P+1(7),=20C'-'          DO UNDERLINE                             
         GOTO1 REPORT                                                           
*                                                                               
COM13    XIT                                                                    
         EJECT                                                                  
*              SUBSIDIARY EDITTING AIDS                                         
         SPACE 2                                                                
DAYEDIT  NTR                                                                    
         CLI   0(R4),X'7F'                                                      
         BNE   DAYEDIT1                                                         
         MVC   0(3,R5),=C'ALL'                                                  
         B     XIT                                                              
DAYEDIT1 EQU   *                                                                
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         SRDL  R0,7                                                             
         LA    R2,DAYTABL                                                       
         LA    R3,7                                                             
         SPACE 2                                                                
DAYEDIT2 SR    R0,R0                                                            
         SLDL  R0,1                                                             
         LTR   R0,R0                                                            
         BZ    DAYEDIT4                                                         
         MVC   0(3,R5),0(R2)                                                    
         LA    R5,4(R5)                                                         
         SPACE 2                                                                
DAYEDIT4 LA    R2,3(R2)                                                         
         BCT   R3,DAYEDIT2                                                      
         B     XIT                                                              
         SPACE 2                                                                
MDEDIT   NTR1                                                                   
         CP    0(2,R4),=P'0'                                                    
         BNZ   MDEDIT2                                                          
         MVI   0(R5),C'0'                                                       
         LA    R0,1                                                             
         B     MDEDIT3                                                          
MDEDIT2  EQU   *                                                                
         EDIT  (P2,0(R4)),(3,0(R5)),ALIGN=LEFT,FLOAT=-                          
MDEDIT3  EQU   *                                                                
         AR    R5,R0                                                            
         MVI   0(R5),C'/'                                                       
         TM    2(R4),X'F0'         IS IT A PLUS NUMBER                          
         BZ    MDEDIT3A                                                         
         NI    2(R4),X'0F'                                                      
         EDIT  (P2,2(R4)),(3,1(R5)),ALIGN=LEFT,FLOAT=+                          
         B     XIT                                                              
MDEDIT3A CP    2(2,R4),=P'0'                                                    
         BNZ   MDEDIT4                                                          
         MVI   1(R5),C'0'                                                       
         B     XIT                                                              
MDEDIT4  EQU   *                                                                
         EDIT  (P2,2(R4)),(3,1(R5)),ALIGN=LEFT,FLOAT=-                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*             *** ROUTINE TO GET FAX NUMBER FROM CONTROL FILE                   
*                                                                               
GETFAX   NTR1                                                                   
         MVC   TOFAX,SPACES                                                     
         OC    FRSVFAX,SPACES                                                   
GETFAXB  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,QAGENCY                                                  
         MVC   CTFXCODE,FRSVFAX+3                                               
         MVC   KEYSAVE,KEY                                                      
         L     R6,PPFILEC          TO ADDRESS PCONREC                           
         USING PPFILED,R6                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEY,PCONREC                
         CLC   PCONREC(18),KEYSAVE      COMPARE 7-BYTE FAX CODE                 
         BNE   GETFAXN                                                          
         LA    R4,PCONREC      PCONREC BEING USED FOR CONTROL FILE I/O          
         LA    R7,CTFXEL1                                                       
         B     GETFX4                                                           
         SPACE 1                                                                
GETFX2   ZIC   R1,1(R7)                                                         
         AR    R7,R1                                                            
         SPACE 1                                                                
GETFX4   CLI   0(R7),0                                                          
         BE    GETFAXN                                                          
         CLI   0(R7),CTFX1ELQ                                                   
         BE    GETFXNO                                                          
         B     GETFX2                                                           
         SPACE 1                                                                
         USING CTFX1EL,R7                                                       
GETFXNO  ZIC   R1,CTFX1LEN         FAX NUMBER                                   
         SH    R1,=H'3'                                                         
         CH    R1,=H'24'                                                        
         BL    *+8                                                              
         LA    R1,24                                                            
         EX    R1,*+8                                                           
*****    B     GETFX2                                                           
         B     GETFAXX                                                          
         MVC   TOFAX(0),CTFX1NUM                                                
         SPACE 1                                                                
*                                                                               
GETFAXN  MVC   TOFAX,=C'** NOT FOUND ** '                                       
*                                                                               
GETFAXX  MVC   KEY(64),PPGKEY          RESTORE PPGKEYS                          
         XIT1                                                                   
         DROP  R4,R6,R7                                                         
         EJECT                                                                  
*                                                                               
*              ROUTINES TO CLEAR AND PRINT POOL                                 
         SPACE 3                                                                
CLRPOOL  NTR1                                                                   
         LA    R2,MAXLN+1       WAS 401                                         
         SPACE 2                                                                
CLRPOOL2 MVC   0(110,R8),SPACES                                                 
         LA    R8,110(R8)                                                       
         BCT   R2,CLRPOOL2                                                      
         B     XIT                                                              
         SPACE 2                                                                
PRTPOOL  NTR1                                                                   
         LA    R2,MAXLN         WAS 400                                         
         SPACE 2                                                                
PRTPL2   CLC   0(110,R8),SPACES                                                 
         BE    XIT                                                              
         CLC   110(110,R8),SPACES                                               
         BNE   PRTPL5                                                           
         CLI   20(R8),0            USED BY REPS AND ADDRS TO SKIP               
         BE    PRTPL5              YES - DON'T DOUBLE SPACE                     
         MVI   SPACING,2                                                        
PRTPL5   MVC   P(110),0(R8)                                                     
         MVC   0(110,R8),SPACES                                                 
         GOTO1 REPORT                                                           
         LA    R8,110(R8)                                                       
         BCT   R2,PRTPL2                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
STPUB    DC    6X'0'                                                            
SVDATE   DS    CL3                                                              
CIRCSW   DS    CL1                                                              
COMSW    DS    CL1                                                              
CLESW    DS    CL1                                                              
GRPSW    DS    CL1                                                              
ADDRSW   DS    CL1                 ADDRESS ELEMENT OR RECORD SWITCH             
DUBLWD   DS    D                                                                
APSTVAL  DS    A                                                                
PSTBLK   DS    CL(PSTLNQ)                                                       
*                                                                               
**NEW 3/16/89                                                                   
SAVER2   DS    F                                                                
SAVLTL   DS    F                                                                
*                                                                               
PPGKEY   DS    CL64                                                             
******ADRKEYS  DS    CL64                FOR PUB ADDRESS RECORDS                
*                                                                               
FRSVFAX  DS    CL12                FAX NUMBER FROM ADDR ELEMENT                 
TOFAX    DS    CL16                FAX NUMBER FROM CONTROL FILE                 
*                                                                               
VPRNTOFC DS    V                   ADDR OF PRNTOFC                              
VOFFICER DS    V                   ADDR OF OFFICER                              
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
PPBYOWRK DS    600C                                                             
         EJECT                                                                  
*              PRINT PUB GROUP ASSIGNMENTS                                      
*                                                                               
*     SPECIAL NOTES- RA SHOULD STILL POINT TO PPWORK                            
*                    DO NOT USE R9 IN THIS CSECT                                
*                    IT IS NEED TO ACCESS ROUTINES IN THE MAIN CSECT            
*                                                                               
         SPACE 3                                                                
PRTPGA   CSECT                                                                  
         NMOD1 0,PRTPGA                                                         
         L     RC,0(R1)         PUBREC                                          
         SPACE 2                                                                
*                                                                               
         MVC   FULL,AREC          STORE PPG'S AREC                              
         CLI   GRPSW,2            FIRST TIME THRU ?                             
         BNE   PGATSTG             NO                                           
         MVI   GRPSW,0             YES                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY              ---------------------                        
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVC   GRPPAGY,PUBKAGY                                                  
         MVC   GRPPMED,PUBKMED                                                  
         MVI   GRPPTYP,GRPPBGQ     PUB GROUP                                    
*****    MVC   GRPPVAL(6),PUBKPUB                                               
         MVC   SAVEPKEY,GRPPKEY                                                 
*                                                                               
         GOTO1 HIGH                GET FIRST PASSIVE POINTER                    
*                                                                               
         CLC   SAVEPKEY(4),GRPPKEY  ANY GRP ASSIGNS FOR THIS AGY/MED ?          
         BE    PGATSTG              YES                                         
         MVI   GRPSW,1              NO                                          
*                                                                               
PGATSTG  CLI   GRPSW,1              ANY GRP ASSIGNS FOR THIS AGY/MED ?          
         BE    PGAXIT               NO                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ---------------------                        
         USING GRPPKEY,R4          BUILD PASSIVE POINTER                        
         MVC   GRPPAGY,PUBKAGY                                                  
         MVC   GRPPMED,PUBKMED                                                  
         MVI   GRPPTYP,GRPPBGQ     PUB GROUP                                    
         MVC   GRPPVAL(6),PUBKPUB                                               
         MVC   SAVEPKEY,GRPPKEY                                                 
*                                                                               
         GOTO1 HIGH                GET PASSIVE POINTER                          
*                                                                               
PGAGO    GOTO1 REPORT              SKIP A LINE                                  
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   P+1(17),=C'GROUP ASSIGNMENTS'                                    
         MVC   PSECOND+1(17),=20C'-'                                            
         CLC   SAVEPKEY(13),GRPPKEY    SAME PUB ?                               
         BE    PGAHDR              YES - SET UP TITLES                          
         MVC   P+20(18),=C'*** NONE FOUND ***'                                  
         GOTO1 REPORT                                                           
         B     PGAXIT              DONE - NO ASSIGNMENTS                        
PGAHDR   MVC   P+20(29),=C'ID CODE  BREAK 1       NAME 1'                       
         MVC   P+69(20),=C'BREAK 2       NAME 2'                                
         MVC   PSECOND+20(29),=C'-- ----  -------       ------'                 
         MVC   PSECOND+69(20),=C'-------       ------'                          
         GOTO1 REPORT                                                           
*                                                                               
PGALOOP  CLC   SAVEPKEY(13),GRPPKEY    SAME PUB ?                               
         BNE   PGAXIT              NO - DONE                                    
         MVC   SAVEPKEY,GRPPKEY                                                 
         LA    R3,P+20             LINE AREA                                    
         USING PGALIND,R3                                                       
         MVC   PGALIN,SPACES                                                    
*                                                                               
         MVC   SAVEID,GRPPID       GROUP ID/CODE FROM PASSIVE PTR               
         MVC   SAVECODE,GRPPCODE   XL2 PWOS                                     
         ICM   R2,B'1100',GRPPCODE        FROM PASSIVE PTR                      
         SRL   R2,12                DD DD ?? ??  =>  00 0D DD D?                
         ST    R2,FULL                                                          
         OI    FULL+3,X'0F'         00 0D DD DS                                 
         UNPK  CODECHAR(5),FULL+1(3)             =>  Z0 ZD ZD ZD ZD             
         DROP  R4                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4           BUILD GROUP DEFINITION KEY                   
         MVC   GRPKAGY,PUBKAGY                                                  
         MVC   GRPKMED,PUBKMED                                                  
         MVC   GRPKID,SAVEID       ID FROM PASSIVE PTR                          
         MVI   GRPKRCOD,GRPKBTYQ   PUB GROUP                                    
         MVC   SAVEKKEY,GRPKEY                                                  
*                                                                               
         GOTO1 HIGH                GET GROUP DEFINITION RECORD                  
*                                                                               
         CLC   SAVEKKEY(10),GRPKEY                                              
         BE    *+6                 GET IT?                                      
         DC    H'0'                NO - VERY BAD NEWS                           
*                                                                               
         MVC   FULL,AREC          STORE PPG'S AREC                              
         L     R6,PPFILEC                                                       
         LA    R7,2048(R6)                                                      
         LA    R7,2048(R7)                                                      
         USING PPFILED,R6,R7                                                    
         LA    R4,PCONREC          I/O FOR GROUP DEFINITION RECORD              
         ST    R4,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R4,33(R4)                                                        
         USING GRPBRKD,R4                                                       
         CLI   GRPBRKCD,GRPBRKCQ      BREAK DESCRIPTION ELEMENT                 
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         ZIC   R2,GRPBK1LN         L'BREAK CODES                                
         ZIC   R0,GRPBK2LN                                                      
         AR    R2,R0               L'WHOLE GROUP CODE                           
         BCTR  R2,0                                                             
         EX    R2,PGAEX                                                         
         B     PGA10                                                            
PGAEX    MVC   PGACODE(0),CODECHAR+1    CODE TO LINE BLANK PADDED               
*                                                                               
PGA10    MVC   PGABRK1,GRPBK1      BREAK TITLES TO LINE                         
         OC    GRPBK2,GRPBK2       MAY BE ONLY ONE                              
         BZ    PGA20                                                            
         MVC   PGABRK2,GRPBK2                                                   
         DROP  R4                                                               
*                                                                               
PGA20    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4           BUILD GROUP RECORD KEY                       
         MVC   GRPKAGY,PUBKAGY                                                  
         MVC   GRPKMED,PUBKMED                                                  
         MVC   GRPKID,SAVEID       ID/CODE FROM PASSIVE POINTER                 
         MVC   GRPKCODE,SAVECODE   GROUP RECORD                                 
         MVI   GRPKRCOD,GRPKBTYQ   PUB GROUP                                    
         MVC   SAVENKEY,GRPKEY                                                  
*                                                                               
         GOTO1 HIGH                GET GROUP RECORD                             
*                                                                               
         CLC   SAVENKEY(10),GRPKEY                                              
         BE    *+6                 NOT THERE?                                   
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         LA    R4,PCONREC          I/O FOR GROUP CODE RECORD                    
         ST    R4,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R4,33(R4)                                                        
         USING GRPGRPD,R4                                                       
         CLI   GRPGRPCD,GRPGRPCQ      BREAK NAMES ELEMENT                       
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   PGANAME1,GRPGNAM1      GROUP NAMES TO LINE                       
         OC    GRPGNAM2,GRPGNAM2                                                
         BZ    PGA30                                                            
         MVC   PGANAME2,GRPGNAM2                                                
*                                                                               
PGA30    DS    0H                  PRINT THE LINE                               
         MVC   PGAID,SAVEID        GROUP ID TO LINE                             
         GOTO1 REPORT                                                           
         MVC   P,SPACES                                                         
         DROP  R3,R4                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),SAVEPKEY    RESTORE KEY TO PASSIVE PTR                   
         GOTO1 HIGH                                                             
         CLC   SAVEPKEY(25),KEY                                                 
         BE    PGA40                                                            
         DC    H'0'                                                             
*                                                                               
PGA40    GOTO1 SEQ                 NEXT PASSIVE POINTER                         
         LA    R4,KEY                                                           
         B     PGALOOP                                                          
         DROP  R6,R7                                                            
*                                                                               
PGAXIT   DS    0H                                                               
         MVC   AREC,FULL           RESTORE PPG'S AREC                           
         MVC   KEY(64),PPGKEY      RESTORE PPG'S KEYS                           
         XIT1                                                                   
*                                                                               
SAVEID   DS    C                   GROUP ID                                     
SAVECODE DS    XL2                 GROUP CODE (PWOS)                            
CODECHAR DS    XL5                 UNPK AREA                                    
SAVEPKEY DS    XL32                PASSIVE POINTER - PUB GRP ASSIGNS            
SAVEKKEY DS    XL32                PUB GROUP ID DEFINITION                      
SAVENKEY DS    XL32                PUB GROUP CODE                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              PRINT PRODUCTION DETAILS                                         
*                                                                               
*     SPECIAL NOTES- RA SHOULD STILL POINT TO PPWORK                            
*                    DO NOT USE R9 IN THIS CSECT                                
*                    IT IS NEED TO ACCESS ROUTINES IN THE MAIN CSECT            
*                                                                               
         SPACE 3                                                                
PRTPROD  CSECT                                                                  
         NMOD1 0,PRTPROD                                                        
         L     RC,0(R1)         PUBREC                                          
*                                                                               
         SPACE 2                                                                
         MVI   ALLOWLIN,2      ALLOW AT LEAST 2 LINES FOR UNDERSCORES           
         MVC   P+1(10),=C'PRODUCTION'                                           
PR2      CLI   0(R2),X'20'                                                      
         BE    PR4                                                              
         CLI   0(R2),0                                                          
         BNE   PR3                                                              
         MVC   PSECOND+1(10),=20C'-'                                            
         MVC   P+20(14),=C'NO INFORMATION'                                      
         GOTO1 REPORT                                                           
         B     PRTPRODX                                                         
*                                                                               
PR3      IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     PR2                                                              
         SPACE 2                                                                
PR4      DS    0H                                                               
         MVC   P+20(19),=C'AGENCY COMMISSION ='                                 
         USING PUBGENEL,R2                                                      
**NEW 3/9/89                                                                    
         CP    PUBAC,=P'-1'        -.001 MEANS 100.00                           
         BNE   PR5                                                              
         MVC   P+40(6),=C'100.00'                                               
         B     PR5X                                                             
**NEW 3/9/89                                                                    
PR5      EDIT  (P3,PUBAC),(6,P+40),3,ALIGN=LEFT                                 
PR5X     MVC   P+60(15),=C'DEADLINE DAYS ='                                     
         EDIT  (P2,PUBCDDAS),(3,P+76),ALIGN=LEFT                                
         SPACE 2                                                                
PR6      GOTO1 REPORT                                                           
         MVC   P+1(10),=20C'-'                                                  
         MVC   P+20(11),=C'CASH DISC.='                                         
         EDIT  (P2,PUBCD),(4,P+32),1,ALIGN=LEFT                                 
         MVC   P+37(11),=C'EFF. DATE ='                                         
         OC    PUBCDDAT,PUBCDDAT                                                
         BZ    PR7                                                              
*        GOTO1 DTCNV,DMCB,(1,PUBCDDAT),(3,P+49)                                 
         GOTO1 DATCON,DMCB,(3,PUBCDDAT),(5,P+49)                                
PR7      CLI   QMEDIA,C'N'                                                      
*NOP*    BNE   PR21                                                             
         BNE   PR11                                                             
         MVC   P+60(12),=C'TABLOID = NO'                                        
         CLI   PUBTBLD,C'T'                                                     
         BNE   PR8                                                              
         MVC   P+70(3),=C'YES'                                                  
         SPACE 2                                                                
PR8      GOTO1 REPORT                                                           
         MVC   P+20(21),=C'MINIMUM DEPTH = OTHER'                               
         CLI   PUBMDROP,C'3'                                                    
         BE    PR9                                                              
         MVC   P+36(8),=C'14 LINES'                                             
         CLI   PUBMDROP,C'2'                                                    
         BE    PR9                                                              
         MVC   P+36(23),=C'AS MANY INCHES AS COLS.'                             
         CLI   PUBMDROP,C'1'                                                    
         BE    PR9                                                              
         MVC   P+36(23),SPACES                                                  
         SPACE 2                                                                
PR9      DS    0H                                                               
         MVC   P+60(22),=C'TYPE OF PRESS = OFFSET'                              
         CLI   PUBPRESS,C'1'                                                    
         BE    PR10                                                             
         MVC   P+76(07),=C'FLATBED'                                             
         CLI   PUBPRESS,C'3'                                                    
         BE    PR10                                                             
         MVC   P+76(09),=C'PHOTOCOMP'                                           
         CLI   PUBPRESS,C'4'                                                    
         BE    PR10                                                             
         MVC   P+76(18),=C'ROTARY LETTERPRESS'                                  
         CLI   PUBPRESS,C'2'                                                    
         BE    PR10                                                             
         MVC   P+76(18),SPACES                                                  
         SPACE 2                                                                
PR10     GOTO1 REPORT                                                           
         MVC   P+20(18),=C'COLUMNS PER PAGE ='                                  
         EDIT  (P2,PUBCPP),(3,P+39),ALIGN=LEFT                                  
*                       WE GET HERE FROM PR7 IF NOT MEDIA NEWSPAPER             
PR11     MVC   P+60(17),=C'EXCLUSIONS = NONE'                                   
         LA    R4,P+73                                                          
         TM    PUBEXCL,X'20'                                                    
         BZ    PR12                                                             
         MVC   0(6,R4),=C'LIQUOR'                                               
         LA    R4,7(R4)                                                         
         SPACE 2                                                                
PR12     TM    PUBEXCL,X'10'                                                    
         BZ    PR14                                                             
         MVC   0(7,R4),=C'TOBACCO'                                              
         LA    R4,8(R4)                                                         
         SPACE 2                                                                
PR14     TM    PUBEXCL,X'80'                                                    
         BZ    PR16                                                             
         MVC   0(4,R4),=C'BEER'                                                 
         LA    R4,5(R4)                                                         
         SPACE 2                                                                
PR16     TM    PUBEXCL,X'40'                                                    
         BZ    PR18                                                             
         MVC   0(4,R4),=C'WINE'                                                 
         LA    R4,5(R4)                                                         
         SPACE 2                                                                
PR18     TM    PUBEXCL,X'08'                                                    
         BZ    PR20                                                             
         MVC   0(10,R4),=C'CIGARETTES'                                          
         LA    R4,11(R4)                                                        
         SPACE 2                                                                
PR20     GOTO1 REPORT                                                           
         CLI   QMEDIA,C'N'                                                      
         BNE   PR21                                                             
         MVC   P+20(18),=C'UNITS PER COLUMN ='                                  
         CLI   PUBLPCI,C'I'       SEE IF DOING INCHES                           
         BNE   PR20C                                                            
*                               INCHES ARE CARRIED PACKED WITHOUT SIGN          
*                               2 DECIMALS IMPLIED                              
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE+5(2),PUBLPC                                               
         MVI   DOUBLE+7,X'0C'                                                   
         DP    DOUBLE,=P'10'                                                    
         ZAP   DOUBLE,DOUBLE(6)                                                 
         EDIT  (P8,DOUBLE),(6,P+39),2,ALIGN=LEFT,TRAIL=C'I'                     
         B     PR20E                                                            
*                                                                               
PR20C    EDIT  (P2,PUBLPC),(4,P+39),ALIGN=LEFT,TRAIL=C'L'                       
PR20E    MVC   P+60(16),=C'BEST FOOD DAYS ='                                    
         LA    R4,PUBBFDS                                                       
         LA    R5,P+77                                                          
         BAS   RE,DAYEDIT                                                       
         CLI   PUBBFDS,0                                                        
         BNE   *+10                                                             
         MVC   P+77(4),=C'NONE'                                                 
         GOTO1 REPORT                                                           
         MVC   P+20(20),=C'UNITS DOUBLE TRUCK ='                                
         CLI   PUBLDTI,C'I'           SEE IF DOING INCHES                       
         BNE   PR20M                                                            
         EDIT  (P3,PUBLDT),(7,P+41),2,ALIGN=LEFT,TRAIL=C'I'                     
         B     PR20P                                                            
*                                                                               
PR20M    EDIT  (P3,PUBLDT),(5,P+41),ALIGN=LEFT,TRAIL=C'L'                       
**NEW 3/16/89                                                                   
         DROP  R2                                                               
*                                                                               
PR20P    ST    R2,SAVER2         SAVE MAIN PUB'S PROD ELEM ADDR                 
         XC    SAVLTL,SAVLTL                                                    
         L     R2,ALTLREC        GET OTHER DATA FOR LTLREC                      
         LA    R2,33(R2)                                                        
PR20Q    CLI   0(R2),0                                                          
         BE    PR20X                                                            
         CLI   0(R2),X'21'                                                      
         BE    PR20R                                                            
         ZIC   R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     PR20Q                                                            
         USING PUBSPREL,R2                                                      
*                                                                               
PR20R    ST    R2,SAVLTL           SAVE ELEM ADDR                               
         MVC   P+60(22),=C'UNITS CHARGED/COLUMN ='                              
         CLI   PUBLCFCI,C'I'          SEE IF DOING INCHES                       
         BNE   PR20R5                                                           
*                               INCHES ARE CARRIED PACKED WITHOUT SIGN          
*                               2 DECIMALS IMPLIED                              
         XC    DOUBLE,DOUBLE                                                    
         MVC   DOUBLE+5(2),PUBLCFC                                              
         MVI   DOUBLE+7,X'0C'                                                   
         DP    DOUBLE,=P'10'                                                    
         ZAP   DOUBLE,DOUBLE(6)                                                 
*                                                                               
         EDIT  (P8,DOUBLE),(7,P+83),2,ALIGN=LEFT,TRAIL=C'I'                     
         B     PR20S                                                            
*                                                                               
PR20R5   EDIT  (P2,PUBLCFC),(3,P+83),ALIGN=LEFT,TRAIL=C'L'                      
*                                                                               
PR20S    GOTO1 REPORT                                                           
         MVC   P+20(14),=C'COLUMN WIDTH ='                                      
         CLI   PUBCWI,C'P'      SEE IF PICAS                                    
         BNE   PR20S5                                                           
         EDIT  (P3,PUBCOLWD),(6,P+35),3,ALIGN=LEFT                              
         MVC   P+43(5),=C'PICAS'                                                
         B     PR20X                                                            
*                                                                               
PR20S5   EDIT  (P3,PUBCOLWD),(6,P+35),4,ALIGN=LEFT                              
         MVC   P+43(6),=C'INCHES'                                               
         DROP  R2                                                               
         USING PUBGENEL,R2                                                      
*                                                                               
PR20X    L     R2,SAVER2                                                        
*                                                                               
PR21     MVC   P+60(19),=C'CLOSING MONTH/DAY ='                                 
         LA    R4,PUBCLMO                                                       
         LA    R5,P+80                                                          
         BAS   RE,MDEDIT                                                        
         CLI   QMEDIA,C'N'                                                      
         BE    PR22                      CNG 12/08/87 PR23 TO PR22              
         GOTO1 REPORT                                                           
         MVC   P+20(11),=C'FREQUENCY ='                                         
         MVC   P+32(2),PUBMFREQ                                                 
         MVC   P+60(19),=C'ON SALE MONTH/DAY ='                                 
         LA    R4,PUBOSMO                                                       
         BAS   RE,MDEDIT                                                        
         GOTO1 REPORT                                                           
         MVC   P+20(16),=C'CLASSIFICATION ='                                    
         MVC   P+37(3),PUBMCLAS                                                 
         MVC   P+60(19),=C'PAYMENT MONTH/DAY ='                                 
         LA    R4,PUBPAYMO                                                      
         BAS   RE,MDEDIT                                                        
         GOTO1 REPORT                                  ADD 12/08/87             
         OI    PR22A+1,X'F0'                           ADD 12/08/87             
*                                                                               
PR22     MVC   P+60(24),=C'MAT. CLOSING MONTH/DAY ='   ADD 12/08/87             
         LA    R4,PUBMCLMO                             ADD 12/08/87             
         LA    R5,P+85                                 ADD 12/08/87             
         BAS   RE,MDEDIT                               ADD 12/08/87             
PR22A    NOP   PR24                 ON IF QMEDIA^=N    ADD 12/08/87             
*                                                                               
PR23     GOTO1 REPORT                                                           
         MVC   P+20(21),=C'RATE STRUCTURE = FLAT'                               
         CLI   PUBFLAT,C'F'                                                     
         BE    PR23C                                                            
         MVC   P+37(13),=C'SLIDING SCALE'                                       
         CLI   PUBFLAT,C'S'                                                     
         BE    PR23C                                                            
         MVC   P+37(13),SPACES                                                  
PR23C    MVC   P+60(25),=C'FULL DEPTH =       INCHES'                           
         OC    PUBFD,PUBFD                                                      
         BZ    PR23E                                                            
         EDIT  (P3,PUBFD),(5,P+73),2,ALIGN=LEFT                                 
*                                                                               
**NEW 3/16/89                                                                   
PR23E    L     R2,SAVLTL                                                        
         LTR   R2,R2                                                            
         BZ    PR24                                                             
         DROP  R2                                                               
         USING PUBSPREL,R2                                                      
         GOTO1 REPORT                                                           
         MVC   P+20(16),=C'CLASSIFICATION ='                                    
         MVC   P+37(2),PUBCLASS                                                 
         SPACE 2                                                                
PR24     NI    PR22A+1,X'0F'                           ADD 12/08/87             
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     PRTPRODX                                                         
PRTPRODX XIT1                                                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              PRINT PUB AD SIZING INFORMATION                                  
*                                                                               
*     SPECIAL NOTES- RA SHOULD STILL POINT TO PPWORK                            
*                    DO NOT USE R9 IN THIS CSECT.  IT IS NEEDED                 
*                    TO ACCESS ROUTINES IN THE MAIN CSECT                       
*                                                                               
         SPACE 3                                                                
PRTSZE   CSECT                                                                  
         NMOD1 0,*PRTSZE*                                                       
         L     RC,0(R1)            PUBREC                                       
         SPACE 2                                                                
*                                                                               
         MVC   FULL,AREC           SAVE PPG'S AREC                              
         XC    KEY,KEY                                                          
         MVC   KEY(PUBKCOD-PUBKEY),PUBREC                                       
         MVI   KEY+PUBKCOD-PUBKEY,X'83'      PUB AD SIZING RECORD CODE          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'PUBDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(10),KEYSAVE     MED/PUB(6)/AGY/COD                           
         BNE   PSZXIT              NO AD SIZING RECORD FOUND                    
*                                  GET SIZING RECORD                            
         L     R6,PPFILEC                                                       
         USING PPFILED,R6                                                       
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'PUBFILE',KEY+27,PCONREC, X        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
         MVI   ALLOWLIN,2          ALLOW FOR UNDERSCORES                        
         MVC   P+01(09),=C'AD SIZING'                                           
         MVC   PSECOND+01(09),=20C'-'                                           
*                                                                               
         LA    R2,PCONREC+33       CONTRACT REC AREA USED FOR I/O               
         CLI   0(R2),X'10'         TRIM ELEMENT ?                               
         BNE   PSZ10               NO TRIM ELEMENT                              
         USING PPPUBTD,R2                                                       
         MVC   P+20(10),=C'TRIM SIZE:'                                          
         MVC   P+40(07),=C'WIDTH -'                                             
         MVC   P+60(07),=C'DEPTH -'                                             
         LA    R5,PPPUBTWU         POINT TO WIDTH DATA                          
         BAS   RE,EDITSZ                                                        
         MVC   P+48(8),WORK+50     WORK+50 HAS N N/N OR NN NN/NN ETC.           
         LA    R5,PPPUBTDU         POINT TO DEPTH DATA                          
         BAS   RE,EDITSZ                                                        
         MVC   P+68(8),WORK+50     WORK+50 HAS N N/N OR NN NN/NN ETC.           
         GOTO1 REPORT                                                           
         DROP  R2                                                               
*                                                                               
PSZ10    DS    0H                                                               
         LA    R2,PCONREC+33       CONTRACT REC AREA USED FOR I/O               
         MVI   BYTE,X'20'                                                       
         CLI   0(R2),X'20'         NON-BLEED AD DIMENSION ELEMENT ?             
         BE    PSZ10B              PRINT THE DATA                               
         BAS   RE,SZNXTL                                                        
         BNE   PSZ30               NO NON-BLEED AD DIMENS. ELEM'S               
PSZ10B   DS    0H                                                               
         MVC   P+20(28),=C'NON-BLEED AD PAGE DIMENSIONS'                        
         MVI   ALLOWLIN,4                                                       
         GOTO1 REPORT                                                           
         MVC   P+20(07),=C'AD SIZE'                                             
         MVC   P+40(05),=C'WIDTH -'                                             
         MVC   P+50(05),=C'DEPTH -'                                             
         MVC   PSECOND+20(07),=20C'-'                                           
         MVC   PSECOND+40(05),=20C'-'                                           
         MVC   PSECOND+50(05),=20C'-'                                           
         GOTO1 REPORT                                                           
         USING PPPUBAD,R2                                                       
PSZ10D   DS    0H                                                               
         GOTO1 =V(PSIZEVAL),DMCB,(1,PPPUBACD),WORK                              
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
         MVC   P+20(17),WORK+3     AD SIZE DESCRIPTION                          
         LA    R5,PPPUBAWU         POINT TO WIDTH DATA                          
         BAS   RE,EDITSZ                                                        
         MVC   P+40(8),WORK+50     WORK+50 HAS N N/N OR NN NN/NN ETC.           
         LA    R5,PPPUBADU         POINT TO DEPTH DATA                          
         BAS   RE,EDITSZ                                                        
         MVC   P+50(8),WORK+50     WORK+50 HAS N N/N OR NN NN/NN ETC.           
         GOTO1 REPORT                                                           
         BAS   RE,SZNXTL           LOOK FOR ANOTHER DIMENS ELEM                 
         BE    PSZ10D              PRINT THE DATA (IF FOUND)                    
         GOTO1 REPORT              SKIP A LINE                                  
         DROP  R2                                                               
*                                                                               
PSZ30    DS    0H                                                               
         LA    R2,PCONREC+33       CONTRACT REC AREA USED FOR I/O               
         MVI   BYTE,X'15'                                                       
         CLI   0(R2),X'15'         SAFETY ELEMENT ?                             
         BE    PSZ30B              PRINT THE DATA                               
         BAS   RE,SZNXTL                                                        
         BNE   PSZ40               NO SAFETY ELEMENTS                           
*                                                                               
PSZ30B   DS    0H                                                               
         LA    R3,P+20                                                          
         MVC   0(41,R3),=C'SAFETY-KEEP LIVE MATTER FROM BLEED PLATE:'           
         MVI   ALLOWLIN,2                                                       
         LA    R3,43(R3)                                                        
         LA    R4,P+90             MAX POINT FOR LINE OUTPUT                    
         USING PPPUBSD,R2                                                       
         B     PSZ30C                                                           
PSZ30B2  BAS   RE,SZNXTL                                                        
         BNE   PSZ30P              NO MORE SAFETY ELEMENTS                      
PSZ30C   CLI   PPPUBSCD,C'5'                                                    
         BNE   PSZ30D                                                           
         MVC   0(8,R3),=C'ANY EDGE'                                             
         LA    R3,9(R3)                                                         
         LA    R5,PPPUBSU          POINT TO UNITS DATA                          
         BAS   RE,EDITSAFE                                                      
         B     PSZ30B2             NEXT ELEMENT                                 
PSZ30D   CLI   PPPUBSCD,C'1'                                                    
         BNE   PSZ30E                                                           
         MVC   0(3,R3),=C'TOP'                                                  
         LA    R3,4(R3)                                                         
         LA    R5,PPPUBSU          POINT TO UNITS DATA                          
         BAS   RE,EDITSAFE                                                      
         B     PSZ30B2             NEXT ELEMENT                                 
PSZ30E   CLI   PPPUBSCD,C'2'                                                    
         BNE   PSZ30F                                                           
         MVC   0(6,R3),=C'BOTTOM'                                               
         LA    R3,7(R3)                                                         
         LA    R5,PPPUBSU          POINT TO UNITS DATA                          
         BAS   RE,EDITSAFE                                                      
         B     PSZ30B2             NEXT ELEMENT                                 
PSZ30F   CLI   PPPUBSCD,C'3'                                                    
         BNE   PSZ30G                                                           
         MVC   0(14,R3),=C'FACE (OUTSIDE)'                                      
         LA    R3,15(R3)                                                        
         LA    R5,PPPUBSU          POINT TO UNITS DATA                          
         BAS   RE,EDITSAFE                                                      
         B     PSZ30B2             NEXT ELEMENT                                 
PSZ30G   CLI   PPPUBSCD,C'4'                                                    
         BNE   PSZ30H                                                           
         MVC   0(6,R3),=C'GUTTER'                                               
         LA    R3,7(R3)                                                         
         LA    R5,PPPUBSU          POINT TO UNITS DATA                          
         BAS   RE,EDITSAFE                                                      
         B     PSZ30B2             NEXT ELEMENT                                 
PSZ30H   CLI   PPPUBSCD,C'6'                                                    
         BE    *+6                                                              
         DC    H'0'                SAFETY CODE MUST BE 1 THRU 6                 
         MVC   0(9,R3),=C'FROM TRIM'                                            
         LA    R3,10(R3)                                                        
         LA    R5,PPPUBSU          POINT TO UNITS DATA                          
         BAS   RE,EDITSAFE                                                      
         B     PSZ30B2             NEXT ELEMENT                                 
*                                                                               
PSZ30P   GOTO1 REPORT              PRINT THE SAFETY LINE(S)                     
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
PSZ40    DS    0H                                                               
         LA    R2,PCONREC+33       CONTRACT REC AREA USED FOR I/O               
         MVI   BYTE,X'30'                                                       
         CLI   0(R2),X'30'         BLEED AD DIMENSION ELEMENT ?                 
         BE    PSZ40B              PRINT THE DATA                               
         BAS   RE,SZNXTL                                                        
         BNE   PSZXIT              NO MORE ELEMENTS - DONE                      
PSZ40B   DS    0H                                                               
         MVC   P+20(24),=C'BLEED AD PAGE DIMENSIONS'                            
         MVI   ALLOWLIN,4                                                       
         GOTO1 REPORT                                                           
         MVC   P+20(13),=C'BLEED AD SIZE'                                       
         MVC   P+40(05),=C'WIDTH -'                                             
         MVC   P+50(05),=C'DEPTH -'                                             
         MVC   PSECOND+20(13),=20C'-'                                           
         MVC   PSECOND+40(05),=20C'-'                                           
         MVC   PSECOND+50(05),=20C'-'                                           
         GOTO1 REPORT                                                           
         USING PPPUBBD,R2                                                       
PSZ40D   DS    0H                                                               
         GOTO1 =V(PSIZEVAL),DMCB,(1,PPPUBBCD),WORK                              
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
         MVC   P+20(17),WORK+3     AD SIZE DESCRIPTION                          
         LA    R5,PPPUBBWU         POINT TO WIDTH DATA                          
         BAS   RE,EDITSZ                                                        
         MVC   P+40(8),WORK+50     WORK+50 HAS N N/N OR NN NN/NN ETC.           
         LA    R5,PPPUBBDU         POINT TO DEPTH DATA                          
         BAS   RE,EDITSZ                                                        
         MVC   P+50(8),WORK+50     WORK+50 HAS N N/N OR NN NN/NN ETC.           
         GOTO1 REPORT                                                           
         BAS   RE,SZNXTL           LOOK FOR ANOTHER DIMENS ELEM                 
         BE    PSZ40D              PRINT THE DATA (IF FOUND)                    
         DROP  R2                                                               
*                                                                               
PSZXIT   DS    0H                                                               
         MVC   AREC,FULL           RESTORE PPG'S AREC                           
         MVC   KEY(64),PPGKEY      RESTORE PPG'S KEYS                           
         XIT1                                                                   
         DROP  R6                                                               
         SPACE 2                                                                
SZNXTL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   BYTE,0(R2)                                                       
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   SZNXTL                                                           
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
EDITSZ   NTR1                      R5 POINTS TO "2,1,1" BINARY FIELDS           
         XC    WORK,WORK                                                        
         LA    R4,WORK+50                                                       
         EDIT  (2,0(R5)),(3,0(R4)),ALIGN=LEFT                                   
         CLI   2(R5),0             ANY "NUMERATOR" ?                            
         BNH   EDITSZX             NO - DONE                                    
         AR    R4,R0                                                            
         EDIT  (1,2(R5)),(2,1(R4)),ALIGN=LEFT                                   
         AR    R4,R0                                                            
         MVI   1(R4),C'/'                                                       
         EDIT  (1,3(R5)),(2,2(R4)),ALIGN=LEFT                                   
EDITSZX  DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
EDITSAFE NTR1                                                                   
         BAS   RE,EDITSZ                                                        
         MVC   0(8,R3),WORK+50     UNITS DATA TO LINE                           
         LA    R3,10(R3)                                                        
         CR    R3,R4               AT END OF LINE ?                             
         BL    EDITSFX             NO - EXIT                                    
         LA    R3,PSECOND+22       USE PSECOND IF P "FILLED"                    
         LA    R4,PSECOND+110      MAX POINT FOR LINE OUTPUT                    
EDITSFX  DS    0H                                                               
         XIT1  REGS=(R3,R4)                                                     
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
POOL     CSECT                                                                  
******** DC    44110C' '      401 X 110 BYTES                                   
         DC    ((MAXLN+1)*110)C' '                                              
*                                                                               
MAXLN    EQU   600            WAS 400                                           
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPUBZREC                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PUBTAXEL                                                       
       ++INCLUDE DDPSTBLK                                                       
       ++INCLUDE PGENGRP                                                        
       ++INCLUDE CTGENFAX                                                       
       ++INCLUDE DDCOMFACSD                                                     
*                                                                               
PGALIND  DSECT                   PUB GROUP ASSIGNMENTS LINE                     
PGALIN   DS    0CL87                                                            
PGAID    DS    CL1                                                              
         DS    CL2                                                              
PGACODE  DS    CL4                                                              
         DS    CL2                                                              
PGABRK1  DS    CL12                                                             
         DS    CL2                                                              
PGANAME1 DS    CL24                                                             
         DS    CL2                                                              
PGABRK2  DS    CL12                                                             
         DS    CL2                                                              
PGANAME2 DS    CL24                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015PPREP4602 10/13/08'                                      
         END                                                                    
