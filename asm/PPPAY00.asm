*          DATA SET PPPAY00    AT LEVEL 012 AS OF 10/14/20                      
*PHASE T40300A                                                                  
*INCLUDE ACPAYXFR                                                               
*INCLUDE CONVMOS                                                                
         TITLE 'T40300 - PAY PROGRAM BASE'                                      
*                                                                               
*  CHANGE LOG                                                                   
*                                                                               
* KWAN 09/11/14  FIX CR/CK OPTION LENGTH BUG FOR PRINT BUY TOOLKIT              
*                                                                               
* KWAN 03/13/14  CANADIAN TAX OVERRIDE FOR PRINT BUY TOOLKIT                    
*                                                                               
* BOBY  05/08    RETURN FX VALUES TO ADBUYER                                    
*                                                                               
* SMYE  07/05    FIX TO ADBUYER MESSAGES AT P10AB4                              
*                                                                               
* BOBY  03/04    ADD PID AND GROSS TO CHECK REQUESTS                            
*                                                                               
* SMYE  08/03    LINK (LINKIO) TO ADBUYER                                       
*                                                                               
* SMYE 04/19/02  PUBVAL AND PUBEDIT CORE-RESIDENT ADDRESSES SET                 
*                                                                               
* SMYE 12/22/97  GETINS MADE CORE-RESIDENT                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T40300   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 GENOLDX-GENOLD,T40300,R8                                         
*                                                                               
         USING GENOLD,RC                                                        
         USING T403FFD,RA                                                       
         BRAS  RE,INITL                                                         
         RELOC RELO00                                                           
*                                                                               
         MVC   VCOMFACS,16(R1)                                                  
         L     RF,VCOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
         MVC   VMINIO,CMINIO-COMFACSD(RF)                                       
         MVC   ALINKIO,CLINKIO-COMFACSD(RF)                                     
*                                                                               
*        SET UP INCLUDE MODULES ADDRESSES                                       
*                                                                               
         L     R0,=A(ADBWRKC)         POINT TO ADBUYER WORK AREA CSECT          
         A     R0,RELO00                                                        
         ST    R0,ADBWRKA                                                       
*                                                                               
         L     R0,=A(ADBIO)           POINT TO ADBUYER I/O AREA CSECT           
         A     R0,RELO00                                                        
         ST    R0,ADBIOA                                                        
*                                                                               
         L     R0,=V(ACPAYXFR)        ACPAYXFR                                  
         A     R0,RELO00                                                        
         ST    R0,VACCPAY                                                       
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AAB'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETINS,DMCB           STORE GETINS ADDRESS                      
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AB8'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPUBVAL,DMCB           STORE PUBVAL ADDRESS                      
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AB9'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPUBEDIT,DMCB          STORE PUBEDIT ADDRESS                     
*                                                                               
         MVC   BLANKS,=50C' '                                                   
         MVI   ADBSW,0                                                          
         LH    RE,=Y(IOAREA-GENOLD)                                             
         AR    RE,RC                                                            
         ST    RE,AIOAREA                                                       
         MVC   AREC,AIOAREA           DEFAULT IO AREA                           
*                                                                               
***********************************************************************         
*                                                                     *         
*   THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK                     *         
*   WHICH ARE "PERSONAL ID"                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PID      DS    0H                                                               
*                                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF) V(GETFACT)                              
*                                                                               
         GOTO1 (RF),DMCB,(2,0),0,0 0,0  RETRIEVE BASIC FACTS                    
*                                                                               
         L     R1,0(R1)            ESTABLISH FACTS AREA                         
         USING FACTSD,R1                                                        
*                                                                               
         LAY   RF,SVSECAGY         SECURITY AGENCY SAVEAREA                     
*                                                                               
         MVC   0(L'SVSECAGY,RF),FATAGYSC   SAVE SECURITY AGENCY                 
*                                                                               
         LAY   RF,SVPID            POINT TO PID SAVEAREA                        
*                                                                               
         XC    0(L'SVPID,RF),0(RF)     PASSWORD ID NUMBER CLEARED               
*                                                                               
         TM    FATFLAG,X'08'       CHECK IF SECRET CODE IS THERE                
         BZ    *+10                                                             
         MVC   0(L'SVPID,RF),FAPASSWD  SAVE PASSWORD ID NUMBER                  
*                                                                               
         DROP  R1                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        SEE IF THIS IS A CALL FROM DDLINK            *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,GLVXLENQ,GLVXCTL                     
         CLI   DMCB+8,GLEGNF                                                    
         BE    NOTLINK             NOT CALL FROM DDLINK                         
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   WORK(12),=C'PRILINPRIPAY'                                        
         BNE   NOTLINK             NOT "MY" CALL FROM DDLINK                    
*                                                                               
         L     R7,ADBWRKA                                                       
         USING ADBWRKD,R7          *** NOTE R7 AS A WORK AREA REGISTER          
*                                                                               
         LA    R2,LIOBAREA                                                      
         USING LIOBD,R2                                                         
         MVC   LIOBABUF,ATIA                                                    
         L     RE,ADBIOA                                                        
         ST    RE,LIOBAREC                                                      
         MVC   LIOBACOM,VCOMFACS                                                
         BASR  RE,0                                                             
         AHI   RE,MAP-*                                                         
         STCM  RE,15,LIOBAMAP                                                   
         L     RE,ADBWRKA                                                       
         ST    RE,LIOBASB1                                                      
         LA    RE,T403FFD                                                       
         ST    RE,LIOBASB2                                                      
         LA    RE,GENOLD                                                        
         ST    RE,LIOBASB3                                                      
         MVI   LIOBMSYS,4          PRINT SYSTEM MESSAGES                        
*                                                                               
         MVI   LIOBINDS,LIOBIMLT   MAY PUT MULTIPLE O/P RECORDS                 
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAINI',LIOBD)                                   
         BNE   NOTLINK             NOT A CALL FROM DDLINK                       
*                                                                               
ISLINK   DS    0H                                                               
         LA    RE,ADBWRKS          START OF ADBUYER STORAGE AFTER LIOB          
         LH    RF,=Y(ADBWRKX-ADBWRKS)   LENGTH OF ABOVE STORAGE                 
         XCEFL ,                        CLEAR                                   
         XC    ADBTWA(ADBTWAX-ADBTWA),ADBTWA  CLEAR ADBUYER TWA FIELDS          
         BAS   RE,FCLCLR         CLEAR ALL UNPROTECTED FIELDS ON SCREEN         
*                                                                               
ISLKGET  DS    0H                                                               
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)                                   
*NOP*    BE    *+6                                                              
*NOP*    DC    H'0'                SHOULD ALWAYS GET ONE RECORD                 
         MVI   ADBSW,C'Y'          DOING AN ADBUYER CALL                        
         OI    LKWRKSW1,LKTOPAYQ   LINK TO PAY CALL                             
         CLC   LIOBMAP#,=X'0160'   "INFO" REQUEST ?                             
         BE    PAYGLBX             YES - GO PROCESS                             
         CLC   LIOBMAP#,=X'0164'   "CLEAR FOR PAY" REQUEST ?                    
         BE    *+6                                                              
         DC    H'0'                MUST BE INFO OR CLEAR/PAY                    
         OC    LIOBDTA#,LIOBDTA#   TEST LIODIRET RETURN                         
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE RECORD "BREAK" ON 1ST GET          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*                  SET UP "CLEAR FOR PAY" REQUEST                     *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         MVI   SERTBLX,X'FF'       MARK END OF SERIAL NUMBER STORAGE            
         ZAP   SERTBL(L'SERTBL),=P'0' INITIALIZE 1ST SERIAL STORE FIELD         
         CLI   ADBPART,C'Y'        DOING A PARTIAL PAYMENT ?                    
         BE    ISLK10              YES - DO NOT SET "TEST" AFTER MEDIA          
         LA    RE,PAYMDH           POINT TO MEDIA HEADER                        
         MVC   9(4,RE),=C'TEST'    SET AS TEST                                  
         MVI   5(RE),5             SET LENGTH (MTEST)                           
*                                                                               
ISLK10   DS    0H                                                               
         CLI   ADBOVRD,C'Y'        OVERRIDING PROFILE VALUES ?                  
         BNE   ISLK25              NO                                           
         CLI   ADBPART,C'Y'        DOING A PARTIAL PAYMENT ?                    
         BE    ISLK15              YES - NO "TEST" AFTER MEDIA                  
         LA    RE,PAYMDH           POINT TO MEDIA HEADER                        
         MVC   13(4,RE),=C'OVRD'   SET OVERRIDE                                 
         MVI   5(RE),9             LENGTH OF MEDIA FIELD (MTESTOVRD)            
         B     ISLK25                                                           
*                                                                               
ISLK15   DS    0H                                                               
         LA    RE,PAYMDH           POINT TO MEDIA HEADER                        
         MVC   9(4,RE),=C'OVRD'    SET OVERRIDE                                 
         MVI   5(RE),5             LENGTH OF MEDIA FIELD (MOVRD)                
*                                                                               
ISLK25   DS    0H                                                               
         LA    RE,PAYOPH           POINT TO OPTIONS FIELD                       
         CLI   ADBSING,C'Y'        MULTI-REQ PAY (SINGLY) ?                     
         BNE   ISLK27              NO                                           
         MVC   8(4,RE),=C'SING'    SET OPTION                                   
         MVI   5(RE),4             SET LENGTH OF OPTION FIELD                   
         OI    4(RE),X'80'         SET "FIELD INPUT THIS TIME"                  
*                                                                               
ISLK27   DS    0H                                                               
         CLI   ADBSING,C'N'        DO NOT DO MULTI-REQ PAY (SINGLY) ?           
         BNE   ISLK30              NO                                           
         MVC   8(4,RE),=C'XSIN'    SET OPTION                                   
         MVI   5(RE),4             SET LENGTH OF OPTION FIELD                   
         OI    4(RE),X'80'         SET "FIELD INPUT THIS TIME"                  
*                                                                               
ISLK30   DS    0H                                                               
*                                                                               
*        CHECK FOR CR/CK REVERSAL OPTION                                        
*                                                                               
ISLCRCK  DS    0H                                                               
*                                                                               
         ICM   RF,15,ADBCRCKA      HAVE CRC/CK INDEX ARRAY?                     
         JZ    ISLCRCKX                                                         
*                                                                               
         USING LQ_D,RF             REQUEST ELEMENT DSECT                        
         CLI   LQ_EL,0             END OF REQUEST?                              
         JE    ISLCRCKX                                                         
         CLC   LQ_DCODE,=AL2(D#CRCKOP)                                          
         JNE   ISLCRCKX            SKIP IF NOT CRCK OPTION                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,LQ_LN          GET CR/CK DATA LENGTH                        
         SHI   R1,LQ_LNDQ          MINUS HEADER LENGTH                          
         CHI   R1,0                ANY REQUEST DATA TO PROCESS?                 
         JNH   ISLCRCKX                                                         
         TM    LQ_TYPE,LQ_TSINQ    SINGLE (FIXED) VALUE?                        
         JZ    ISLCRCKX                                                         
*                                                                               
         LA    R2,8(RE)            POINT TO START OF OPTIONS FIELD              
         SR    R3,R3                                                            
         ICM   R3,1,5(RE)          GET CURRENT OPTIONS LENGTH                   
         JZ    ISLCR32                                                          
         LA    R2,0(R3,R2)         POINT TO NEXT AVAILABLE SLOT                 
         MVI   0(R2),C','          ADD SEPARATOR                                
         LA    R2,1(R2)            BUMP TO NEXT POSITION                        
         AHI   R3,1                ADJUST LENGTH WITH COMMAS                    
         STC   R3,5(RE)                                                         
*                                                                               
ISLCR32  LA    R3,LQ_VALUE         POINT TO INPUT                               
         AR    R3,R1                                                            
         BCTR  R3,0                POINT TO LAST CHAR IN INPUT                  
         CLI   0(R3),C' '                                                       
         JH    *+12                                                             
         BCTR  R1,0                ADJUST ACTUAL INPUT LENGTH                   
         BCTR  R3,0                POINT TO PREVIOUS INPUT CHAR                 
         J     *-12                                                             
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,5(RE)          GET CURRENT OPTIONS LENGTH                   
         AR    R3,R1                                                            
         STC   R3,5(RE)            NEW OPTIONS FIELD LENGTH                     
         CHI   R3,L'PAYOP          MAX LENGTH EXCEEDED?                         
         JNH   ISLCR40                                                          
         SR    R0,R0               GET CURRENT OPTIONS LENGTH                   
         ICM   R0,1,5(RE)                                                       
         LA    R1,L'PAYOP          SET TO MAX                                   
         SR    R1,R0               MAX INPUT LENGTH CAN BE MOVED                
         LA    R0,L'PAYOP          SET TO MAX                                   
         STC   R0,5(RE)            NEW OPTIONS FIELD LENGTH                     
*                                                                               
ISLCR40  BCTR  R1,0                PREPARE TO EX                                
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),LQ_VALUE    MOVE DATA TO OPTIONS FIELD                   
*                                                                               
         OI    4(RE),X'80'         SET "FIELD INPUT THIS TIME"                  
*                                                                               
ISLCRCKX DS    0H                                                               
         DROP  RF                                                               
*                                                                               
*        CHECK FOR GST/PST OVERRIDES                                            
*                                                                               
ISLGPST  DS    0H                                                               
*                                                                               
         ICM   RF,15,ADBGPSTA      HAVE GST/PST INDEX ARRAY?                    
         JZ    ISLGPSTX                                                         
*                                                                               
         USING LQ_D,RF             REQUEST ELEMENT DSECT                        
         CLI   LQ_EL,0             END OF REQUEST?                              
         JE    ISLGPSTX                                                         
         CLC   LQ_DCODE,=AL2(D#GPSTOV)                                          
         JNE   ISLGPSTX            SKIP IF NOT GST/PST OVERRIDE                 
*                                                                               
         SR    R3,R3                                                            
         AHI   R3,1                SET TO EXCUTE LOOP AT LEAST ONCE             
         SR    R0,R0                                                            
         ICM   R0,3,LQ_LN          GET CR/CK DATA LENGTH                        
         TM    LQ_TYPE,LQ_TSINQ    SINGLE (FIXED) VALUE?                        
         JZ    *+16                                                             
         SHI   R0,LQ_LNDQ          MINUS HEADER LENGTH                          
         LA    R1,LQ_VALUE         POINT TO SINGLE VALUE INPUT                  
         J     ISLGP20                                                          
         TM    LQ_TYPE,LQ_TLSTQ    LIST OF VALUES?                              
         JZ    ISLGPSTX                                                         
         SHI   R0,LW_LN1Q          MINUS HEADER LENGTH                          
         LA    R1,LW_LN1Q(RF)      POINT TO FIRST VALUE IN LIST                 
         ICM   R3,3,LQ_LNDQ(RF)    NUMBER OF ENTRIES IN LIST                    
*                                                                               
ISLGP20  CHI   R0,0                ANY REQUEST DATA TO PROCESS?                 
         JNH   ISLGPSTX                                                         
*                                                                               
ISLGP30  ST    R3,FULL             SAVE LOOP COUNTER                            
         LA    R2,8(RE)            POINT TO START OF OPTIONS FIELD              
         SR    R3,R3                                                            
         ICM   R3,1,5(RE)          GET CURRENT OPTIONS LENGTH                   
         JZ    ISLGP32                                                          
         LA    R2,0(R3,R2)         POINT TO NEXT AVAILABLE SLOT                 
         MVI   0(R2),C','          ADD SEPARATOR                                
         LA    R2,1(R2)            BUMP TO NEXT POSITION                        
         AHI   R3,1                ADJUST LENGTH WITH COMMAS                    
         STC   R3,5(RE)                                                         
*                                                                               
ISLGP32  LA    R0,TAXOPTLQ         SET LENGTH OF TAX OPTION FIELD               
         LR    R3,R1               POINT TO INPUT                               
         AR    R3,R0                                                            
         BCTR  R3,0                POINT TO LAST CHAR IN INPUT                  
         CLI   0(R3),C' '                                                       
         JH    *+12                                                             
         BCTR  R0,0                ADJUST ACTUAL INPUT LENGTH                   
         BCTR  R3,0                POINT TO PREVIOUS INPUT CHAR                 
         J     *-12                                                             
*                                                                               
         LR    R3,R0               SAVE ACTUAL INPUT LENGTH                     
         SR    R0,R0                                                            
         ICM   R0,1,5(RE)          GET CURRENT OPTIONS LENGTH                   
         AR    R0,R3                                                            
         STC   R0,5(RE)            NEW OPTIONS FIELD LENGTH                     
         CHI   R0,L'PAYOP          MAX LENGTH EXCEEDED?                         
         JNH   ISLGP40                                                          
         SR    R0,R0               GET CURRENT OPTIONS LENGTH                   
         ICM   R0,1,5(RE)                                                       
         LA    R3,L'PAYOP          SET TO MAX                                   
         SR    R3,R0               MAX INPUT LENGTH CAN BE MOVED                
         LA    R0,L'PAYOP          SET TO MAX                                   
         STC   R0,5(RE)            NEW OPTIONS FIELD LENGTH                     
*                                                                               
ISLGP40  BCTR  R3,0                PREPARE TO EX                                
         EX    R3,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),0(R1)       MOVE DATA TO OPTIONS FIELD                   
*                                                                               
         OI    4(RE),X'80'         SET "FIELD INPUT THIS TIME"                  
         L     R3,FULL             RESTORE LOOP COUNTER                         
         LA    R1,TAXOPTLQ(R1)     POINT TO NEXT INPUT (FOR LIST)               
         JCT   R3,ISLGP30                                                       
*                                                                               
ISLGPSTX DS    0H                                                               
*                                                                               
TAXOPTLQ EQU   27                  LENGTH OF TAX OPT FIELD IN REQUEST           
*                                                                               
         B     PAYGLBX                                                          
*                                                                               
NOTLINK  DS    0H                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        CHECK GLOBBER TO SEE IF THIS IS CALL FROM MATCH                        
*        IF SO GO FILL IN FIELDS ON SCREEN WITH THOSE FROM GLOBBER              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PAYGLB   DS    0H                                                               
*                                                                               
         OC    VGLOBBER,VGLOBBER   MUST HAVE GLOBBER ADDRESS                    
         BZ    PAYGLBX                                                          
*                                                                               
*        SKIP IF STILL PROCESSING CALL FROM INVOICE MATCH                       
*                                                                               
         CLI   INVMATSW,C'Y'       IF NOT DOING INV MATCH CALL                  
         BNE   PAYGLB10              CHECK GLOBBER AREA                         
*                                                                               
         L     RF,VTIOB            POINT TO INPUT AREA                          
         USING TIOBD,RF            ESTABLISH AREA                               
         CLI   TIOBAID,12          LOOK FOR PFKEY 12 HIT                        
         BE    *+8                                                              
         CLI   TIOBAID,24                                                       
         BNE   PAYGLBX             SKIP GLOBBER IF NOT FOUND                    
*                                                                               
         SR    RF,RF                                                            
         LA    R2,PAYMSGH          POINT TO FIRST FLD ON SCREEN                 
         IC    RF,PAYMSGH          LENGTH OF 1ST FIELD ON SCREEN                
         LA    R2,0(RF,R2)         POINT TO SERVICE REQUEST FIELD               
         MVC   8(17,R2),=CL17'=SW ' SET FOR RETURN TO MATCH                     
*                                                                               
         B     PAY20               RETURN WITH SWAP OTHERWISE                   
*                                                                               
         DROP  RF                                                               
*                                                                               
PAYGLB10 DS    0H                                                               
*                                                                               
*        READ GLOBBER AREA FOR INVOICE PAY AMOUNT.                              
*        IF FOUND WE ARE COMING FROM INVOICE MATCH                              
*        ELSE NORMAL PAY                                                        
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',PAYAMT1H,,GLVPRGRS                        
*                                                                               
         CLI   DMCB+8,GLEGNF       DONE IF NOT FOUND                            
         BE    PAYGLBX                                                          
*                                                                               
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,FILSCR           GO FILL IN SCREEN FROM GLOBBER               
*                                                                               
PAYGLBX  DS    0H                                                               
*                                                                               
         XC    PAYMSG,PAYMSG       CLEAR LAST MESSAGE                           
         FOUT  PAYMSGH                                                          
         BAS   R9,CLRTST                                                        
*                                                                               
         OI    PAYCL+2,C' '        ENSURE NO BINARY ZERO                        
         OC    SVDATE,SVDATE                                                    
         BNZ   PAY1                                                             
         GOTO1 VDATCON,DMCB,(5,0),(0,SVDATE)   TODAY'S DATE                     
         GOTO1 VDATCON,DMCB,SVDATE,(3,BTODAY)                                   
*                                                                               
PAY1     DS    0H                                                               
         EJECT                                                                  
* TEST IF VALIDATED THRU PAYEE                                                  
*                                                                               
         TM    PAYOPH+4,X'20'                                                   
         BZ    PAY2                                                             
         TM    PAYMDH+4,X'20'                                                   
         BZ    PAY2                                                             
         TM    PAYCLH+4,X'20'                                                   
         BZ    PAY2                                                             
         TM    PAYPRH+4,X'20'                                                   
         BZ    PAY2                                                             
         TM    PAYDTH+4,X'20'                                                   
         BZ    PAY2                                                             
         TM    PAYPBH+4,X'20'                                                   
         BZ    PAY2                                                             
         TM    PAYEEH+4,X'20'                                                   
         BZ    PAY2                                                             
         TM    PAYCKH+1,X'20'      NOP SELECTIVE CHECKS                         
         BO    *+12                IF CHECK CONTROL DATE FIELD                  
*                                  PROTECTED                                    
         TM    PAYCKH+4,X'20'                                                   
         BZ    PAY2                                                             
         TM    PAYINVDH+4,X'20'        INVOICE DATE                             
         BZ    PAY2                                                             
*                                                                               
         B     PAY10                                                            
         SPACE 2                                                                
* CALL HEADLINE EDIT                                                            
*                                                                               
PAY2     GOTO1 VCALLOV,DMCB,(1,0),(RA)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        ADBUYER CALL LOGIC BELOW                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         CLI   ADBSW,C'Y'          DOING AN ADBUYER CALL ?                      
         BNE   NOTADB              NO                                           
*                                                                               
         L     R7,ADBWRKA          RESTORE POSSIBLY ALTERED REGISTERS           
         LA    R2,LIOBAREA                                                      
*                                                                               
*                                  HANDLE REPLY                                 
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)       GET "NEXT OR END"           
*NOP*    BNE   *+6                                                              
*NOP*    DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
         CLI   ERRAREA,0                                                        
         BE    P2AB                OK - NOT ERROR                               
         CLC   LIOBMAP#,=X'0160'   "INFO" REQUEST ?                             
         BE    ADBERR              YES - DONE - SEND ERROR                      
*                                  GET "END" OF CLEAR FOR PAY REQUEST           
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)                                   
         B     ADBERR              SEND ERROR                                   
*                                                                               
P2AB     DS    0H                                                               
         CLC   LIOBMAP#,=X'0160'   "INFO" REQUEST ?                             
         BE    P2ABR               YES - DONE - SEND REPLY                      
*                                                                               
         CLC   LIOBMAP#,=X'0164'   CLEAR FOR PAY REQUEST ?                      
         BE    *+6                 YES                                          
         DC    H'0'                MUST BE ONE OF ABOVE REQUESTS                
*                                                                               
*  LINKIO PROVIDES ADDRESS AND TYPE OF "LIST" DATA - PUT IN SERLISTA            
         CLI   SERLISTA,0          ANY SERIAL NUMBERS SENT ?                    
         BNH   ISLK60              NO                                           
         MVC   BYTE,SERLISTA       SAVE LIST "TYPE"                             
         MVI   SERLISTA,0                                                       
         ICM   R6,15,SERLISTA      POINT R6 TO SERIAL# "ELEMENT"                
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE ADDRESS                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* BUILD TABLE OF SERIAL NUMBERS OF BUYS SENT FOR PAY CLEARING                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          R6 POINTING TO SERIAL# ELEMENT - SHOULD BE           
*                          ONE ONLY ELEMENT CONTAINING 1-NN SERIAL #S           
         CLI   BYTE,LQ_TSINQ      ONE VALUE ?                                   
         BNE   ISLK40             NO - MUST BE LIST OF VALUES                   
         LA    R4,SERTBL          POINT TO START OF SERIAL# TABLE               
         PACK  DUB,6(9,R6)        PACK EBCDIC DATA FROM ELEMENT                 
         MVC   0(5,R4),DUB+3      MOVE TO SERIAL # TABLE                        
         MVI   5(R4),X'FF'        END OF SERIAL NUMBER TABLE                    
         J     ISLK60             DONE WITH SERIAL#                             
*                                                                               
ISLK40   DS    0H                                                               
         CLI   BYTE,LQ_TLSTQ      LIST OF VALUES ?                              
         BE    *+6                                                              
         DC    H'0'               CAN'T BE OTHER THAN ONE VALUE OR LIST         
         OC    6(2,R6),6(R6)      SERIAL# COUNTER POSITIVE ?                    
         BNZ   *+6                                                              
         DC    H'0'               NO                                            
         ZICM  R0,6(R6),2         SERIAL# COUNT TO LOOP COUNTER                 
         CHI   R0,201                                                           
         BL    *+6                                                              
         DC    H'0'               MAXIMUM OF 200 EXCEEDED                       
         LA    R6,8(R6)           POINT TO START OF SERIAL# LIST                
         LA    R4,SERTBL          POINT TO START OF SERIAL# TABLE               
ISLK50   PACK  DUB,0(9,R6)        PACK EBCDIC DATA FROM LIST                    
         MVC   0(5,R4),DUB+3      MOVE TO SERIAL # TABLE                        
         LA    R4,5(R4)           BUMP TO NEXT SLOT IN SER# TBL                 
         LA    R6,9(R6)           BUMP TO NEXT SERIAL# AREA IN ELEM             
         BCT   R0,ISLK50                                                        
         MVI   0(R4),X'FF'        END OF SERIAL NUMBER TABLE                    
*                                                                               
ISLK60   CLI   XINV#LST,0         HAVE ADDITIONAL INVOCIES IN REQUEST?          
         JNH   ISLK70                                                           
         MVC   AXLKINV#,XINV#LST  ADDRESS OF ADDITIONAL LINK INVOICES           
         OI    LKWRKSW1,LKXVIN#Q  LINK REQUEST HAS MORE INVOICES                
*                                                                               
ISLK70   DS    0H                 FOR FUTURE INDEX ARRAYS                       
*                                                                               
ISLKEND  DS    0H                                                               
         B     PAY10               CALL PAY02 TO PROCESS                        
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*              SET UP AND SEND INFO. REPLY                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
P2ABR    DS    0H                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#PAYADR)              
*                                                                               
         CLI   PAYCK,C' '          HAVE CONTROL DATE ?                          
         BNH   P2ABR10             NO                                           
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTDTE),     *        
               ('LD_CHARQ',PAYCK),(L'PAYCK,0)                                   
P2ABR10  DS    0H                                                               
         CLI   PAYEE,C' '          HAVE PAYEE CODE ?                            
         BNH   P2ABR15             NO                                           
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PAYREP),    *        
               ('LD_CHARQ',PAYEE),(L'PAYEE,0)                                   
P2ABR15  DS    0H                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADDRL1),    *        
               ('LD_CHARQ',PAYEENM),(L'PAYEENM,0)                               
P2ABR20  DS    0H                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADDRL2),    *        
               ('LD_CHARQ',PAYADD1),(L'PAYADD1,0)                               
P2ABR25  DS    0H                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADDRL3),    *        
               ('LD_CHARQ',PAYADD2),(L'PAYADD2,0)                               
P2ABR28  DS    0H                                                               
         CLI   ADBPF11,C' '        HAVE A VALUE FOR PART PMT PROFILE ?          
         BNH   P2ABR30             NO                                           
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PARTOK),    *        
               ('LD_CHARQ',ADBPF11),(L'ADBPF11,0)                               
P2ABR30  DS    0H                                                               
         CLI   ADBPF12,C' '        HAVE A VALUE FOR SINGLY PROFILE ?            
         BNH   P2ABR35             NO                                           
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#SINGLY),    *        
               ('LD_CHARQ',ADBPF12),(L'ADBPF12,0)                               
P2ABR35  DS    0H                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CLTNAM),    *        
               ('LD_CHARQ',PAYCLNM),(L'PAYCLNM,0)                               
P2ABR40  DS    0H                                                               
         CLI   PAYPRNM,C' '        HAVE A PRODUCT NAME ?                        
         BNH   P2ABR45             NO                                           
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PRDNAM),    *        
               ('LD_CHARQ',PAYPRNM),(L'PAYPRNM,0)                               
P2ABR45  DS    0H                                                               
         CLI   PAYESNM,C' '        HAVE AN ESTIMATE NAME ?                      
         BNH   P2ABR50             NO                                           
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTNAM),    *        
               ('LD_CHARQ',PAYESNM),(L'PAYESNM,0)                               
P2ABR50  DS    0H                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBNAM),    *        
               ('LD_CHARQ',PAYPBNM),(L'PAYPBNM,0)                               
*                                                                               
         CLC   LIOBMAP#,=X'0160'   "INFO" REQUEST ?                             
         BE    ADBEND              BACK TO ADBUYER                              
*                                                                               
         DC    H'0'                                                             
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*              SET UP AND SEND ERROR REPLY                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADBERR   DS    0H                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#PAYERR)              
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRNUM),    *        
               ('LD_UBINQ',ADBERFLD),(L'ADBERFLD,0)                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRDSC),    *        
               ('LD_CHARQ',PAYMSG),(L'PAYMSG,0)                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*              CLOSE WORKER FILE AND EXIT (BACK TO ADBUYER)                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADBEND   DS    0H                                                               
         GOTOR ALINKIO,DMCB,('LIOACLO',LIOBD)       CLOSE WORKER FILE           
         BE    *+6                                                              
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
         B     EXXMOD                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*              END OF ADBUYER CALL ABOVE                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NOTADB   DS    0H                                                               
*                                                                               
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
         CLI   INVGNSW,0           OKAY IF NOT FROM INVOICE MATCH               
         BE    PAYGRSX                                                          
*                                                                               
         CLI   GNOPT,C'G'          OKAY IF GROSS WANTED                         
         BE    PAYGRSX                                                          
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',PAYAMT1H,,GLVPRNET                        
*                                                                               
         MVI   INVGNSW,C'N'        INDICATE NET AMOUNT                          
*                                                                               
PAYGRSX  DS    0H                                                               
*                                                                               
EDITHL2  DS    0H                  DISPLAY INTERFACE DATA                       
*                                                                               
         FOUT  PAYXLINH                                                         
         MVC   PAYXLIN,SPACES                                                   
*                                                                               
         CLI   PROGPROF+10,C'C'    TEST CCUSA INTERFACE ACTIVE                  
         BNE   EDITHL10                                                         
*                                                                               
         LA    R4,PAYXLIN                                                       
         LA    R5,SVXFRDTA                                                      
         USING PAYXFRD,R5                                                       
         CLI   SVXFRSW,C'N'                                                     
         BNE   *+14                                                             
         MVC   PAYXLIN(30),=C'** NO EXPENDITURE INTERFACE **'                   
         B     EDITHLX                                                          
*                                                                               
*                                                                               
         MVC   0(4,R4),=C'ACN='                                                 
         MVC   4(5,R4),XFRACN                                                   
         MVI   9(R4),C','                                                       
         LA    R4,10(R4)                                                        
*                                                                               
         MVC   0(7,R4),=C'BUDGET='                                              
         LA    R4,7(R4)                                                         
         EDIT  XFRBUDG,(8,(R4)),ALIGN=LEFT                                      
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         MVC   0(6,R4),=C'Y-T-D='                                               
         LA    R4,6(R4)                                                         
         EDIT  XFRYTD,(8,(R4)),ALIGN=LEFT                                       
         B     EDITHLX                                                          
*                                                                               
EDITHLX  DS    0H                                                               
*                                                                               
EDITHL10 CLI   PROGPROF+1,C'N'     TEST HAVE CLEARED INVS AND AMTS              
         BNE   EXIT                YES-EXIT WITH ENTER MESSAGE                  
         LA    R2,PAYAMT1H         TEST ANY AMOUNT INPUT                        
         CLI   5(R2),0                                                          
         BE    EXIT                NO- EXIT, YES EDIT                           
         CLI   INVMATSW,C'Y'       IF WE CAME FROM MATCH                        
         BNE   PAY10                                                            
*                                                                               
*        RETURN SCREEN WITH MESSAGE                                             
*                                                                               
         XC    PAYMSG,PAYMSG                                                    
         MVC   PAYMSG(21),=C'PLEASE ENTER COMMENTS'                             
         OI    PAYCMT1H+6,OI1C     INSERT CURSOR                                
         B     EXIT                DISPLAY SCREEN                               
*                                                                               
* CALL INVOICE EDIT                                                             
*                                                                               
PAY10    GOTO1 VCALLOV,DMCB,(2,0),(RA)                                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        ADBUYER CALL LOGIC FOR "CLEAR FOR PAY" REPLY BELOW                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         CLI   ADBSW,C'Y'          DOING AN ADBUYER CALL ?                      
         BNE   NOTADB2             NO                                           
*                                                                               
         L     R7,ADBWRKA          RESTORE POSSIBLY ALTERED REGISTERS           
         LA    R2,LIOBAREA                                                      
*                                  HANDLE REPLY                                 
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)   GET "END"                       
*                                                                               
         CLI   ERRAREA,0                                                        
         BNE   ADBERR              ERROR DETECTED                               
*                                                                               
*                                  OUTPUT GENERAL REPLY MAP AND DATA            
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#CLRPAY)              
         MVI   BYTE,C'Y'                                                        
*                                                                               
         CLC   =C'AMOUNTS AGREE',PAYMSG      "SUCCESSFUL" CLEARING ?            
         BE    P10AB               YES                                          
         CLC   =C'PART PAY',PAYMSG           "SUCCESSFUL" CLEARING ?            
         BE    P10AB               YES                                          
         CLC   =C'CR CHANGED',PAYMSG         "SUCCESSFUL" CLEARING ?            
         BE    P10AB               YES                                          
         CLC   =C'CK CHANGED',PAYMSG         "SUCCESSFUL" CLEARING ?            
         BE    P10AB               YES                                          
*                                                                               
         MVI   BYTE,C'N'           NO                                           
*                                                                               
P10AB    DS    0H                                                               
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PAYSUC),    *        
               ('LD_CHARQ',BYTE),(L'BYTE,0)                                     
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CLRMSG),    *        
               ('LD_CHARQ',PAYMSG),(L'PAYMSG,0)                                 
*                                                                               
         L     R5,AIOAREA          POINT TO A WORKAREA                          
         LA    R0,L'PAYMSG         MINIMUM OUTPUT LENGTH                        
*                                                                               
         MVC   0(L'PAYMSG,R5),PAYMSG COPY MESSAGE                               
         OC    0(L'PAYMSG,R5),SPACES KILL NULLS                                 
*                                                                               
         OC    PAYMSG1,PAYMSG1     SKIP IF NO FX DATA                           
         BZ    P10AB1                                                           
*                                                                               
         MVI   L'PAYMSG(R5),C'!'   LINE SEPARATOR                               
*                                                                               
         MVC   L'PAYMSG+1(L'PAYMSG,R5),PAYMSG1  FX DATA                         
         OC    L'PAYMSG+1(L'PAYMSG,R5),SPACES   KILL NULLS                      
*                                                                               
         LA    R0,2*L'PAYMSG+1     MAX MESSAGE LENGTH                           
*                                                                               
P10AB1   DS    0H                                                               
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CLRMSG),    *        
               ('LD_CHARQ',0(R5)),((R0),0)                                      
*                                                                               
         CLI   PAYCK,C' '          HAVE CONTROL DATE ?                          
         BNH   P10AB4              NO                                           
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTDTE),     *        
               ('LD_CHARQ',PAYCK),(8,0)                                         
*                                                                               
         J     P10AB8                                                           
*                                                                               
P10AB4   DS    0H                  USE "CALCULATED CONTROL DATE"                
*                                                                               
         CLI   CRCKSW,0            SKIP IF DOING CR/CK                          
         JNE   P10AB8                                                           
*                                                                               
         CLC   PAYXLIN+19(5),=C'DATE='                                          
         JNE   ADBERR                                                           
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CTDTE),     *        
               ('LD_CHARQ',PAYXLIN+25),(8,0)                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SET UP AND SEND CLEAR FOR PAY ITEMIZED REPLY RECORD(S)                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
P10AB8   CLI   ADBYTBLX,X'FF'      ANYTHING TO SEND?                            
         JNE   ADBEND                                                           
*                                                                               
         LA    R6,ADBYTBL          BUY DATA CAPTURED IN PPPAY02                 
         USING ADBYRPLY,R6                                                      
*                                                                               
P10AB10  DS    0H                  OUTPUT ITEMIZED REPLY MAP AND DATA           
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#CLRDTA)              
*                                                                               
         CLI   ABRPSER#,C' '       SERIAL#?                                     
         JNH   P10AB20                                                          
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#BYSER),     +        
               ('LD_CHARQ',ABRPSER#),(L'ABRPSER#,0)                             
*                                                                               
P10AB20  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#NETPD),     +        
               ('LD_CBINQ',ABRPAMT),(L'ABRPAMT,0)                               
*                                                                               
         CLI   FXSW,0              FX PAYMENT?                                  
         JZ    P10AB21                                                          
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#FXNETP),    +        
               ('LD_CBINQ',ABRPFX),(L'ABRPFX,0)                                 
*                                                                               
P10AB21  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBCOD),    +        
               ('LD_CHARQ',ABRPPUB),(L'ABRPPUB,0)                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PRDCOD),    +        
               ('LD_CHARQ',ABRPPRD),(L'ABRPPRD,0)                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ESTNUM),    +        
               ('LD_CHARQ',ABRPEST),(L'ABRPEST,0)                               
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#INSDAT),    +        
               ('LD_CHARQ',ABRPDAT),(L'ABRPDAT,0)                               
*                                                                               
P10ABUP  LA    R6,ABRPLNTH(R6)     NEXT BUY DATA AREA                           
         CLI   0(R6),X'FF'         END OF TABLE?                                
         JNE   P10AB10                                                          
*                                                                               
         J     ADBEND              RETURN TO ADBUYER                            
         DROP  R6                                                               
*                                                                               
NOTADB2  CLI   ERRAREA,0                                                        
         JNE   EXXMOD              EXIT IF ERROR MESSAGE PRESENT                
*                                                                               
         CLI   INVMATSW,C'Y'                                                    
         JNE   EXXMOD              EXIT IF NOT CALL FROM MATCH                  
*                                                                               
         CLC   PAYMD+1(4),=C'TEST'                                              
         JE    EXXMOD              EXIT IF RETURN FROM 'TEST' OPTION            
*                                                                               
         L     R5,AIOAREA                                                       
         XC    0(2+L'PAYMSG,R5),0(R5)                                           
         CLI   ERRAREA,0           SET ERROR RETURN CODE IF NEEDED              
         JE    *+8                                                              
         MVI   0(R5),X'FF'                                                      
*                                                                               
         MVI   1(R5),L'PAYMSG      LENGTH OF MESSAGE                            
*                                                                               
         MVC   2(L'PAYMSG,R5),PAYMSG                                            
*                                                                               
         CLC   PAYMSG(13),=C'AMOUNTS AGREE'                                     
         JNE   PAY14                                                            
         OC    CKDATE,CKDATE       CHK FOR CONTROL DATE                         
         JZ    PAY14                                                            
*                                                                               
         XC    2(L'PAYMSG,R5),2(R5)                                             
         MVC   2(15,R5),=C'CHECK TOTALS N='                                     
*                                                                               
* NET AMOUNT AND CD                                                             
*                                                                               
         MVC   17(27,R5),PAYMSG+30                                              
*                                                                               
* CONTROL DATE                                                                  
*                                                                               
         MVC   45(8,R5),=C'CONTROL='                                            
         GOTO1 VDATCON,DMCB,(3,CKDATE),(8,53(R5))                               
*                                                                               
PAY14    LA    R0,L'PAYMSG+2       DATA LENGTH                                  
         L     R5,AIOAREA                                                       
         GOTO1 VGLOBBER,DMCB,=C'PUTD',(R5),(R0),GLVPRRTN                        
*                                                                               
         XC    0(24,R5),0(R5)                                                   
         USING GLVXFRSY,R5                                                      
         MVC   GLVXFRSY,=C'PRI'    FROM THE PRINT SYSTEM                        
         MVC   GLVXFRPR,=C'PAY'    FROM THE PAY PROGRAM                         
         MVC   GLVXTOSY,=C'PRI'    TO THE PRINT SYSTEM                          
         MVC   GLVXTOPR,=C'MAT'    TO THE MATCH SYSTEM                          
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS   RETURN                              
         DROP  R5                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',(R5),24,GLVXCTL                           
*                                                                               
         SR    RF,RF                                                            
         LA    R2,PAYMSGH          POINT TO FIRST FLD ON SCREEN                 
         IC    RF,PAYMSGH          LENGTH OF 1ST FIELD ON SCREEN                
         LA    R2,0(RF,R2)         POINT TO SERVICE REQUEST FIELD               
*                                                                               
PAY20    DS    0H                                                               
*                                                                               
         B     EXXMOD                                                           
*                                                                               
CLRTST   LA    R2,PAYTSTH                                                       
         CLI   0(R2),0                                                          
         BE    CLRX                                                             
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,CLROC                                                         
         BZ    CLR1                                                             
         EX    R5,CLRXC                                                         
         FOUT  (R2)                                                             
CLR1     LA    R2,9(R5,R2)                                                      
         B     CLRTST+4                                                         
CLRX     BR    R9                                                               
CLROC    OC    8(0,R2),8(R2)                                                    
CLRXC    XC    8(0,R2),8(R2)                                                    
*                                                                               
SPACES   DC    80C' '                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* INITIALIZATION CODE                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITL    LR    R0,RE               SAVE RETURN ADDRESS                          
         LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R5,0(R1)                                                      
         ST    R2,VTIOB            SAVE ADDRESS                                 
         ST    R5,ATIA             BUFFER FOR LINKIO                            
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD          A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD          A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2)        NUMBER OF FIELDS                             
         ST    R3,VTWA             A(TWA)                                       
         MVC   VDATAMGR(44),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA)       TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CHI   R5,250                                                           
         JNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         AHI   R5,-250                                                          
         J     CLEARWRK                                                         
*                                                                               
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
*                                                                               
VARCLEAR XC    0(0,R4),0(R4)                                                    
*                                                                               
EXIT     OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
EXXMOD   DS    0H                                                               
         FOUT  PAYINV1H            ALWAYS RE-DISPLAY INVOICE #                  
         XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        FILL IN FIELDS ON SCREEN FROM GLOBBER AREA                   *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
FILSCR   NTR1                                                                   
*                                                                               
         BAS   RE,FCLCLR         CLEAR ALL UNPROTECTED FIELDS ON SCREEN         
*                                                                               
         MVI   INVMATSW,C'Y'       INDICATE CALLED BY MATCH                     
*                                                                               
         L     RF,VGLOBBER         POINT TO GLOBBER                             
*                                                                               
*        FILL IN HEADLINE FIELDS                                                
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',PAYMDH,,GLVPRMD    MEDIA                      
         GOTO1 (RF),(R1),,PAYRQH,,GLVPRREQ           REQUESTOR                  
         GOTO1 (RF),(R1),,PAYCLH,,GLVPRCLT           CLIENT                     
         GOTO1 (RF),(R1),,PAYPRH,,GLVPRPRD           PRODUCT                    
         GOTO1 (RF),(R1),,PAYPBH,,GLVPRPUB           PUB                        
         GOTO1 (RF),(R1),,PAYEEH,,GLVPRPAY           PAYEE                      
*                                                                               
         CLC   PAYEE,BLANKS        CHECK FOR NO INPUT                           
         BH    *+14                                                             
         XC    PAYEE,PAYEE         CLEAR FIELD                                  
         MVI   PAYEEH+5,0          INDICATE NO LENGTH                           
*                                                                               
         GOTO1 (RF),(R1),=C'GETD',WORK,12,GLVPRPER   DATE                       
         CLC   WORK(6),WORK+6      SEE IF START = END                           
         BNE   FCLCL5                                                           
         GOTO1 VDATCON,DMCB,(0,WORK),(5,PAYDT)                                  
         MVI   PAYDTH+4,0          KILL INPUT INDICATORS                        
         MVI   PAYDTH+5,8          SET DATA LENGTH                              
         B     FCLCLX                                                           
*                                                                               
FCLCL5   CLI   WORK+6,C'-'           SEE IF I HAVE A LINE NUMBER                
         BNE   FCLCL30                                                          
         MVI   PAYDTH+4,0          KILL INPUT INDICATORS                        
         MVI   PAYDTH+5,11         SET DATA LENGTH                              
         GOTO1 VDATCON,DMCB,(0,WORK),(5,PAYDT)                                  
         MVI   PAYDT+8,C'-'                                                     
         MVC   PAYDT+9(2),WORK+8                                                
         CLI   WORK+7,C'0'                                                      
         BNE   FCLCL10                                                          
         B     FCLCLX                                                           
*                                                                               
FCLCL10  DS    0H              SPECIAL HANDLING OF BIG LINE NUMBERS             
*                              100-109 = A0-A9                                  
*                              110-119 = B0-B9                                  
*                              ETC...                                           
*                                                                               
         LA    R1,LINTAB                                                        
FCLCL12  CLC   0(2,R1),WORK+7       MATCH FIRST 2 DIGITS                        
         BE    FCLCL9                                                           
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   FCLCL12                                                          
         B     FCLCLX              BAD LINE NUMBER                              
*                                  T40301 SHOULD DETECT THE ERROR               
*                                                                               
FCLCL9   DS    0H                                                               
         MVC   PAYDT+9(1),2(R1)                                                 
         MVC   PAYDT+10(1),WORK+9                                               
         B     FCLCLX                                                           
*                                                                               
FCLCL30  GOTO1 VDATCON,DMCB,(X'10',WORK),(17,PAYDT)  CONVERT DATE               
         MVI   PAYDTH+4,0          KILL INPUT INDICATORS                        
         MVI   PAYDTH+5,17         SET DATA LENGTH                              
*                                                                               
FCLCLX   FOUT  PAYRQH              FORCE RE-TRANSMISSION OF MEDIA               
         FOUT  PAYCLH              FORCE RE-TRANSMISSION OF CLIENT              
         FOUT  PAYPRH              FORCE RE-TRANSMISSION OF PRODUCT             
         FOUT  PAYDTH              FORCE RE-TRANSMISSION OF DATE                
         FOUT  PAYPBH              FORCE RE-TRANSMISSION OF PUB                 
         FOUT  PAYEEH              FORCE RE-TRANSMISSION OF PAYEE               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        INITIALIZE MINIO VALUES                                      *         
*                                                                     *         
***********************************************************************         
*                                                                               
FCLMIN   DS    0H                                                               
*                                                                               
         LH    RF,=Y(PYMINBLK-GENOLD) POINT TO MINIO BLOCK                      
         LA    R0,GENOLD(RF)                                                    
         LA    R1,MINBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               INIT MINIO BLOCK                             
*                                                                               
         LH    R9,=Y(PYMINBLK-GENOLD) POINT TO MINIO BLOCK                      
         LA    R9,GENOLD(R9)                                                    
         USING MINBLKD,R9                                                       
*                                                                               
         MVC   MINRECUP,VRECUP     A(RECUP)                                     
         MVC   MINCOMF,VCOMFACS    A(COMFACS)                                   
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=CL8'PRTFIL' FILE NAME                                    
         MVC   MINDIR,=CL8'PRTDIR' DIR NAME                                     
         MVI   MINFKLEN,L'PINVKEY  KEY LENGTH                                   
         MVI   MINEKLEN,L'PINVMINI   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'PINVMAST   DISPLACEMENT TO ELEMENT KEY                
         MVI   MINNCTL,2           NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=AL2(2900) MAXIMUM RECORD LENGTH                        
         MVC   MINBUFF,AIOAREA     A(BUFFER)                                    
         MVI   MINNBUF,1           USE ONE BUFFER                               
         LH    R1,=Y(PYMINTAB-GENOLD)                                           
         LA    R1,GENOLD(R1)                                                    
         ST    R1,MINRTAB          A(AREA FOR RECORD TABLE)                     
         MVC   MINRTABL,=Y(L'PYMINTAB)  LENGTH OF RECORD TABLE                  
*                                                                               
         LH    R1,=Y(PYMINELM-GENOLD)                                           
         LA    R1,GENOLD(R1)                                                    
         ST    R1,MINELEM          A(AREA FOR ELEM OR CLUSTER)                  
         MVC   MINMAXEL,=Y(L'PYMINELM)  MAX LENGTH OF ELEM OF CLUSTER           
         XC    0(L'PYMINELM,R1),0(R1)   CLEAR MINELEM AREA                      
*                                                                               
*        READ IN INVOICE RECORD                                                 
*                                                                               
         XC    MINMKEY,MINMKEY     CLEAR MASTER KEY FOR MINIO                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',MINMKEY,L'PINVKEY,GLVPRMAT INV            
*                                                                               
         LA    R4,MINMKEY                                                       
         USING PINVKEY,R4                                                       
*                                                                               
         MVC   MINEKEY(L'PINVMINI),PINVMINI   ELEMENT KEY                       
*                                                                               
         GOTO1 VMINIO,DMCB,('MINRD',MINBLKD)   READ INVOICE                     
         CLI   MINERR,0            NO ERRORS ALLOWED FOR NOW                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,MINELEM          POINT TO FOUND ELEMENT                       
         USING PIMHDREL,R3         ESTABLISH AS INVOICE HEADER                  
*                                                                               
         MVC   INVSTAT,PIMSTAT     SAVE INVOICE STATUS BYTE                     
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PIMINVDT),(17,PAYINVD)  INVOICE DATE             
         FOUT  PAYINVDH            FORCE TRANSMISSION                           
         MVI   PAYINVDH+5,8        SET LENGTH                                   
*                                                                               
         XC    PAYINV1,PAYINV1     INIT SCREEN INVOICE NUMBER                   
         MVC   PAYINV1,PIMINVNO    FILL IN INVOICE NUMBER                       
         FOUT  PAYINV1H            FORCE RE-TRANSMISSION                        
         MVI   PAYINV1H+5,11       SET FOR MAX LENGTH                           
         OI    PAYINV1H+1,X'01'    TURN ON MODIFIED TAG                         
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETF',PAYAMT1H,,GLVPRGRS                        
*                                                                               
         GOTO1 (RF),(R1),=C'DELE'  DELETE FROM GLOBBER AREA                     
*                                                                               
*        GET THEN DELETE RETURN ELEM                                            
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,16,GLVXCTL                           
*                                                                               
         GOTO1 (RF),(R1),=C'DELE',,,GLVXCTL                                     
*                                                                               
         FOUT  PAYAMT1H            FORCE RE-TRANSMISSION                        
         MVI   PAYAMT1H+5,12       MAX LENGTH                                   
*                                                                               
         MVI   INVGNSW,C'G'        AMOUNT HAS GROSS IN IT                       
*                                                                               
FILSCRX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*               CLEAR ALL UNPROTECTED FIELDS ON SCREEN                *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FCLCLR   NTR1                                                                   
*                                                                               
         LA    R2,PAYMDH           POINT TO FIRST FIELD ON SCREEN               
         SR    RF,RF                                                            
*                                                                               
FCLCLRLP DS    0H                                                               
*                                                                               
         TM    1(R2),X'20'         SKIP IF PROTECTED FIELD                      
         BO    FCLCLRCN                                                         
*                                                                               
         ICM   RF,1,0(R2)          TOTAL FIELD LENGTH                           
         BZ    FCLCLRDN            DONE IF SCREEN END REACHED                   
*                                                                               
         SH    RF,=H'8'            HEADER LENGTH                                
         TM    1(R2),X'02'         SKIP IF NOT EXTENDED HEADER                  
         BNO   *+8                                                              
         SH    RF,=H'8'            EXTENSION LENGTH                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
*                                                                               
         FOUT  (R2)                FORCE RE-TRANSMISSION                        
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
*                                                                               
FCLCLRCN DS    0H                                                               
*                                                                               
         IC    RF,0(R2)            BUMP TO NEXT FIELD ON SCREEN                 
         LA    R2,0(RF,R2)                                                      
         B     FCLCLRLP                                                         
*                                                                               
FCLCLRDN DS    0H                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
LINTAB   DS    0C                                                               
         DC    C'10',C'A'                                                       
         DC    C'11',C'B'                                                       
         DC    C'12',C'C'                                                       
         DC    C'13',C'D'                                                       
         DC    C'14',C'E'                                                       
         DC    C'15',C'F'                                                       
         DC    C'16',C'G'                                                       
         DC    C'17',C'H'                                                       
         DC    C'18',C'I'                                                       
         DC    C'19',C'J'                                                       
         DC    C'20',C'K'                                                       
         DC    C'21',C'L'                                                       
         DC    C'22',C'M'                                                       
         DC    C'23',C'N'                                                       
         DC    C'24',C'O'                                                       
         DC    C'25',C'P'                                                       
         DC    X'FFFFFF'                                                        
         LTORG                                                                  
*                                                                               
         DROP  R3,R4,R9                                                         
         EJECT                                                                  
*                                                                               
MAP      DS    0XL(LIORL)                                                       
         DC    AL2(M#DLRPA,E#PAYADR,DATA1-MAP)    REFRESH PAY INFO.             
         DC    AL2(M#ULCLRP,E#CLRPAY,DATA2-MAP)   CLEAR FOR PAY                 
MAPX     DC    AL2(0)                                                           
*                                                                               
DATA1    DS    0XL(LIODL)                       REFRESH PAY INFORMATION         
*                                                                               
         DC    AL2(D#MEDCOD),AL1(LIOBSB2Q)      MEDIA CODE                      
         DC    AL2(PAYMDH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#REQID),AL1(LIOBSB2Q)       REQUESTOR                       
         DC    AL2(PAYRQH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#CLTCOD),AL1(LIOBSB2Q)      CLIENT CODE                     
         DC    AL2(PAYCLH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PRDCOD),AL1(LIOBSB2Q)      PRODUCT CODE                    
         DC    AL2(PAYPRH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#STEND),AL1(LIOBSB2Q)       START-END DATE                  
         DC    AL2(PAYDTH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)      PUB CODE                        
         DC    AL2(PAYPBH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVDAT),AL1(LIOBSB2Q)      INVOICE DATE                    
         DC    AL2(PAYINVDH-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PAYREP),AL1(LIOBSB2Q)      PAYING REP                      
         DC    AL2(PAYEEH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
DATA1X   DC    AL2(0)                                                           
*                                                                               
DATA2    DS    0XL(LIODL)                       CLEAR FOR PAY                   
*                                                                               
         DC    AL2(D#MEDCOD),AL1(LIOBSB2Q)      MEDIA CODE                      
         DC    AL2(PAYMDH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#REQID),AL1(LIOBSB2Q)       REQUESTOR                       
         DC    AL2(PAYRQH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#CLTCOD),AL1(LIOBSB2Q)      CLIENT CODE                     
         DC    AL2(PAYCLH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PRDCOD),AL1(LIOBSB2Q)      PRODUCT CODE                    
         DC    AL2(PAYPRH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#STEND),AL1(LIOBSB2Q)       START-END DATE                  
         DC    AL2(PAYDTH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PUBCOD),AL1(LIOBSB2Q)      PUB CODE                        
         DC    AL2(PAYPBH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PAYREP),AL1(LIOBSB2Q)      PAYING REP                      
         DC    AL2(PAYEEH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#CTDTE),AL1(LIOBSB2Q)       CONTROL DATE                    
         DC    AL2(PAYCKH-T403FFD),AL1(0)                                       
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVDAT),AL1(LIOBSB2Q)      INVOICE DATE                    
         DC    AL2(PAYINVDH-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#PAYIND),AL1(LIOBSB2Q)      SINGLY OPTION                   
         DC    AL2(ADBSING-T403FFD),AL1(L'ADBSING)                              
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#PARTPY),AL1(LIOBSB2Q)      PARTIAL PAYMENT OPTION          
         DC    AL2(ADBPART-T403FFD),AL1(L'ADBPART)                              
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#CSHDSC),AL1(LIOBSB2Q)      CASH DISCOUNT OPTION            
         DC    AL2(ADBNOCD-T403FFD),AL1(L'ADBNOCD)                              
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
         DC    AL2(D#CRCKOP),AL1(LIOBSB2Q)      A(CRCK OPTION)                  
         DC    AL2(ADBCRCKA-T403FFD),AL1(0)                                     
         DC    AL1(LIODINDX,0)                  END OF HEADER DATA              
*                                                                               
         DC    AL2(D#GPSTOV),AL1(LIOBSB2Q)      A(GST/PST OVERRIDES)            
         DC    AL2(ADBGPSTA-T403FFD),AL1(0)                                     
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#OVPROV),AL1(LIOBSB2Q)      OVERRIDE OPTION                 
         DC    AL2(ADBOVRD-T403FFD),AL1(L'ADBOVRD)                              
         DC    AL1(LIODISFF,LIODIRET)                                           
*                                                                               
*                                                    DATA BREAK                 
*                                                                               
         DC    AL2(D#BYSER),AL1(LIOBSB1Q)       A(BUY SERIAL #'S LIST)          
         DC    AL2(SERLISTA-ADBWRKD),AL1(0)                                     
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#VINVNO),AL1(LIOBSB2Q)      INVOICE NUMBER 1                
         DC    AL2(PAYINV1H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#AMTCLR),AL1(LIOBSB2Q)      AMOUNT TO CLEAR 1               
         DC    AL2(PAYAMT1H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#REMCOM),AL1(LIOBSB2Q)      COMMENT 1                       
         DC    AL2(PAYCMT1H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#VINVN2),AL1(LIOBSB2Q)      INVOICE NUMBER 2                
         DC    AL2(PAYINV2H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#AMTCL2),AL1(LIOBSB2Q)      AMOUNT TO CLEAR 2               
         DC    AL2(PAYAMT2H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#REMCO2),AL1(LIOBSB2Q)      COMMENT 2                       
         DC    AL2(PAYCMT2H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#VINVN3),AL1(LIOBSB2Q)      INVOICE NUMBER 3                
         DC    AL2(PAYINV3H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#AMTCL3),AL1(LIOBSB2Q)      AMOUNT TO CLEAR 3               
         DC    AL2(PAYAMT3H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#REMCO3),AL1(LIOBSB2Q)      COMMENT 3                       
         DC    AL2(PAYCMT3H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#VINVN4),AL1(LIOBSB2Q)      INVOICE NUMBER 4                
         DC    AL2(PAYINV4H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#AMTCL4),AL1(LIOBSB2Q)      AMOUNT TO CLEAR 4               
         DC    AL2(PAYAMT4H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#REMCO4),AL1(LIOBSB2Q)      COMMENT 4                       
         DC    AL2(PAYCMT4H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#VINVN5),AL1(LIOBSB2Q)      INVOICE NUMBER 5                
         DC    AL2(PAYINV5H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#AMTCL5),AL1(LIOBSB2Q)      AMOUNT TO CLEAR 5               
         DC    AL2(PAYAMT5H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#REMCO5),AL1(LIOBSB2Q)      COMMENT 5                       
         DC    AL2(PAYCMT5H-T403FFD),AL1(0)                                     
         DC    AL1(LIODITFH,0)                                                  
*                                                                               
         DC    AL2(D#INVNUM),AL1(LIOBSB1Q)      A(ADDITIONAL INV#S)             
         DC    AL2(XINV#LST-ADBWRKD),AL1(0)                                     
         DC    AL1(LIODINDX,0)                                                  
*                                                                               
         DC    AL2(D#INVSRC),AL1(LIOBSB2Q)      SOURCE OF PAYMENT               
         DC    AL2(PYSOURCE-T403FFD),AL1(L'PYSOURCE)                            
         DC    AL1(LIODISFF,0)                                                  
*                                                                               
DATA2X   DC    AL2(0)                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
                                                                                
ADBWRKC  CSECT                                                                  
         DC    (ADBWRKX-ADBWRKD)X'00'                                           
*                                                                               
                                                                                
ADBIO    CSECT                                                                  
         DC    14000X'00'                                                       
*                                                                               
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
       ++INCLUDE PPPAYWRK                                                       
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
       ++INCLUDE PINVREC                                                        
         EJECT                                                                  
PBILLRCD DSECT                     BILL RECORD                                  
PBILKIDQ EQU   X'08'               BILL RECORD ID                               
       ++INCLUDE PBILLREC                                                       
         EJECT                                                                  
       ++INCLUDE PPPAYFFD                                                       
         EJECT                                                                  
       ++INCLUDE PPPAYTWA                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
                                                                                
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012PPPAY00   10/14/20'                                      
         END                                                                    
