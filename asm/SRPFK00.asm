*          DATA SET SRPFK00    AT LEVEL 007 AS OF 08/22/00                      
*PHASE T10B00A                                                                  
         TITLE '$PFK - HANDLE PFKS'                                             
         PRINT NOGEN                                                            
PFK      CSECT                                                                  
         NMOD1 SRWORKX-SRWORKD,**$PFK**,RR=R4                                   
         USING SRWORKD,RC                                                       
         USING SRPARMD,R1                                                       
         ST    R4,RELO                                                          
         ST    R1,SAVER1                                                        
         MVC   ATIA,SRQATIA        SAVE A(TIA)                                  
*                                                                               
         L     R2,SRQATWA          R2=A(TWA)                                    
         USING SRPFKFFD,R2                                                      
         L     R9,SRQASYSF         R9=A(SYSFACS)                                
         USING SYSFACD,R9                                                       
         L     RF,VSYSFAC0         A(COMFACS)                                   
         L     RF,CGETTXT-COMFACSD(RF)                                          
         STCM  RF,15,AGETTXT                                                    
         L     RF,VSSB             GET SYSTEM ID                                
         MVC   SYSID,SSBSYSID-SSBD(RF)                                          
         MVC   DELIM,SSBSYSCH-SSBD(RF)                                          
         MVC   RECLEN,SSBTWAL-SSBD(RF)                                          
         L     R8,SRQAUTL          R8=A(UTL ENTRY)                              
         USING UTLD,R8                                                          
         MVC   LANG,TLANG          SAVE TERMINAL LANGUAGE                       
         L     RF,SRQATIOB         A(TIOB) TO SET BELL ON                       
         OI    TIOBINDS-TIOBD(RF),TIOBALRM (GETETXT MAT NOT BE CALLED)          
         DROP  R1                                                               
*                                                                               
         MVI   STDMSG,0            INIT TO NON STD MSG                          
         MVC   SAVSRV,SRVSRV       SAVE S/R INPUT FIELD                         
         XC    SRVSRV,SRVSRV                                                    
         MVC   SRVSRV(3),=C'=CT'   SET TO CONNECT                               
         MVI   OKMSG,1             PFK DISPLAYED                                
         OC    TUSER,TUSER                                                      
         BZ    PFK1                                                             
         MVC   SRVSRV(3),=C'=RE'   SET TO RECALL IF ALREADY CONNECTED           
         MVI   OKMSG,2             PFK DISP ENTER TO RECALL SCREEN              
         EJECT                                                                  
***********************************************************************         
* TEST TO SEE WHY WE HAVE BEEN CALLED.                                *         
* IF =PFK,01 THRU =PFK,12 THEN CALLER WAS MSGQIN BECAUSE THE INPUT PFK*         
* WAS NOT IN ITS TABLE OF VALID EQUATABLE VALUES.                     *         
* ANY OTHER VALUE WE WILL ASSUME WAS TYPED INTO THE S/R FIELD AND WE  *         
* WILL JUST DISPLAY THE PFK TABLE.                                    *         
***********************************************************************         
         SPACE 1                                                                
PFK1     LA    R4,SRVSRVH          EXAMINE S/R FIELD FOR =PFK,NN                
         XC    APFKTAB,APFKTAB                                                  
         CLI   SAVSRV+4,C','                                                    
         BNE   PFKDIS              ASSUME INPUT FROM TERM AND DISPLAY           
         LA    RE,PFKTAB                                                        
         SPACE 1                                                                
PFK2     CLC   0(2,RE),=X'FFFF'    SEARCH TABLE OF VALUES                       
         BE    PFKDIS                                                           
         CLC   0(2,RE),SAVSRV+5                                                 
         BE    *+12                                                             
         LA    RE,L'PFKTAB(RE)                                                  
         B     PFK2                                                             
         ST    RE,APFKTAB          SAVE ADDRESS OF ENTRY                        
*&&US*&& B     PFKDIS              IN US, PFK1 ALWAYS DISPLAYS PFK LIST         
         SPACE 1                                                                
PFK3     CLC   0(2,RE),=C'01'      TEST FOR PFK1 - THE HELP KEY                 
         BNE   PFKDIS              DISPLAY PFK LIST                             
         OC    TUSER,TUSER         TEST IF TERM IS CONNECTED                    
         BZ    PFKDIS              NO THEN DISPLAY PFK LIST                     
         EJECT                                                                  
***********************************************************************         
* PFK1 WAS ENTERED - TRY TO EXPAND LINE ONE MESSAGE INFO.             *         
* MESSAGE REFERENCE MUST BE PRESENT. FORMAT XY/9999 OR XY99999        *         
* WHERE X=VALID MESSAGE TYPE. Y=VALID SYSTEM CODE. /=SYSTEM DELIMITER *         
***********************************************************************         
         SPACE 1                                                                
PFKHLP   XC    DMCB(24),DMCB       SET TEMPSTR PAGE=0                           
         MVC   DMCB+10(2),TNUM     SET TERMINAL NUMBER                          
         L     R7,ATIA                                                          
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(R7)                       
         CLI   8(R1),0                                                          
         BNE   PFKDIS              CAN'T READ TEMP STORE                        
*                                                                               
PFKHLP1  ST    R7,ATMPSTR          SAVE A(TEMP STORE)                           
         LA    R7,64(R7)           POINT TO TWA MSG FIELD                       
         TM    1(R7),X'20'         FIRST FIELD MUST BE PROTECTED                
         BZ    PFKDIS              CAN'T BE A STANDARD TWA                      
         LA    R1,8                                                             
         TM    1(R7),X'02'         TEST EXTENDED FIELD HDR                      
         BZ    *+8                                                              
         LA    R1,16                                                            
         SR    RF,RF                                                            
         IC    RF,0(R7)                                                         
         SR    RF,R1                                                            
         CH    RF,=H'60'           FIRST FIELD MUST BE 60 BYTES LONG            
         BNE   PFKDIS              CAN'T BE A STANDARD TWA                      
         ST    R7,AMSGHDR          SAVE A(MESSAGE HDR)                          
         LA    R7,8(R7)                                                         
*                                                                               
PFKHLP2  MVI   WORK,X'F0'          DEFINE A ZERO FILLED FIELD FOR MVZ           
         MVC   WORK+1(9),WORK                                                   
         LA    R3,2(R7)            TEST IF GETTXT STANDARD REFERENCE            
         LA    R1,4                EX L'MESSAGE NUMBER                          
         CLC   0(1,R3),DELIM       MAY BE XX/9999 OR XX99999                    
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         LA    R3,1(R3)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),0(R3)                                                    
         CLC   WORK(5),WORK+5      STILL F0'S THEN IT'S NUMERIC                 
         BNE   PFKNOHLP            ELSE JUST RESTORE APPLICATION TWA            
         EX    R1,*+8              CONVERT MESSAGE NUMBER TO BINARY             
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1               IGNORE IF MESSAGE ZERO                       
         BZ    PFKNOHLP                                                         
*                                                                               
         STCM  R1,3,MSGNO                                                       
*                                                                               
         LA    R1,SYSTAB           TABLE OF SYSTEM ID'S                         
         LA    RF,L'SYSTAB-1(R1)   A(EOT)                                       
         LA    RE,L'SYSCODE        L'ENTRY                                      
         CLC   1(1,R7),0(R1)                                                    
         BE    *+12                FOUND SYSTEM                                 
         BXLE  R1,RE,*-10                                                       
         B     PFKNOHLP            NOT A VALID SYSTEM I.D.                      
*                                                                               
         MVI   SYSCODE,X'FF'       GENERAL SYSTEM MESSAGES (SYS0)               
         LA    R0,SYSTAB                                                        
         SR    R1,R0               R1=SYSTEM CODE                               
         BZ    *+8                                                              
         STC   R1,SYSCODE                                                       
*                                                                               
         LA    R1,MSGTAB           R1=A(MESSAGE TYPE TABLE)                     
PFKHLP4  CLI   0(R1),0             SCAN FOR MESSAGE TYPE                        
         BE    PFKNOHLP            NOT A VALID MESSAGE TYPE                     
         CLC   0(L'MSGCODE,R7),0(R1)                                            
         BE    *+12                                                             
         LA    R1,L'MSGCODE(R1)                                                 
         B     PFKHLP4                                                          
*                                                                               
         MVC   MSGCODE,0(R1)                                                    
         MVI   STDMSG,C'Y'         FLAG AS A STD MESSAGE                        
         EJECT                                                                  
***********************************************************************         
* GET MESSAGE AND INSERT FULL TEXT IN SERVICE REQUEST TWA             *         
***********************************************************************         
         SPACE 1                                                                
PFKHLP5  LA    R3,DMCB             DEFINE GETTXT CONTROL BLOCK                  
         USING GETTXTD,R3                                                       
         XC    DMCB(L'GTBLOCK),DMCB                                             
         MVC   GTMSGNO,MSGNO                                                    
         MVC   GTMTYP,MSGCODE                                                   
         MVC   GTMSYS,SYSCODE                                                   
         MVC   GTMLANG,LANG                                                     
         OI    GT1INDS,GT1REF+GT1OHDR                                           
         OI    GT2INDS,GT2SRPF1    INFORM GETTXT SPECIAL SVC CALL               
*                                                                               
         GOTO1 AGETTXT,GETTXTD                                                  
         SPACE 1                                                                
         TM    GT1INDS,GT1RETM0    IGNORE IF RETURNED WITH MSG ZERO             
         BO    PFKNOHLP                                                         
*                                                                               
         ICM   R3,7,GTAIO          A(IOAREA)-SPECIAL CALL ONLY                  
         DROP  R3                                                               
         LA    R3,GMSGEL-GMSGD(R3) A(SHORT MESSAGE ELEMENT)                     
         ZIC   R1,GMSGELL-GMSGEL(R3)                                            
PFKHLP6  AR    R3,R1               SCAN IO AREA FOR TEXT ELEMENT                
         ICM   R1,1,1(R3)                                                       
         BZ    PFKNOHLP            NO EXPANDED TEXT ELEMENT                     
         CLI   0(R3),GMTXTELC                                                   
         BNE   PFKHLP6                                                          
         USING GMTXTD,R3                                                        
*                                                                               
         CLC   GMTXTLIN(5),DC@HELP                                              
         BE    HV1HELP             GO GET FULL PANEL HELP                       
*                                                                               
         LA    R6,SRVLST1H                                                      
         USING DISPLD,R6                                                        
         SPACE 1                                                                
PFKHLP7  ZIC   R1,GMTXTLNO         RELATIVE LINE NUMBER                         
         CLI   GMTXTLNO,MAXLIN     CHECK FOR ENOUGH LINES ON SCREEN             
         BH    EXIT                GIVE UP IF SCREEN FULL                       
         MVC   HIGHNO,GMTXTLNO     SAVE HIGHEST SO FAR (IN EL SEQ)              
         BCTR  R1,0                OFFSET LINE NUMBER                           
         MH    R1,LINELEN          X LINE LENGTH                                
         AR    R1,R6               R1=A(RELEVANT LINE HEADER)                   
         ZIC   RF,GMTXTELL                                                      
         SH    RF,=Y(GMTXTFXD+1)   RF=EX L'TEXT                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLEXPTXT-DISPLD(0,R1),GMTXTLIN                                   
*                                                                               
         ZIC   R1,GMTXTELL                                                      
         AR    R3,R1               SCAN IO AREA FOR TEXT ELEMENT                
         ICM   R1,1,GMTXTELL                                                    
         BZ    *+12                DONE                                         
         CLI   GMTXTEL,GMTXTELC                                                 
         BE    PFKHLP7             MOVE IN NEXT LINE                            
         SPACE 1                                                                
*        BUILD A BOX AROUND THE TEXT                                            
*                                                                               
         LA    R6,SRVTLSH          A(TOP LEFT STAR HDR)                         
         MVI   DLLSTAR,C'*'        GENERATE TOP LINE OF '* * *'                 
         MVI   DLRSTAR,C'*'                                                     
         MVI   DLDATA,C'*'                                                      
         MVC   DLDATA+2(L'DLDATA-2),DLDATA                                      
*                                                                               
         LA    R6,SRVLST1H         PUT BOX CHRS AT START/END OF LINE            
         ZIC   RF,HIGHNO           HIGHEST LINE USED                            
         BCTR  RF,0                                                             
         MH    RF,LINELEN          DISP TO LAST LINE                            
         AR    RF,R6               A(LAST USED LINE)                            
         LH    RE,LINELEN                                                       
         MVI   DLLSTAR,C'*'                                                     
         MVI   DLRSTAR,C'*'                                                     
         BXLE  R6,RE,*-8           BXLE ENDS WITH R6=A(1ST BLANK LINE)          
*                                                                               
         OI    DLHDR2+1,X'08'      SWITCH ON HIGH INTENSITY                     
         STCM  R6,15,ALSTHLP       SAVE A(LAST LINE USED)                       
         MVI   DLLSTAR,C'*'                                                     
         MVI   DLRSTAR,C'*'                                                     
         MVI   DLDATA,C'*'                                                      
         MVC   DLDATA+2(L'DLDATA-2),DLDATA                                      
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ATTEMPT TO MERGE SERVICE REQUEST SCREEN ONTO APPLICATION TWA        *         
* FIRST ATTEMPT TO MERGE FROM BOTTOM UP, THEN TOP DOWN ELSE JUST      *         
* LEAVE SERVICE REQUEST SCREEN                                        *         
***********************************************************************         
         SPACE 1                                                                
PFKMRG   LA    R1,24               DEFINE 1ST HLP ROW (ROW-1)*80+COL-1          
         ZIC   RF,HIGHNO           TOTAL NO HELP LINES O/P                      
         LA    RF,2(RF)            +BOXES                                       
         SR    R1,RF               RELATIVE START LINE FOR HELP                 
         MH    R1,=H'80'           TWA HDR FORMAT ROW N COL 1                   
         STCM  R1,3,HLPRELR                                                     
         SR    R1,R1                                                            
         XC    CURPOSN,CURPOSN                                                  
         L     R7,AMSGHDR          SCAN APPLIC TWA FOR CURSOR POSN              
*                                                                               
PFKMRG1  TM    6(R7),X'40'         POSN CURSOR NEXT TIME                        
         BZ    *+8                                                              
         STCM  R7,15,CURPOSN       SAVE A(LAST CURSOR POSN HEADER)              
         ICM   R1,1,0(R7)          L'FIELD                                      
         BZ    PFKMRG2                                                          
         AR    R7,R1                                                            
         CLI   0(R7),0             END OF TWA                                   
         BNE   PFKMRG1                                                          
         SPACE 1                                                                
PFKMRG2  ICM   R7,15,CURPOSN       FIND CURSOR POSN                             
         BZ    PFKMRGX             CAN'T MERGE IF IT'S NOT DEFINED              
         CLC   HLPRELR,2(R7)       CHECK CURSOR IS BEFORE 1ST HELP LINE         
         BH    PFKMRGB             ENOUGH ROOM TO MERGE AT BOTTOM               
         SPACE 1                                                                
         ZIC   RF,HIGHNO           SEE IF THERE IS ROOM ABOVE                   
         LA    RF,2(RF)            +BOXES=LAST RELATIVE LINE REQUIRED           
         ICM   R1,3,2(R7)                                                       
         SR    R0,R0                                                            
         D     R0,=F'80'           CURSOR RELATIVE ROW NUMBER                   
         CR    R1,RF                                                            
         BNH   PFKMRGX             CAN'T MERGE ABOVE CURSOR                     
         EJECT                                                                  
***********************************************************************         
* MERGE AT TOP OF SCREEN                                              *         
* 1ST MOVE HELP LINES TO REL LINE 1 WITHIN SERVICE REQUEST TWA        *         
* 2ND FIND FIRST LINE OF APPLICATION TWA TO MOVE IN FROM TIA          *         
* 3RD MOVE REMAINING APPLICATION TWA FROM TIA TO TWA                  *         
***********************************************************************         
         SPACE 1                                                                
PFKMRGT  LA    R4,80               DEFINE ROW 2 AS START ROW                    
         LH    RE,LINELEN                                                       
         ICM   RF,15,ALSTHLP       A(LAST LINE TO XFR)                          
         LA    R6,SRVTLSH          MOVE FROM TOP LEFT STAR                      
         LA    R7,SRVP1H           MOVE HELP AFTER SRVSRV                       
         USING DISPLD,R7                                                        
*                                                                               
PFKMT10  MVC   DISPLD(DISPLLEN),0(R6)                                           
         LA    R1,DLCOL1           RELATIVE COLUMN NUMBER                       
         AR    R1,R4               TWA FORMAT ROW/COLUMN                        
         STCM  R1,3,DLHDR1+2                                                    
         LA    R1,DLCOL2                                                        
         AR    R1,R4                                                            
         STCM  R1,3,DLHDR2+2                                                    
         LA    R1,DLCOL3                                                        
         AR    R1,R4                                                            
         STCM  R1,3,DLHDR3+2                                                    
         LA    R4,80(R4)           NEXT RELATIVE ROW NUMBER                     
         AR    R7,RE                                                            
         BXLE  R6,RE,PFKMT10       DROPS THRU WITH R7&R4 SET TO NEXT            
         DROP  R7                                                               
         SPACE 1                                                                
         L     R6,AMSGHDR          FIND FIRST APPL TWA FIELD TO MOVE IN         
         SR    R1,R1                                                            
PFKMT20  IC    R1,0(R6)                                                         
         AR    R6,R1                                                            
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                LOGIC BUG-CAN'T REACH END OF APP TWA         
         CLM   R4,3,2(R6)          CHECK IF WE'VE REACHED REQUIRED LINE         
         BH    PFKMT20                                                          
         SPACE 2                                                                
PFKMT30  ICM   R1,1,0(R6)          L'FIELD TO MOVE                              
         BZ    PFKMT40             DONE                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R6)       MOVE HDR+FIELD FROM TIA TO TWA               
         LA    R7,1(R1,R7)         NEXT TWA ENTRY                               
         LA    R6,1(R1,R6)         NEXT TIA ENTRY                               
         B     PFKMT30                                                          
         SPACE 1                                                                
PFKMT40  XC    0(3,R7),0(R7)       DEFINE END OF TWA                            
         L     R1,AMSGHDR          MOVE TOP 2 FIELDS BACK FROM TIA              
         MVC   SRVMSG,8(R1)                                                     
         ZIC   RF,0(R1)                                                         
         AR    R1,RF               A(SRV HDR)                                   
         MVC   SRVSRV,8(R1)                                                     
         B     PFKMRGD                                                          
         EJECT                                                                  
***********************************************************************         
* MERGE AT BOTTOM OF SCREEN                                           *         
* FIND THE LAST LINE TO LEAVE ON SCREEN AND MERGE REST IN             *         
***********************************************************************         
         SPACE 1                                                                
PFKMRGB  SR    R1,R1                                                            
*                                                                               
PFKMB10  IC    R1,0(R7)            FIND FIRST FIELD TO OVERWRITE                
         AR    R7,R1                                                            
         CLI   0(R7),0                                                          
         BE    PFKMB20                                                          
         CLC   HLPRELR,2(R7)       CHECK IF WE'VE REACHED REQUIRED LINE         
         BH    PFKMB10                                                          
*                                                                               
PFKMB20  LA    R6,SRVTLSH          MOVE TO TIA FROM TWA                         
         SR    R4,R4               DEFINE ROW/COLUMN                            
         ICM   R4,3,HLPRELR        RELATIVE ROW NUMBER/COL1                     
         LH    RE,LINELEN                                                       
         ICM   RF,15,ALSTHLP       A(LAST LINE TO XFR)                          
         USING DISPLD,R7           R7=START POINT IN TIA TO MOVE TO             
*                                                                               
PFKMB30  MVC   DISPLD(DISPLLEN),0(R6)                                           
         LA    R1,DLCOL1           RELATIVE COLUMN NUMBER                       
         AR    R1,R4               TWA FORMAT ROW/COLUMN                        
         STCM  R1,3,DLHDR1+2                                                    
         LA    R1,DLCOL2                                                        
         AR    R1,R4                                                            
         STCM  R1,3,DLHDR2+2                                                    
         LA    R1,DLCOL3                                                        
         AR    R1,R4                                                            
         STCM  R1,3,DLHDR3+2                                                    
         LA    R4,80(R4)           NEXT RELATIVE ROW NUMBER                     
         AR    R7,RE                                                            
         BXLE  R6,RE,PFKMB30                                                    
         DROP  R7                                                               
         SPACE 1                                                                
         XC    0(3,R7),0(R7)       DEFINE END OF SCREEN                         
         L     RE,ATMPSTR          NOW NOW THE TIA TO THE TWA                   
         LR    R0,R2               R0=A(TWA)                                    
         ICM   R1,15,LTWA          LENGTH OF TWA                                
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         SPACE 1                                                                
PFKMRGD  OI    TFLAG,TFLAGHLP      FORCE FULL SCREEN WRITE NEXT TIME            
*                                                                               
PFKMRGX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* NO EXTENDED HELP AVAILABLE SO RESTORE APPLICATION TWA               *         
***********************************************************************         
         SPACE 1                                                                
PFKNOHLP L     RE,ATMPSTR          OVERWRITE TWA WITH APPLICATION TWA           
         LR    R0,R2               R0=A(TWA)                                    
         ICM   R1,15,LTWA          LENGTH OF TWA                                
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         CLI   STDMSG,C'Y'         PUT OUT 'NO HELP' IF STD MESSAGE             
         BNE   EXIT                                                             
*                                                                               
         ZIC   RF,LANG             GET CONNECT LANGUAGE                         
         LA    R1,L'NOHELP                                                      
         MR    R0,RF               DISP TO 'NO HELP' FOR LANGUAGE               
         LA    RF,NOHELP                                                        
         AR    RF,R1               A('NO HELP' FOR LANGUAGE)                    
         XC    SRVMSG+7(L'SRVMSG-7),SRVMSG+7 CLEAR MESSAGE                      
         MVC   SRVMSG+8(L'NOHELP),0(RF)                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST OF PFK VALUES AND HELP TEXT                            *         
***********************************************************************         
         SPACE 1                                                                
PFKDIS   LA    RE,2                GENERATE '* * * ETC' TOP LINE                
         LA    R6,SRVTLSH          A(TOP LEFT STAR HDR)                         
         USING DISPLD,R6                                                        
         LA    RF,L'SRVL1-1(R6)    A(LAST CHR IN LINE)                          
         MVI   DLLSTAR,C'*'                                                     
         MVI   DLRSTAR,C'*'                                                     
         MVI   DLDATA,C'*'                                                      
         BXLE  R6,RE,*-4                                                        
*                                                                               
         LA    R6,SRVLST1H         R6=A(DISPLAY LINE)                           
         LA    R7,PFKTAB           R7=A(PFK TABLE ENTRY)                        
         USING PFKTABD,R7                                                       
         LA    R3,MAXLIN           TOTAL LINES ALLOWED                          
*                                                                               
PFKDIS1  CLC   PFKID,=X'FFFF'      TEST END OF TABLE                            
         BE    PFKDIS4                                                          
         MVI   DLLSTAR,C'*'                                                     
         MVI   DLRSTAR,C'*'                                                     
         MVC   DLPFKNUM,PFKNUM     SET PFK NUMBER                               
         MVC   DLPFKSRV,PFKSRV     SET SERVICE DATA                             
         TM    PFKFLG1,PFKND       DISPLAY PFK TEXT IF KEY IS DEFINED           
         BZ    PFKDIS2                                                          
         C     R7,APFKTAB          DISPLAY NOT DEFINE ON KEY ENTERED            
         BNE   PFKDIS3                                                          
*                                                                               
PFKDIS2  C     R7,APFKTAB          HIGHLIGHT ENTERED VALUE                      
         BNE   *+8                                                              
         OI    DLHDR2+1,X'08'      SWITCH ON HIGH INTENSITY                     
         ICM   RE,15,PFKTXTA       GET A(TEXT FIELDS)                           
         BZ    PFKDIS3                                                          
         A     RE,RELO                                                          
         SR    RF,RF               GET LANGUAGE CODE                            
         IC    RF,LANG                                                          
         SLL   RF,2                INDEX INTO ADCON LIST                        
         L     RF,0(RE,RF)                                                      
         LTR   RF,RF                                                            
         BZ    PFKDIS3                                                          
         A     RF,RELO                                                          
         MVC   DLPFKTXT,0(RF)      SET TEXT FOR APPROPRIATE LANGUAGE            
PFKDIS3  LA    R7,L'PFKTAB(R7)                                                  
         LA    R6,SRVL2H-SRVL1H(R6)                                             
         BCT   R3,PFKDIS1                                                       
         B     PFKDISX             NO ROOM FOR BOTTOM BOX                       
*                                                                               
PFKDIS4  LA    RE,2                GENERATE '* * * ETC'                         
         LA    RF,L'SRVL1-1(R6)    A(LAST CHR IN LINE)                          
         OI    DLHDR2+1,X'08'      SWITCH ON HIGH INTENSITY                     
         MVI   DLLSTAR,C'*'                                                     
         MVI   DLRSTAR,C'*'                                                     
         MVI   DLDATA,C'*'                                                      
         BXLE  R6,RE,*-4                                                        
         DROP  R6                                                               
*                                                                               
PFKDISX  B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
* PASS CONTROL TO $HELP FOR FULL PANEL HELP                           *         
***********************************************************************         
         SPACE 1                                                                
         USING GMTXTD,R3                                                        
HV1HELP  CLI   GMTXTELL,21         TXTEL MUST BE 21 CHRS MIN                    
         BL    PFKNOHLP                                                         
         CLI   GMTXTELL,25         TXTEL MUST BE 25 CHRS MAX                    
         BH    PFKNOHLP                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,GMTXTELL            GET ELEMNT LEN IN RF                      
         SH    RF,=Y(3+6+1)           REDUCE TO EXECUTE LEN                     
         CLI   GMTXTLIN+5,C','        CHECK FORMAT GOOD                         
         BNE   PFKNOHLP                                                         
         XC    WORK,WORK              CLEAR WORK AREA                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+3(0),GMTXTLIN+6   INSERT HELP KEY TEXT                      
         LA    RF,4(RF)                                                         
         STC   RF,WORK                                                          
         MVC   WORK+1(2),SRVP1H+2     INSERT SCREEN ADDRS                       
         L     R1,TBUFF                                                         
         MVI   0(R1),9                LEN FOR "LAA=HELP "                       
         MVC   1(2,R1),SRVSRVH+2      INSERT SCREEN ADDR                        
         MVC   3(6,R1),DC@HELP        INSERT "=HELP "                           
         MVC   9(L'WORK,R1),WORK                                                
*                                                                               
         MVC   SRVSRV(8),=C'=GOBACK ' PASS CONTROL TO =HELP                     
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY ERROR MESSAGE OR OK MESSAGE                                 *         
***********************************************************************         
         SPACE 1                                                                
OKEXIT   ZIC   R1,OKMSG            GET OK EXIT NUMBER                           
         BCTR  R1,0                                                             
         LA    R0,MSGALSTL         L'ADDR LIST FOR EACH MESSAGE                 
         MR    R0,R0               OFFSET TO RELEVANT ADDR LIST                 
         ZIC   RE,LANG                                                          
         SLL   RE,2                OFFSET FOR LANG WITHIN ENTRY                 
         AR    R1,RE               OFFSET FOR LANG WITHIN TABLE                 
         LA    RF,MSGALST                                                       
         L     R1,0(R1,RF)         R1=A(RELEVANT LANGUAGE TEXT)                 
         LTR   R1,R1                                                            
         BZ    EXIT                                                             
         A     R1,RELO                                                          
         MVC   SRVMSG(L'OKMSGTXT),0(R1)                                         
         SPACE 2                                                                
EXIT     LA    R1,SRVSRVH                                                       
         NI    SRVSRVH+6,X'BF'     SWITCH OFF POSN CURSOR                       
         OI    6(R1),X'40'         RE-POSN AS DEFINED BY EXIT POINT             
         OI    6(R1),X'81'         FORCE XMT+MODIFIED FOR NEXT I/P              
         XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLES AND CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
SYSTAB   DS    0CL16               SYSTEM IDENTIFIERS USED BY GETTXT            
*&&UK*&& DC    C'GD23MLAF8BCZ??I?'                                              
*&&US*&& DC    C'GDSNPLANRBCZWENSP????LLSNP??????'                              
DC@HELP  DC    C'=HELP '                                                        
         SPACE 2                                                                
MSGTAB   DS    0CL1                                                             
         DC    AL1(GMKTERR)                                                     
         DC    AL1(GMKTINF)                                                     
         DC    AL1(GMKTSCR)                                                     
         DC    AL1(GMKTTXT)                                                     
         DC    AL1(GMKTWRN)                                                     
         DC    AL1(0)                                                           
         SPACE 2                                                                
LINELEN  DC    Y(SRVL2-SRVL1)                  L'LINE                           
MAXLIN   EQU   (SRVLX-SRVL1)/(SRVL2-SRVL1)+1   NO LINES                         
LTWA     DC    AL4(6*1024)                     SIZE OF TWA                      
         SPACE 2                                                                
         DS    0F                  WORD ALIGN                                   
MSGALST  DS    0CL(4*10)           10 ADDRESSES                                 
MSGALSTL EQU   L'MSGALST                                                        
OKMSG1A  DC    A(OKENG01,OKENG01,OKENG01,OKGER01,OKFRE01,OKENG01)               
         DC    A(OKENG01,OKENG01,OKENG01,OKENG01)                               
OKMSG2A  DC    A(OKENG02,OKENG02,OKENG02,OKGER02,OKFRE02,OKENG02)               
         DC    A(OKENG02,OKENG02,OKENG02,OKENG02)                               
*                                                                               
NOHELP   DC    CL40'No extra information available'          ENG                
         DC    CL40'No extra information available'          EUK                
         DC    CL40'No extra information available'          EUS                
         DC    CL40'Hilfsunterstuetzung hier nicht moeglich' GER                
         DC    CL40'Plus d''information disponible'          FRE                
         DC    CL40'No extra information available'          SPA                
         DC    CL40'No extra information available'          ITA                
         DC    CL40'No extra information available'          N/D                
         DC    CL40'No extra information available'          N/D                
         DC    CL40'No extra information available'          N/D                
         SPACE 1                                                                
         DS    0F                                                               
PFKTAB   DS    0CL24                                                            
*&&UK*&& DC    C'01',C'PF1 ',X'8000',C'...         ',A(PF01A)                   
*&&US*&& DC    C'01',C'PF1 ',X'0000',C'=PFK        ',A(PF01A)                   
         DC    C'02',C'PF2 ',X'0000',C'=SWS        ',A(PF02A)                   
         DC    C'03',C'PF3 ',X'8000',C'=NEW        ',A(PF03A)                   
         DC    C'04',C'PF4 ',X'0000',C'=DQU        ',A(PF04A)                   
         DC    C'05',C'PF5 ',X'0000',C'=PQ         ',A(PF05A)                   
         DC    C'06',C'PF6 ',X'0000',C'=JOB        ',A(PF06A)                   
         DC    C'07',C'PF7 ',X'8000',C'...         ',A(PF07A)                   
         DC    C'08',C'PF8 ',X'0000',C'=COPY       ',A(PF08A)                   
         DC    C'09',C'PF9 ',X'0000',C'=SVS        ',A(PF09A)                   
         DC    C'10',C'PF10',X'0000',C'=PQ         ',A(PF10A)                   
         DC    C'11',C'PF11',X'0000',C'=PQ         ',A(PF11A)                   
         DC    C'12',C'PF12',X'0000',C'=RE         ',A(PF12A)                   
         DC    X'FFFFFFFF'                                                      
         SPACE 1                                                                
PF01A    DS    0A                                                               
*&&UK*&& DC    A(ENGND,ENGND,ENGND,GERND,FREND,ENGND,ENGND,ENGND,ENGND)         
*&&US*&& DC    A(ENG01,ENG01,ENG01,GER01,FRE01,ENG01,ENG01,ENG01,ENG01)         
PF02A    DC    A(ENG02,ENG02,ENG02,GER02,FRE02,ENG02,ENG02,ENG02,ENG02)         
PF03A    DC    A(ENGND,ENGND,ENGND,GERND,FREND,ENGND,ENGND,ENGND,ENGND)         
PF04A    DC    A(ENG04,ENG04,ENG04,GER04,FRE04,ENG04,ENG04,ENG04,ENG04)         
PF05A    DC    A(ENG05,ENG05,ENG05,GER05,FRE05,ENG05,ENG05,ENG05,ENG05)         
PF06A    DC    A(ENG06,ENG06,ENG06,GER06,FRE06,ENG06,ENG06,ENG06,ENG06)         
PF07A    DC    A(ENGND,ENGND,ENGND,GERND,FREND,ENGND,ENGND,ENGND,ENGND)         
PF08A    DC    A(ENG08,ENG08,ENG08,GER08,FRE08,ENG08,ENG08,ENG08,ENG08)         
PF09A    DC    A(ENG09,ENG09,ENG09,GER09,FRE09,ENG09,ENG09,ENG09,ENG09)         
PF10A    DC    A(ENG10,ENG10,ENG10,GER10,FRE10,ENG10,ENG10,ENG10,ENG10)         
PF11A    DC    A(ENG11,ENG11,ENG11,GER11,FRE11,ENG11,ENG11,ENG11,ENG11)         
PF12A    DC    A(ENG12,ENG12,ENG12,GER12,FRE12,ENG12,ENG12,ENG12,ENG12)         
         EJECT                                                                  
ENG01    DC    CL50'Display list of PF key definitions               '          
ENG02    DC    CL50'Swap to last saved screen                        '          
ENG03    DC    CL50'Display latest new information about program     '          
ENG04    DC    CL50'Print queue display report on screen             '          
ENG05    DC    CL50'Print queue reports function                     '          
ENG06    DC    CL50'Display soon jobs status                         '          
ENG07    DC    CL50'                                                 '          
ENG08    DC    CL50'Copy screen to print queue                       '          
ENG09    DC    CL50'Save the current screen                          '          
ENG10    DC    CL50'Print last report on default printer             '          
ENG11    DC    CL50'Show status of default printer                   '          
ENG12    DC    CL50'Recall last screen                               '          
         SPACE 2                                                                
GER01    DC    CL50'Funktionen der PF-Tasten anzeigen                '          
GER02    DC    CL50'Wechseln auf letztes gesichertes Bild            '          
GER03    DC    CL50'Display latest new information about program     '          
GER04    DC    CL50'Druckbereich -BERICHTE- anzeigen                 '          
GER05    DC    CL50'Funktionen fuer Druckbereich -BERICHTE- anzeigen '          
GER06    DC    CL50'Status der -EILIGEN JOBS- anzeigen               '          
GER07    DC    CL50'                                                 '          
GER08    DC    CL50'Bildschirminhalt ausdrucken                      '          
GER09    DC    CL50'Bildschirminhalt sichern                         '          
GER10    DC    CL50'Print last report on default printer             '          
GER11    DC    CL50'Show status of default printer                   '          
GER12    DC    CL50'Letztes Bild anzeigen                            '          
         SPACE 2                                                                
FRE01    DC    CL50'Display list of PF key definitions               '          
FRE02    DC    CL50'Swapper a la screen dernier                      '          
FRE03    DC    CL50'Display latest new information about program     '          
FRE04    DC    CL50'Regarder la report sur la screen                 '          
FRE05    DC    CL50'La queue du print regarder                       '          
FRE06    DC    CL50'Regarder les status des jobs                     '          
FRE07    DC    CL50'                                                 '          
FRE08    DC    CL50'Copy screen to print queue                       '          
FRE09    DC    CL50'Saver la screen                                  '          
FRE10    DC    CL50'Print last report on default printer             '          
FRE11    DC    CL50'Show status of default printer                   '          
FRE12    DC    CL50'Retourner a la screen dernier                    '          
         SPACE 2                                                                
ENGND    DC    CL50'Not defined                                      '          
GERND    DC    CL50'Nicht definiert                                  '          
FREND    DC    CL50'Non disponible                                   '          
         SPACE 2                                                                
OKMSGTXT DS    0CL50                                                            
OKENG01  DC    CL50'PFK values displayed                             '          
OKENG02  DC    CL50'PFK values displayed. Enter to recall screen     '          
OKGER01  DC    CL50'PF-Funktionen angezeigt                          '          
OKGER02  DC    CL50'PF-Funktionen angezeigt. -EINGABE- = weiter      '          
OKFRE01  DC    CL50'PFK values displayed                             '          
OKFRE02  DC    CL50'PFK values displayed. Enter to recall screen     '          
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
SRWORKD  DSECT                                                                  
DUB      DS    D                                                                
RELO     DS    F                                                                
SAVER1   DS    F                                                                
APFKTAB  DS    A                                                                
ATIA     DS    A                                                                
AGETTXT  DS    A                                                                
ATMPSTR  DS    A                                                                
AMSGHDR  DS    A                                                                
ALSTHLP  DS    A                                                                
CURPOSN  DS    A                                                                
DMCB     DS    6F                                                               
LANG     DS    X                                                                
SYSID    DS    X                                                                
DELIM    DS    X                                                                
OKMSG    DS    X                                                                
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
MSGNO    DS    XL2                 MESSAGE NUMBER                               
MSGCODE  DS    XL1                 MESSAGE CODE (E/W/I/S/T)                     
SYSCODE  DS    XL1                 SYSTEM NUMBER                                
HIGHNO   DS    XL1                                                              
HLPRELR  DS    XL2                                                              
STDMSG   DS    X                                                                
SAVSRV   DS    CL17                                                             
WORK     DS    XL20                                                             
IOA      DS    2000C                                                            
SRWORKX  DS    0C                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL DSECTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
PFKTABD  DSECT                                                                  
PFKID    DS    CL2                 IDENTIFICATION                               
PFKNUM   DS    CL4                 NUMBER                                       
PFKFLG1  DS    X                   FLAG BYTE ONE                                
PFKND    EQU   X'80'               PF KEY NOT DEFINED                           
PFKFLG2  DS    X                   FLAG BYTE TWO                                
PFKSRV   DS    CL12                DATA THAT GOES IN S/R FIELD                  
PFKTXTA  DS    AL4                 ADDRESS OF EXPANSION TEXT                    
         SPACE 1                                                                
DISPLD   DSECT                                                                  
DLHDR1   DS    XL8                                                              
DLCOL1   EQU   1                   RELATIVE COLUMN POSN                         
DLLSTAR  DS    XL1                 LEFT STAR                                    
DLHDR2   DS    XL8                                                              
DLCOL2   EQU   3                   RELATIVE COLUMN POSN                         
DLDATA   DS    0CL75                                                            
         DS    CL4                                                              
DLPFKNUM DS    CL4                                                              
         DS    CL1                                                              
DLPFKSRV DS    CL12                                                             
         DS    CL1                                                              
DLPFKTXT DS    CL50                                                             
         ORG   DLDATA                                                           
         DS    CL2                                                              
DLEXPTXT DS    CL70                                                             
         ORG   DLDATA+L'DLDATA                                                  
DLHDR3   DS    XL8                                                              
DLCOL3   EQU   79                  RELATIVE COLUMN POSN                         
DLRSTAR  DS    XL1                 RIGHT STAR                                   
DISPLLEN EQU   *-DISPLD                                                         
         EJECT                                                                  
***********************************************************************         
* TWA AND SYSTEM DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
SRPFKFFD DSECT                                                                  
         DS    CL64                                                             
* SRPFKFFD                                                                      
       ++INCLUDE SRPFKFFD                                                       
         EJECT                                                                  
* FADSECTS                                                                      
         PRINT  OFF                                                             
       ++INCLUDE FADSECTS                                                       
         PRINT  ON                                                              
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT  OFF                                                             
       ++INCLUDE DDCOMFACS                                                      
         PRINT  ON                                                              
         SPACE 1                                                                
* FAGETTXTD                                                                     
         PRINT  OFF                                                             
       ++INCLUDE FAGETTXTD                                                      
         PRINT  ON                                                              
         SPACE 1                                                                
* GEGENMSG                                                                      
         PRINT  OFF                                                             
       ++INCLUDE GEGENMSG                                                       
         PRINT  ON                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SRPFK00   08/22/00'                                      
         END                                                                    
