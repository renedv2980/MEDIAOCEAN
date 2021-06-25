*          DATA SET PPLFM00    AT LEVEL 058 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044157.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T40400A                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T40400   PRINT LOGICAL FILE MAINT.  CLT/PRD/EST'                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMYE 6/16/10  DELETE REFERENCE TO POWER CODES NO LONGER ON DDS SYSTEM         
*                                                                               
* SMYE 03/28/03 INCLUDE LINKS TO PPBROWSE FOR ESTIMATE CODE                     
*                                                                               
* KWAN 03/25/03 USE COMFACS' GETPROF INSTEAD OF INCLUDING IT                    
*                                                                               
* KWAN 07/08/02 ALLOW DEL AND RESTORE ACTION FOR GFEST RECORDS                  
*                                                                               
* SMYE 06/11/02 ALLOW ONLY A THRU Z AND 0 THRU 9 AS 1ST CHARACTERS              
*               OF CLIENT CODE                                                  
*                                                                               
* SMYE 10/25/01 INCLUDE LINKS TO PPBROWSE FOR CLIENT AND PRODUCT CODES          
*                                                                               
* KWAN 03/22/01 NO-OP CODES FROM 12/00 (PREVIOUS ENTRY)                         
*                                                                               
* KWAN 12/00    BY PASS CLIENT OFFICE SECURITY FOR PRD SECURITY                 
*                                                                               
* SMYE 10/24/00 NO-OPPED (F *NOP*) WESTERN AGENCY INDICATOR FOR                 
*               AGENCY CODE 'SJ' IN SVAGYSW SWITCH                              
*                                                                               
* KWAN 06/00    NO OP INSTRUCTION "OI    GLVXFLG1,GLV1GOTO"                     
*                                                                               
* SMYE 05/15/00 ALLOW ONLY * AND CHARACTERS A THROUGH 9 AS                      
*               FIRST CHARACTER OF PRODUCT CODE                                 
*                                                                               
* SMYE 04/00    SAVE PAGYPINI (BINARY RFP ID NUMBER) IN SVAGPINI                
*               IN VALIDATE MEDIA FOR USE IN PPLFM01                            
*                                                                               
* BPLA 01/99    SET WR AS A WESTERN AGENCY (WRLA)                               
*                                                                               
* SMYE 09/98    NEW CREDIT LIMIT BIT (X'04') AT BYTE TWA+12 BEING               
*               USED IN PPLFM01                                                 
*                                                                               
* BPLA 08/98    MORE SPECIAL CHARACTERS INVALIDATED AS                          
*               FIRST CHARACTER OF CLIENT CODE (&,?,,)                          
*                                                                               
* BPLA 03/98    DISPLAY OF PFKEYS RE-ACTIVATED                                  
*                                                                               
* SMYE 03/98    RE-ACTIVATED WESTERN AGENCY INDICATOR FOR ALL WESTERN           
*               AGENCY CODES AND 'SJ'                                           
*                                                                               
* BPLA 02/98    SEE IF STEREO - DON'T DISPLAY PFKEYS                            
*                                                                               
* BPLA 01/98    NO-OP DISPLAY OF PFKEYS (FOR NOW)                               
*               NOTE- PFKEY SWITCHING BETWEEN                                   
*               FILE AND INFO WILL STILL WORK                                   
*                                                                               
* SMYE 12/97    NO-OPPED (*NOP*) WESTERN AGENCY INDICATOR FOR ALL               
*               AGENCY CODES EXCEPT 'WJ' AND 'SJ'                               
*                                                                               
* SMYE 10/97    SET INDICATOR IF WESTERN AGENCY AND SAVE IN SVAGYSW             
*               ALSO SAVE PCLTSTAT IN SVCLSTAT AFTER CLIENT READ                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T40400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 700,T40400,R9,RR=R3                                              
         USING GENOLD,RC           GENOLD AREA IS 328 BYTES                     
*                                                                               
         ST    R3,RELO             NOTE: R9 IS SECOND BASE REGISTER             
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         BAS   RE,INITL                                                         
         USING T404FFD,RA                                                       
*                                                                               
         OI    HDRSRVH+6,X'81'     FORCE SRVREQ TO XMT/MOD                      
*                                                                               
         L     RF,0(R1)                                                         
         ST    RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID                                                       
         CHI   R0,12                                                            
         BNH   *+8                                                              
         AHI   R0,-12              ADJUST                                       
         STC   R0,PFKEY                                                         
         DROP  RF                                                               
*                                                                               
         MVC   VTWA,16(R1)         COMFACS                                      
         L     RF,VTWA             REALLY ADDRESS OF COMFACS                    
         ST    RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   VGLOBBER,CGLOBBER                                                
         L     RF,CGETFACT-COMFACSD(RF)                                         
         MVI   SVSTEREO,C'N'                                                    
         GOTO1 (RF),DMCB,0                                                      
         L     RE,0(R1)                                                         
         USING FACTSD,RE                                                        
         TM    FATSTAT6,X'80'      TEST FOR STEREO                              
         BZ    *+8                                                              
         MVI   SVSTEREO,C'Y'                                                    
         DROP  RE,RF                                                            
*                                                                               
         XC    HDRMSG,HDRMSG                                                    
         FOUT  HDRMSGH                                                          
*                                                                               
         CLI   SVBROWSE,C'B'       RETURNING FROM BROWSE ?                      
         BE    PFL1B                                                            
*                                                                               
* FIRST CHECK FOR TRANSFER CONTROL DATA OR PFKEY                                
*                                                                               
         BAS   RE,TESTXFR                                                       
*                                                                               
PFL1A    CLI   PFKEY,12            RETURN?                                      
         BNE   PFL1B                                                            
         CLI   SVXFRSY,0           SOMEPLACE TO RETURN?                         
         BE    PFL1B               NO - IGNORE                                  
         MVI   BYTE,C'R'           RETURN CALL                                  
         BAS   RE,CALLXFR                                                       
         B     EXIT                                                             
*                                                                               
* SAVE THE FIRST 3 BYTES OF OVERLAY SCREEN AND SET                              
* HDRLAST TO CLEAR AFTER SO LOWER SCREEN WILL BE                                
* CLEARED IF AN ERROR OCCURS IN HEADER FIELDS                                   
*                                                                               
PFL1B    MVI   SVBROWSE,C' '       CLEAR BROWSE INDICATOR                       
*                                                                               
         TM    HDRRECH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         MVC   LACT1,BACT          SAVE BACT IN LACT1                           
         TM    HDRACTH+4,X'20'                                                  
         BO    CKVALC                                                           
*                                                                               
* IF ACTION=CHA & LAST ACTION=DISP & OTHER FLDS VALIDATED THEN DO EDIT          
*                                                                               
         CLC   HDRACT(3),=C'CHA'                                                
         BNE   NOTVAL                                                           
*                                                                               
         MVI   BYTE,3              CHANGE                                       
         CLI   HDRACT+3,C'B'                                                    
         BNE   *+8                                                              
         MVI   BYTE,5              CHANGE BILL                                  
         CLC   HDRACT+3(3),=C'ADV'                                              
         BNE   *+8                                                              
         MVI   BYTE,9              CHANGE ADV                                   
         CLC   BYTE,BACT                                                        
         BNE   NOTVAL                                                           
         TM    HDRMEDH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         TM    HDRCLTH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         CLI   BREC,X'02'          CLIENT                                       
         BE    CHAVAL                                                           
         TM    HDRPRDH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         CLI   BREC,X'06'                                                       
         BE    CHAVAL                                                           
         TM    HDRESTH+4,X'20'                                                  
         BNO   NOTVAL                                                           
*                                                                               
CHAVAL   OI    HDRACTH+4,X'20'     VALIDATE                                     
         MVI   BACT,X'02'                                                       
         CLI   HDRACT+3,C'B'                                                    
         BNE   *+8                                                              
         MVI   BACT,4              CHANGE BILL                                  
         CLC   HDRACT+3(3),=C'ADV'                                              
         BNE   *+8                                                              
         MVI   BACT,8              CHANGE ADV                                   
         B     CKVALC                                                           
*                                                                               
NOTVAL   MVI   LACT1,0             VALIDATE RECORD                              
         LA    R2,HDRRECH                                                       
         LA    R3,RECERR                                                        
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         LA    R5,LFRECS                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,RELO                                                          
         LA    R5,6(R5)                                                         
         IC    R4,5(R2)                                                         
         BCTR  R4,R0                                                            
*                                                                               
CKREC    EX    R4,COMP                                                          
         BE    CKRECA                                                           
         BXLE  R5,R6,CKREC                                                      
         LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
CKRECA   MVC   BREC(1),8(R5)                                                    
         OI    HDRRECH+4,X'20'                                                  
         FOUT  HDRRECH                                                          
*                                                                               
         LA    R2,HDRACTH          VALIDATE ACTION                              
         LA    R3,ACTERR                                                        
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         LA    R5,LFACTS                                                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,RELO                                                          
         LA    R5,6(R5)                                                         
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
*                                                                               
CKACT    EX    R4,COMP                                                          
         BE    CKACTA                                                           
         BXLE  R5,R6,CKACT                                                      
         LA    R3,2                                                             
         B     ERROR                                                            
CKACTA   MVC   BACT(1),8(R5)                                                    
         OI    HDRACTH+4,X'20'                                                  
         FOUT  HDRACTH                                                          
*                                                                               
* UNVALIDATE THIS REC PART OF KEY                                               
*                                                                               
         NI    HDRCLTH+4,X'DF'                                                  
         NI    HDRPRDH+4,X'DF'                                                  
         NI    HDRESTH+4,X'DF'                                                  
         B     CKVALC                                                           
*                                                                               
COMP     CLC   8(0,R2),0(R5)                                                    
*                                                                               
* VALIDATE  RECORD - ACTION  COMBINATION                                        
*                                                                               
CKVALC   NI    SVAGYSW,X'FF'-X'01' TURN WESTERN AGENCY SWITCH OFF               
         CLC   AGYALPHA(2),=C'WI'  "WESTERN" AGENCY?                            
         BE    CKVALC1             YES                                          
         CLC   AGYALPHA(2),=C'WJ'  "WESTERN" TEST AGENCY?                       
         BE    CKVALC1             YES                                          
         CLC   AGYALPHA(2),=C'WT'  "WESTERN" AGENCY?                            
         BE    CKVALC1             YES                                          
         CLC   AGYALPHA(2),=C'WR'  "WESTERN" AGENCY?                            
         BNE   CKVALC1X            NO                                           
*                                                                               
CKVALC1  OI    SVAGYSW,X'01'       TURN WESTERN AGENCY SWITCH ON                
*                                                                               
CKVALC1X CLI   T404FFD+1,C'*'      ONLY DDS TERM IS ALLOWED DISADV              
         BE    CKVALC5             OR CHAADV                                    
         CLI   BACT,X'08'                                                       
         BE    CKVALC3                                                          
         CLI   BACT,X'09'                                                       
         BNE   CKVALC5                                                          
*                                                                               
CKVALC3  LA    R3,ACTERR                                                        
         LA    R2,HDRACTH                                                       
         B     ERROR                                                            
*                                                                               
CKVALC5  LA    R5,COMBOS                                                        
         LA    R6,4                                                             
         LA    R7,COMBOSX-1                                                     
*                                                                               
CKCOMB   CLC   BREC(2),0(R5)                                                    
         BE    CKACC                                                            
         BXLE  R5,R6,CKCOMB                                                     
         NI    HDRRECH+4,X'DF'                                                  
         NI    HDRACTH+4,X'DF'                                                  
         LA    R3,COMBERR                                                       
         B     ERROR                                                            
*                                                                               
CKACC    DS    0H                  CHK FOR LIMITED ACCESS                       
         CLI   T404FFD+1,C'*'      DDS TERM                                     
         BE    CKMED                                                            
         MVC   WORK(1),3(R5)                                                    
         NC    WORK(1),T404FFD+12                                               
         BZ    CKMED                                                            
         CLC   =C'DIS',HDRACT      DISPLAY IS OK                                
         BE    CKMED                                                            
         LA    R3,ACCSERR                                                       
         LA    R2,HDRACTH          CURSOR TO ACTION                             
         B     ERROR                                                            
*                                                                               
* REC/ACT OK  NOW  EDIT  KEY                                                    
*                                                                               
CKMED    MVC   OLNUM(1),2(R5)      VALIDATE MEDIA                               
         LA    R2,HDRMEDH                                                       
         LA    R3,MEDERR                                                        
         XC    KEY,KEY                                                          
         MVC   KAGY,AGYALPHA                                                    
         MVC   KMED,HDRMED                                                      
         TM    4(R2),X'20'                                                      
         BO    CKCLIENT                                                         
         MVI   KRCD,X'01'                                                       
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
*                                                                               
* SAVE BYTE 12 OF AGENCY PROF                                                   
*                                                                               
         MVC   SVAGPF12(1),PAGYPROF+11                                          
*                                                                               
* SAVE 2-BYTE BINARY RFP ID NUMBER                                              
*                                                                               
         MVC   SVAGPINI,PAGYPINI                                                
*                                                                               
         MVC   WNATION,PAGYNAT     NATIONALITY                                  
*                                                                               
         XC    SVACCAGY,SVACCAGY                                                
         LA    R5,PAGYREC+33                                                    
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   CKMED5                                                           
         ZIC   R1,1(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,MVACCAGY                                                      
         B     CKMED5                                                           
*                                                                               
MVACCAGY MVC   SVACCAGY(0),2(R5)   EXECUTED                                     
*                                                                               
CKMED5   XC    SVCTAGY,SVCTAGY                                                  
         LA    R5,PAGYREC+33                                                    
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   CKMED6                                                           
         MVC   SVCTAGY,2(R5)                                                    
*                                                                               
CKMED6   FOUT  HDRMEDNH,PAGYMED,10                                              
         FOUT  HDRCLTNH,SPACES,20                                               
         FOUT  HDRPRDNH,SPACES,50                                               
         FOUT  HDRESTNH,SPACES,20                                               
         NI    HDRCLTH+4,X'DF'                                                  
         NI    HDRPRDH+4,X'DF'                                                  
         NI    HDRESTH+4,X'DF'                                                  
         OI    4(R2),X'20'                                                      
         MVI   DSPSW,1             SET ACTION=FORMAT                            
         B     CKCLIENT                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE  CLIENT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKCLIENT DS    0H                                                               
         CLI   PFKEY,2             TEST PF2 ENTERED - CALL TO INF               
         BNE   CKCLT03                                                          
         MVI   BYTE,C'I'                                                        
         BAS   RE,CALLXFR                                                       
         B     EXIT                                                             
*                                                                               
CKCLT03  LA    R2,HDRCLTH                                                       
         LA    R3,CLTERR                                                        
         OI    HDRCLT+2,X'40'                                                   
*                                                                               
         CLI   HDRCLT,C'='         SEARCH CLIENT CODES ?                        
         BNE   CKCLT04             NO                                           
*                                                                               
         MVI   SVBROWSE,C'B'       SET BROWSE INDICATOR                         
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,VTWA,(RD),(R2),                       +        
               0,(KMED,C' CLT'),0,RR=RELO                                       
*                                                                               
         DC    H'0'                BROWSE SHOULD HAVE TAKEN IT                  
*                                                                               
CKCLT04  MVC   KCLT,HDRCLT                                                      
         MVI   KRCD,X'02'                                                       
         TM    4(R2),X'20'                                                      
         BO    CKCLT05                                                          
*                                                                               
* EVEN IF PREV VALID READ RECORD AND GET FO PROFILE                             
*                                                                               
         FOUT  HDRCLTNH,SPACES,20                                               
         FOUT  HDRPRDNH,SPACES,50                                               
         FOUT  HDRESTNH,SPACES,20                                               
         NI    HDRPRDH+4,X'DF'                                                  
         NI    HDRESTH+4,X'DF'                                                  
         BAS   RE,ANY                                                           
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         CLC   8(3,R2),=C'ALL'     ALL IS INVALID                               
         BE    ERROR                                                            
*NOP*    CLI   8(R2),C'*'          "*" IS INVALID 1ST CHARACTER                 
*NOP*    BE    ERROR                                                            
*NOP*    CLI   8(R2),C'$'          "$" IS INVALID 1ST CHARACTER                 
*NOP*    BE    ERROR                                                            
*NOP*    CLI   8(R2),C'&&'         "&" IS INVALID 1ST CHARACTER                 
*NOP*    BE    ERROR                                                            
*NOP*    CLI   8(R2),C'?'          "?" IS INVALID 1ST CHARACTER                 
*NOP*    BE    ERROR                                                            
*NOP*    CLI   8(R2),C','          "," IS INVALID 1ST CHARACTER                 
*NOP*    BE    ERROR                                                            
*                                                                               
*   ONLY A THRU Z AND 0 THRU 9 ARE VALID 1ST CHARACTERS OF CLIENT CODE          
*                                                                               
         CLI   8(R2),C'A'                                                       
         BL    ERROR                INVALID 1ST CHARACTER                       
         CLI   8(R2),C'I'                                                       
         BNH   CKCLT05              OK                                          
         CLI   8(R2),C'J'                                                       
         BL    ERROR                INVALID 1ST CHARACTER                       
         CLI   8(R2),C'R'                                                       
         BNH   CKCLT05              OK                                          
         CLI   8(R2),C'S'                                                       
         BL    ERROR                INVALID 1ST CHARACTER                       
         CLI   8(R2),C'Z'                                                       
         BNH   CKCLT05              OK                                          
         CLI   8(R2),C'0'                                                       
         BL    ERROR                INVALID 1ST CHARACTER                       
         CLI   8(R2),C'9'                                                       
         BH    ERROR                INVALID 1ST CHARACTER                       
*                                                                               
CKCLT05  CLC   BREC(2),=X'0201'                                                 
         BE    CHKADD                                                           
         LA    R3,40               CLIENT NOT FOUND                             
         BAS   RE,READ                                                          
         MVC   CLTADDR,KEY+27                                                   
         BAS   RE,GETREC                                                        
*                                                                               
******** CLI   T404FFD+1,C'*'      DDS TERM?                                    
******** BE    CKCLT08             YES, NO NEED TO CHECK SECURITY               
*                                                                               
         LA    R0,WRKREC           INIT SECBLK (TEMP USE)                       
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    CKCLT05H                                                         
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         LA    R3,WRKREC                                                        
         GOTO1 (RF),DMCB,('SECPINIT',(R3)),0                                    
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
CKCLT05H MVC   BYTE2,PCLTOFF       SAVE ORIGINAL CLT OFF CODE                   
         LA    R3,CACCERR                                                       
         OC    6(2,RA),6(RA)                                                    
         BZ    CKCLT05X            NO RESTRICTIONS                              
*                                                                               
         BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON?                          
         BNE   CKCLT05M            NO                                           
         BRAS  RE,TRAFFACC         LOOK FOR CLIENT TRAFFIC OFFICE CODE          
         CLI   BYTE3,0             ANYTHING FOUND?                              
         BE    CKCLT05M            NO                                           
         MVC   PCLTOFF,BYTE3       USE CLIENT TRAFFIC OFFICE CODE               
*                                                                               
CKCLT05M XC    WORK,WORK           WORK MUST BE AT LEAST 48 BYTES               
         LA    R1,WORK             (LENGTH OF OFFICED IS 48 BYTES)              
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF                                                   
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         LA    RE,WRKREC                                                        
         STCM  RE,15,OFCSECD       ADDRESS OF SECRET BLOCK                      
         DROP  R1                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),255           ADDRESS OF OFFICER FOUND?                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS                                   
         CLI   0(R1),0                                                          
         BNE   ERROR                                                            
*                                                                               
CKCLT05X MVC   PCLTOFF,BYTE2       RESTORE CLT OFF CODE                         
*                                                                               
* AT THIS POINT, CLIENT SECURITY IS CHECKED                                     
*                                                                               
CKCLT08  CLC   CPROFLE(1),PCLTPROF                                              
         BE    CKCLT08M                                                         
         CLI   SCRNUM,X'F2'                                                     
         BNE   *+8                 DIFFERENT SCREEN FOR PRDS WITH DIVS          
         MVI   SCRNUM,0                                                         
*                                                                               
CKCLT08M MVC   SVCLSTAT,PCLTSTAT   SAVE IN SVCLSTAT FOR LATER MODULES           
         MVC   CPROFLE(20),PCLTPROF                                             
*                                                                               
* READ FOR F0 PROFILE AND SAVE IN F0PROF                                        
*                                                                               
         MVC   WORK(12),SPACES                                                  
         MVC   WORK(4),=C'P0F0'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'C0',WORK),F0PROF,VDATAMGR                           
*                                                                               
         FOUT  HDRCLTNH,PCLTNAME,20                                             
         MVC   FINANSW,PCLTFIN                                                  
         XC    SVP1USER(SVULNQ),SVP1USER                                        
         LA    R4,PCLTELEM                                                      
*                                                                               
CKCLT10  CLI   0(R4),0                                                          
         BE    CKCLT30                                                          
         CLI   0(R4),X'20'         UDEF ELEMENT                                 
         BE    CKCLT20                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     CKCLT10                                                          
*                                                                               
CKCLT20  MVC   SVP1USER(SVULNQ),2(R4)                                           
*                                                                               
CKCLT30  TM    4(R2),X'20'         SEE IF PREVIOUSLY VALIDATED                  
         BO    CKPRD                                                            
*                                                                               
         MVI   DSPSW,1             SET ACTION=FORMAT                            
         OI    4(R2),X'20'                                                      
         B     CKPRD                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE  PRODUCT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPRD    CLI   BREC,X'02'                                                       
         BNE   CKPRD0                                                           
         CLI   BACT,X'01'                                                       
         BE    CHKADD              CK ADD EVEN IF PREVIOUSLY VALIDATED          
         B     GETOVLY                                                          
*                                                                               
CKPRD0   LA    R2,HDRPRDH                                                       
         LA    R3,PRDERR                                                        
         OI    HDRPRD+2,X'40'                                                   
*                                                                               
         CLI   HDRPRD,C'='         SEARCH PRODUCT CODES ?                       
         BNE   CHKPRDM             NO                                           
*                                                                               
         MVI   SVBROWSE,C'B'       SET BROWSE INDICATOR                         
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,VTWA,(RD),(R2),                       +        
               (0,KCLT),(KMED,C' PRD'),0,RR=RELO                                
*                                                                               
         DC    H'0'                BROWSE SHOULD HAVE TAKEN IT                  
*                                                                               
CHKPRDM  MVC   KPRD,HDRPRD                                                      
         MVI   KRCD,X'06'                                                       
         MVC   SAVEPRD,8(R2)       SAVE PRODUCT                                 
         TM    4(R2),X'20'                                                      
         BO    CKEST                                                            
         FOUT  HDRPRDNH,SPACES,50                                               
         FOUT  HDRESTNH,SPACES,20                                               
         NI    HDRESTH+4,X'DF'                                                  
*                                                                               
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         CLC   8(3,R2),=C'ALL'     ALL IS INVALID                               
         BE    ERROR                                                            
*                                                                               
         CLI   BREC,X'08'          P&G RECORD?                                  
         BNE   CKP05               NO                                           
         CLI   5(R2),3             INPUT PRODUCT CODE=3 CHARS?                  
         BNE   CKP05               NO                                           
         CLC   =C'ZZZ',8(R2)       PRODUCT='ZZZ'?                               
         BNE   CKP05               NO                                           
         BAS   RE,HIGH             SEE IF THERE IS A PRD RECD                   
         CLC   KEYSAVE(10),KEY     IF MATCH ON PROD, YES                        
         BE    CKPRD1              YES- PRINT OUT IT'S NAME                     
         B     CKEST               NO NAME EXISTS (THAT'S OKAY TOO)             
*                                                                               
CKP05    ZIC   R5,5(R2)                                                         
         LA    R6,8(R2)                                                         
         CLI   0(R6),C'*'          MAY START WITH *                             
         BE    CKP2                                                             
         CLI   0(R6),C'A'          MUST START WITH A OR GREATER                 
         BL    ERROR                                                            
*                                                                               
CKP1     CLI   0(R6),C' '          EMBEDDED BLANK IS OK                         
         BE    CKP2                                                             
         CLI   0(R6),C'0'                                                       
         BE    CKP2                                                             
         TM    0(R6),X'C0'                                                      
         BNO   ERROR                                                            
         MVC   DUB(1),0(R6)                                                     
         NI    DUB,X'0F'                                                        
         CLI   DUB,X'01'                                                        
         BL    ERROR                                                            
         CLI   DUB,X'09'                                                        
         BH    ERROR                                                            
*                                                                               
CKP2     LA    R6,1(R6)                                                         
         BCT   R5,CKP1                                                          
*                                                                               
CKP3     CLC   BREC(2),=X'0601'                                                 
         BNE   CKPRD1                                                           
         B     CHKADD                                                           
*                                                                               
CKPRD1   BAS   RE,READ                                                          
         MVC   PRDADDR,KEY+27                                                   
         BAS   RE,GETREC           PRODUCT CODE HAS BEEN VALIDATED              
*                                                                               
         TM    SVCLSTAT,X'20'      DOES CLT INDICATE PRD NEED SECURITY?         
         BZ    CKPRD2              NO, DONE WITH PRD SECURITY CK                
         CLI   T404FFD+1,C'*'      DDS TERM                                     
         BE    CKPRD2                                                           
         CLI   T404FFD+6,C'*'      OFFICE LIMIT ACCESS                          
         BNE   CKPRD2                                                           
         CLC   PPRDOFFC(1),T404FFD+7                                            
         BE    CKPRD2                                                           
         LA    R3,PACCERR          PRODUCT LIMIT ACCESS ERROR                   
         B     ERROR                                                            
*                                                                               
CKPRD2   DS    0H                  PRODUCT CODE HAS PASSED SECURITY CK          
         FOUT  HDRPRDNH,PPRDNAME,20                                             
         XC    HDRPRDN+20(L'AORMSG),HDRPRDN+20                                  
*                                                                               
         CLI   PPRDOAN,0           IS THIS AN AOR PRODUCT                       
         BE    NOTHISPR                                                         
         MVC   AORMSG+21(2),PPRDOAN                                             
         MVC   HDRPRDN+20(L'AORMSG),AORMSG                                      
         NI    HDRPRDNH+7,X'80'                                                 
         OI    HDRPRDNH+7,X'50'    NEW LENGTH OF OUTPUT                         
*                                                                               
NOTHISPR DS    0H                                                               
         OI    4(R2),X'20'                                                      
         MVI   DSPSW,1                                                          
         B     CKEST                                                            
*                                                                               
AORMSG   DC    CL30'*OTHER AGY NAME CODE=   ***'                                
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE ESTIMATE                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKEST    CLI   BREC,X'06'                                                       
         BNE   CKEST01                                                          
         CLI   BACT,X'01'                                                       
         BE    CHKADD              CK ADD EVEN IF PREVIOUSLY VALIDATED          
         B     GETOVLY                                                          
*                                                                               
CKEST01  LA    R2,HDRESTH                                                       
         CLI   CPROFLE+5,C'1'      CK MASTER CLT                                
         BNE   CKEST2                                                           
         LA    R3,ESTERR1          NOT EST ALLOWED                              
         B     ERROR                                                            
*                                                                               
CKEST2   LA    R3,ESTERR                                                        
*                                                                               
         CLI   HDREST,C'='         SEARCH ESTIMATE CODES ?                      
         BNE   CKEST2AA            NO                                           
*                                                                               
         MVI   SVBROWSE,C'B'       SET BROWSE INDICATOR                         
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,VTWA,(RD),(R2),                       +        
               (0,KCLT),(KMED,C' EST'),0,RR=RELO     KCLT=CLT(3)PRD(3)          
*                                                                               
         DC    H'0'                BROWSE SHOULD HAVE TAKEN IT                  
*                                                                               
CKEST2AA DS    0H                                                               
         CLI   BREC,X'07'          SEE IF ESTIMATE HEADER                       
         BNE   CKEST2X                                                          
         CLI   BACT,X'03'          AND DISPLAY                                  
         BE    CKEST2A                                                          
         CLI   BACT,X'05'          OR DISB                                      
         BE    CKEST2A                                                          
         CLI   BACT,X'06'          OR COPY                                      
         BNE   CKEST2X                                                          
*                                                                               
CKEST2A  CLC   HDREST(4),=C'HIGH'  SEARCH FOR HIGHEST NUMBER USED               
         BE    CKEST2AX                                                         
         CLC   HDREST(4),=C'LAST'  SPECIAL - TO DISPLAY                         
         BNE   CKEST2X             ESTIMATE WITH HIGHEST START DATE             
*                                                                               
CKEST2AX XC    SVESDT,SVESDT       CLEAR SEARCH EST START DATE                  
         XC    SVLEST,SVLEST       CLEAR ESTIMATE NUMBER                        
         MVI   KRCD,X'07'                                                       
         BAS   RE,HIGH                                                          
         B     CKEST2C                                                          
*                                                                               
CKEST2B  BAS   RE,SEQ                                                           
*                                                                               
CKEST2C  CLC   KEY(10),KEYSAVE     CHECK THROUGH PRD                            
         BNE   CKEST2T                                                          
         CLC   HDREST(4),=C'HIGH'  SEE IF LOOKING FOR HIGHEST                   
         BNE   CKEST2D                                                          
         MVC   SVLEST,KEY+10       SAVE NUMBER AND KEEP SEARCHING               
         B     CKEST2B                                                          
*                                                                               
CKEST2D  BAS   RE,GETREC                                                        
         CLC   SVESDT,PESTST                                                    
         BH    CKEST2B                                                          
         MVC   SVESDT,PESTST                                                    
         MVC   SVLEST,PESTKEST                                                  
         B     CKEST2B                                                          
*                                                                               
CKEST2T  DS    0H                  LAST EST READ                                
         OC    SVLEST,SVLEST       SEE IF I FOUND ANY                           
         BZ    ERROR                                                            
*                                                                               
         XC    HDREST,HDREST                                                    
         EDIT  (B2,SVLEST),(3,8(R2)),0,ALIGN=LEFT                               
         FOUT  (R2)                                                             
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEST,SVLEST                                                      
         BAS   RE,READ                                                          
         MVC   ESTADDR,KEY+27                                                   
         BAS   RE,GETREC                                                        
         FOUT  HDRESTNH,PESTNAME,20                                             
         OI    4(R2),X'20'                                                      
         MVI   DSPSW,1             SET ACTION = FORMAT                          
         B     GETOVLY                                                          
*                                                                               
CKEST2X  CLI   5(R2),3             MAX 3 DIGITS                                 
         BH    ERROR                                                            
         TM    4(R2),X'08'         CHECK FOR NUMERIC                            
         BNO   ERROR                                                            
         BAS   RE,PACK                                                          
*                                                                               
* PGE - ALLOW ESTIMATE=0 IFF PRODUCT<>ZZZ                                       
*                                                                               
         CLI   BREC,X'08'          IF PGEST, ALLOW ESTIMATE=0                   
         BNE   CKEST10             NOT PGEST                                    
         LTR   R0,R0                                                            
         BNZ   CKEST32             NON ZERO, ALLOW ANY ESTIMATE                 
         CLC   SAVEPRD,=C'ZZZ'     FOR ZZZ - CAN'T BE ZERO                      
         BE    ERROR               ERROR                                        
         FOUT  HDRESTNH,SPACES,20  ZERO: ALLOW-- HANDLE DIFFERENTLY             
         B     CKEST33                                                          
*                                                                               
* IF ZZZ THEN EST > 0 ELSE EST=0                                                
*                                                                               
CKEST10  CLI   BREC,X'09'          SEE IF GF EST REC                            
         BNE   CKEST20                                                          
         CLC   SAVEPRD,=C'ZZZ'     FOR ZZZ - CAN'T BE ZERO                      
         BNE   CKEST30                                                          
*                                                                               
CKEST20  LTR   R0,R0                                                            
         BZ    ERROR                                                            
         B     CKEST32                                                          
*                                                                               
* GF/PG-EST RECS: NON-ZZZ MUST BE EST=0                                         
*                                                                               
CKEST30  LTR   R0,R0                                                            
         BNZ   ERROR                                                            
         FOUT  HDRESTNH,SPACES,20                                               
         B     CKEST33                                                          
*                                                                               
CKEST32  STH   R0,KEST                                                          
         MVI   KRCD,X'07'                                                       
         TM    4(R2),X'20'                                                      
         BO    CKEST4                                                           
         FOUT  HDRESTNH,SPACES,20                                               
         TM    4(R2),X'08'                                                      
         BZ    ERROR                                                            
         CLC   BREC(2),=X'0701'                                                 
         BE    CHKADD                                                           
*                                                                               
* SKIP EST READ FOR GFEST/PG-EST REC PRD=ZZZ                                    
*                                                                               
         JIF   BREC,EQ,X'08',AND,SAVEPRD,EQ,=C'ZZZ',CKEST33,JUMP=N              
         JIF   BREC,EQ,X'09',AND,SAVEPRD,EQ,=C'ZZZ',CKEST33,JUMP=N              
*                                                                               
         CLI   BREC,X'09'          GFEST RECORD?                                
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       FOR GFEST, GET DELETED RECORDS TOO           
*                                                                               
         BAS   RE,READ                                                          
         MVC   ESTADDR,KEY+27                                                   
         BAS   RE,GETREC                                                        
         FOUT  HDRESTNH,PESTNAME,20                                             
*                                                                               
CKEST33  OI    4(R2),X'20'                                                      
         CLI   BREC,X'07'                                                       
         BNE   CKPGEST                                                          
         MVI   DSPSW,1             SET ACTION = FORMAT                          
         B     GETOVLY                                                          
*                                                                               
CKEST4   CLI   BREC,X'08'                                                       
         BE    CKPGEST                                                          
         CLI   BREC,X'09'                                                       
         BE    CKGFEST                                                          
         CLI   BACT,X'01'                                                       
         BE    CHKADD              CK ADD EVEN IF PREVIOUSLY VALIDATED          
         B     GETOVLY                                                          
*                                                                               
CKPGEST  CLI   BREC,X'09'                                                       
         BE    CKGFEST                                                          
         CLI   BREC,X'08'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   KRCD,X'0A'                                                       
         CLC   BREC(2),=X'0801'                                                 
         BE    CHKADD                                                           
         BAS   RE,READ                                                          
         MVC   ESTADDR,KEY+27                                                   
*                                                                               
         CLI   BACT,3              IS CURRENT ACTION DISP?                      
         BE    CKPGEST5                                                         
         MVI   DSPSW,0                                                          
         CLI   LACT1,2             WAS LAST CHANGE?                             
         BE    GETOVLY             YES - TO ALLOW CONSECUTIVE CHANGES           
         CLI   LACT1,3             IS LAST ACTION DISP?                         
         BE    GETOVLY                                                          
*                                                                               
CKPGEST5 MVI   DSPSW,1                                                          
         B     GETOVLY                                                          
*                                                                               
CKGFEST  CLI   BREC,X'09'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   KRCD,X'0B'                                                       
         CLC   =X'0901',BREC       ADDING GFEST RECORD?                         
         BE    CHKADD                                                           
         OI    DMINBTS,X'08'       FOR GFEST, GET DELETED RECORDS TOO           
         BAS   RE,READ                                                          
         MVC   ESTADDR,KEY+27                                                   
*                                                                               
         CLI   BACT,3              IS CURRENT ACTION DISP?                      
         BE    CKGF2               YES                                          
         MVI   DSPSW,0                                                          
         CLI   LACT1,2             WAS LAST CHANGE?                             
         BE    GETOVLY             YES - TO ALLOW CONSECUTIVE CHANGES           
         CLI   LACT1,3             IS LAST ACTION DISP?                         
         BE    GETOVLY                                                          
*                                                                               
CKGF2    MVI   DSPSW,1                                                          
         B     GETOVLY                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKADD   LA    R4,=C'DMREAD'                                                    
         LA    R5,=C'PRTDIR'                                                    
         LA    R6,KEY                                                           
         LA    R7,KEYSAVE                                                       
         LA    R1,DMCB                                                          
         STM   R4,R7,0(R1)                                                      
         MVI   0(R1),X'08'         SET TO PASS DELETED RECORDS                  
         MVC   16(1,R1),TERMNAL                                                 
         L     RF,VDATAMGR                                                      
         BASR  RE,RF                                                            
         LA    R3,51               DISK ERROR                                   
         TM    8(R1),X'40'                                                      
         BNZ   ERROR                                                            
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ADDOK               RECORD NOT FOUND - OK                        
         LA    R3,56               DELETED                                      
         TM    KEYSAVE+25,X'80'                                                 
         BNZ   ERROR                                                            
         LA    R3,52               DUP KEY ON ADD                               
         B     ERROR                                                            
*                                                                               
ADDOK    OI    4(R2),X'20'         VALIDATE                                     
         B     GETOVLY                                                          
         EJECT                                                                  
*                                                                               
         CNOP  2,4                                                              
LFRECS   DC    H'9'                                                             
         DC    A(LFRECSX-1)                                                     
         DC    CL8'CLIENT'                                                      
         DC    X'02'                                                            
         DC    CL8'CLTREC'                                                      
         DC    X'02'                                                            
         DC    CL8'PRODUCT'                                                     
         DC    X'06'                                                            
         DC    CL8'PRDREC'                                                      
         DC    X'06'                                                            
         DC    CL8'ESTIMATE'                                                    
         DC    X'07'                                                            
         DC    CL8'PGEST'                                                       
         DC    X'08'                                                            
         DC    CL8'GFEST'                                                       
         DC    X'09'                                                            
LFRECSX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
         CNOP  2,4                                                              
LFACTS   DC    H'9'                                                             
         DC    A(LFACTSX-1)                                                     
         DC    CL8'ADD'                                                         
         DC    X'01'                                                            
         DC    CL8'CHANGE'                                                      
         DC    X'02'                                                            
         DC    CL8'DISPLAY'                                                     
         DC    X'03'                                                            
         DC    CL8'CHABILL'                                                     
         DC    X'04'                                                            
         DC    CL8'DISBILL'                                                     
         DC    X'05'                                                            
         DC    CL8'COPY'                                                        
         DC    X'06'                                                            
         DC    CL8'CHAADV '                                                     
         DC    X'08'                                                            
         DC    CL8'DISADV '                                                     
         DC    X'09'                                                            
         DC    CL8'DELETE '                                                     
         DC    X'0C'                                                            
         DC    CL8'RESTORE'                                                     
         DC    X'0D'                                                            
LFACTSX  EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
COMBOS   DC    X'02010140'         REC/ACT/OVERLAY/RESTRICTED ACCESS            
         DC    X'02020140'                                                      
         DC    X'02030140'                                                      
         DC    X'02080A40'         CHA ADV                                      
         DC    X'02090A40'         DIS ADV                                      
         DC    X'06010220'                                                      
         DC    X'06020220'                                                      
         DC    X'06030220'                                                      
         DC    X'07010310'                                                      
         DC    X'07020310'                                                      
         DC    X'07030310'                                                      
         DC    X'07060510'         ESTIMATE COPY                                
*                                  BILLING FIELDS                               
         DC    X'06040421'                                                      
         DC    X'06050421'                                                      
         DC    X'07040411'                                                      
         DC    X'07050411'                                                      
*                                                                               
         DC    X'08010710'         ADD PGEST RECORD                             
         DC    X'08020710'         CHANGE PGEST RECORD                          
         DC    X'08030710'         DISPLAY PGEST RECORD                         
*                                                                               
         DC    X'09010710'         ADD GFEST RECORD                             
         DC    X'09020710'         CHANGE GFEST RECORD                          
         DC    X'09030710'         DISPLAY GFEST RECORD                         
         DC    X'090C0710'         DELETE GFEST RECORD                          
         DC    X'090D0710'         RESTORE GFEST RECORD                         
*                                                                               
COMBOSX  EQU   *                                                                
*                                                                               
SPACES   DC    CL50' '                                                          
ZEROS    DC    40C'0'                                                           
RECERR   EQU   11                                                               
ACTERR   EQU   12                                                               
MEDERR   EQU   13                                                               
CLTERR   EQU   14                                                               
PRDERR   EQU   15                                                               
ESTERR1  EQU   177                                                              
ESTERR   EQU   16                                                               
COMBERR  EQU   10                                                               
ACCSERR  EQU   96                  NOT AUTH FOR THIS FUNCTION                   
CACCERR  EQU   207                 CLIENT LIMIT ACCESS ERROR                    
PACCERR  EQU   7                   PRODCUT LIMIT ACCESS ERROR                   
         EJECT                                                                  
*                                                                               
* RESTORE 1ST THREE BYTES OF OVERLAY SCR - NO ERROR IN HDR FLDS                 
*                                                                               
GETOVLY  DS    0H                                                               
         CLI   BACT,1              IF ADD                                       
         BNE   *+8                                                              
         MVI   DSPSW,0             UNSET FORMAT SW                              
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),OLNUM                                                    
         ST    RA,DMCB+4                                                        
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
         BASR  RE,RF                                                            
         TM    4(1),X'FF'                                                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RC,DMCB                                                          
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         XC    LDONE(9),LDONE                                                   
         CLI   DONESW,1                                                         
         BNE   EXXMOD                                                           
*                                                                               
         BAS   RE,SETPF                                                         
*                                                                               
         MVI   LDONE,1                                                          
         MVC   LACT,BACT                                                        
         MVC   LREC,BREC                                                        
         MVC   LMED,KMED                                                        
         MVC   LCLT,KCLT                                                        
         MVC   LEST,KEST                                                        
         MVC   HDRMSG,=CL60'*** ACTION COMPLETED ***'                           
*                                                                               
         CLI   BACT,1                                                           
         BE    REQ                                                              
         CLI   BACT,2                                                           
         BE    REQ                                                              
         CLI   BACT,4              BILLING PROF CHANGE                          
         BE    REQ                                                              
*                                                                               
GTOV2    LA    R2,HDRRECH                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REQ      DS    0H                                                               
         XC    QCTL,QCTL                                                        
         MVI   QAREA,C' '                                                       
         MVC   QAREA+1(79),QAREA                                                
         MVC   QAREA(2),=C'41'                                                  
         MVC   QAREA+2(2),AGYALPHA                                              
         MVC   QAREA+4(1),HDRMED                                                
         MVC   QAREA+5(3),HDRCLT                                                
         CLI   HDRREC,C'E'                                                      
         BNE   REQ2                                                             
         LA    R2,HDRESTH                                                       
         BAS   RE,PACK                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QAREA+20(3),DUB                                                  
         B     REQ3                                                             
*                                                                               
REQ2     CLI   HDRREC,C'P'                                                      
         BNE   REQ4                                                             
*                                                                               
REQ3     MVC   QAREA+11(3),HDRPRD                                               
         OI    QAREA+13,C' '                                                    
*                                                                               
REQ4     MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,41                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                   
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    GTOV2                                                            
         SR    R3,R3                                                            
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
NXTELEM  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETPF    NTR1                                                                   
         CLI   SVSTEREO,C'Y'                                                    
         BE    JUMPXIT1                                                         
         LA    R2,HDRRECH                                                       
SETPF2   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SETPF4                                                           
         CLC   0(4,R2),=X'1020076C'                                             
         BE    SETPF4              OLD PFKEY HEADER - SHORT                     
         CLC   0(4,R2),=X'1C20076C'                                             
         BE    SETPF4              OLD PFKEY HEADER - LONG                      
*                                                                               
         CLC   2(2,R2),=AL2(23*80) PAST ROW 24/COL 1                            
         BNH   SETPF2              NO                                           
*                                                                               
* IF IN LINE 24, THINK ABOUT WHERE THE FIELD ENDS !                             
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(R2)            LENGTH OF FLDHDR                             
         AHI   RE,-8                                                            
         TM    1(R2),X'02'         TEST EXTENDED FLDHDR                         
         BZ    *+8                                                              
         AHI   RE,-8                                                            
         SR    RF,RF                                                            
         ICM   RF,3,2(R2)          GET FIELD START                              
         AR    RE,RF               GIVES END + 1                                
         CLM   RE,3,STRFLDH+2      COMPARE TO MY START POSN                     
         BL    SETPF2                                                           
         B     JUMPXIT1                                                         
*                                                                               
SETPF4   MVC   0(STRFLDX-STRFLDH+3,R2),STRFLDH                                  
         OC    SVXFRSY,SVXFRSY     TEST XFRCTL FROM ANOTHER PROGRAM             
         BZ    JUMPXIT1                                                         
         MVC   0(STRFLDX2-STRFLDH2+3,R2),STRFLDH2                               
*                                                                               
JUMPXIT1 XIT1                                                                   
*                                                                               
STRFLDH  DC    AL1(STRFLDX-STRFLDH)                                             
         DC    X'20'               PROTECTED                                    
         DC    AL2(23*80+60)       ROW 24/COL 60                                
         DC    X'00'               INDS                                         
         DC    X'00'               INPUT LENGTH                                 
         DC    X'80'               TRANSMIT                                     
         DC    AL1(STRFLDX-STRFLDH-8)                                           
         DC    C'PF2=',X'C9958696' (INFO)                                       
STRFLDX  EQU   *                                                                
         DC    X'000101'                                                        
*                                                                               
STRFLDH2 DC    AL1(STRFLDX2-STRFLDH2)                                           
         DC    X'20'               PROTECTED                                    
         DC    AL2(23*80+60)       ROW 24/COL 40                                
         DC    X'00'               INDS                                         
         DC    X'00'               INPUT LENGTH                                 
         DC    X'80'               TRANSMIT                                     
         DC    AL1(STRFLDX2-STRFLDH2-8)  DATA LENGTH                            
         DC    C'PF2=',X'C9958696' (INFO)                                       
         DC    C' PF12=',X'D985A3A49995'   (RETURN))                            
STRFLDX2 EQU   *                                                                
         DC    X'000101'                                                        
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* TEST XFR CONTROL FROM INFO OR ELSEWHERE                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TESTXFR  NTR1                                                                   
         ICM   RF,15,VGLOBBER                                                   
         BZ    TESTXFRX                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',WORK,24,GLVXCTL                               
         CLI   8(R1),0                                                          
         BNE   TESTXFRX                                                         
*                                                                               
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         LA    R1,WORK             SAVE CALLING SYS/PRG FOR OVERLAYS            
         USING GLVXFRSY,R1                                                      
         MVC   SVXFRSY,GLVXFRSY                                                 
         MVC   SVXFRPR,GLVXFRPR                                                 
         TM    GLVXFLG1,GLV1RETN   SEE IF RETURN CALL                           
         BZ    TESTXFR5                                                         
*                                                                               
* ON RETURN CLEAR TRANSFER SYS AND PROG                                         
*                                                                               
         XC    SVXFRSY,SVXFRSY                                                  
         XC    SVXFRPR,SVXFRPR                                                  
*                                                                               
* ON RETURN CALL XMIT ALL FIELDS                                                
*                                                                               
         LA    R2,HDRRECH                                                       
         SR    R0,R0                                                            
TESTXFR2 IC    R0,0(R2)                                                         
         CLI   0(R2),0             END OF SCREEN                                
         BE    TESTXFR3                                                         
         AR    R2,R0                                                            
         B     TESTXFR2                                                         
*                                                                               
TESTXFR3 MVC   0(3,R2),=X'000101'  FORCE XMT ALL FIELDS                         
         BAS   RE,SETPF            DISPLAY PFKEYS                               
         B     EXXMOD              THEN EXIT                                    
         DROP  R1                                                               
*                                                                               
TESTXFR5 GOTO1 (RF),DMCB,=C'GETF',HDRRECH,3,GLVXREC                             
         CLI   8(R1),0                                                          
         BNE   TESTXFRX            FORGET ABOUT IT THEN                         
         MVC   HDRACT(3),=C'DIS'                                                
         MVI   HDRACTH+5,3                                                      
         GOTO1 (RF),(R1),=C'GETF',HDRMEDH,1,GLVPRMD  MEDIA                      
         CLI   8(R1),0                                                          
         BNE   TESTXFRX            FORGET ABOUT IT THEN                         
         GOTO1 (RF),(R1),=C'GETF',HDRCLTH,3,GLVPRCLT CLIENT                     
         CLI   8(R1),0                                                          
         BNE   TESTXFRX            FORGET ABOUT IT THEN                         
         GOTO1 (RF),(R1),=C'GETF',HDRPRDH,3,GLVPRPRD PRODUCT                    
         CLI   8(R1),0                                                          
         BNE   TESTXFRX            FORGET ABOUT IT THEN                         
         GOTO1 (RF),(R1),=C'GETF',HDRESTH,3,GLVPREST ESTIMATE                   
         CLI   8(R1),0                                                          
         BNE   TESTXFRX            FORGET ABOUT IT THEN                         
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
TESTXFRX J     JUMPXIT1                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CALL PFM OR INF THROUGH GLOBBER. USER RETURNS WITH =SW                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CALLXFR  NTR1                                                                   
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
*                                                                               
         CLI   BYTE,C'R'           TEST RETURN                                  
         BNE   CALLXFR2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'PRI'                                                 
         MVC   GLVXFRPR,=C'FIL'                                                 
         MVC   GLVXTOSY,SVXFRSY                                                 
         MVC   GLVXTOPR,SVXFRPR                                                 
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         B     CALXFR30                                                         
*                                                                               
CALLXFR2 CLI   BYTE,C'P'           TEST PFM CALL                                
         BNE   CALXFR10                                                         
         USING GLPFMFIL,R1                                                      
         MVC   GLPFMFIL(6),=C'PRTFIL'                                           
         MVC   GLPFMDA,KEY+27                                                   
         MVC   GLPFMKEY(4),=C'*   '                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,54,GLPFMCDQ                          
         B     CALXFR20                                                         
*                                                                               
CALXFR10 CLI   BYTE,C'I'           TEST INFO CALL                               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTF',HDRRECH,,GLVXREC                          
         XC    WORK,WORK                                                        
         CLI   HDRMED,C' '         SEE IF I HAVE A MEDIA                        
         BNH   CALXFR18                                                         
         MVC   WORK(1),HDRMED                                                   
         CLI   HDRCLT,C' '         SEE IF I HAVE A CLIENT                       
         BNH   CALXFR18                                                         
         MVI   WORK+1,C','                                                      
         MVC   WORK+2(3),HDRCLT                                                 
         OC    WORK+2(3),SPACES                                                 
         CLI   HDRPRD,C' '         SEE IF I HAVE A PRODUCT                      
         BNH   CALXFR18                                                         
         CLI   WORK+4,C' '         CHECK FOR 3 CHARACTER CLIENT                 
         BNH   CALXFR12                                                         
         MVI   WORK+5,C','                                                      
         MVC   WORK+6(3),HDRPRD                                                 
         CLI   HDREST,C' '         CHECK FOR EST                                
         BNH   CALXFR18                                                         
         MVI   WORK+9,C','                                                      
         MVC   WORK+10(3),HDREST                                                
         B     CALXFR18                                                         
*                                                                               
CALXFR12 MVI   WORK+4,C','                                                      
         MVC   WORK+5(3),HDRPRD                                                 
         CLI   HDREST,C' '                                                      
         BNH   CALXFR18                                                         
         MVI   WORK+8,C','                                                      
         MVC   WORK+9(3),HDREST                                                 
*                                                                               
CALXFR18 GOTO1 (RF),(R1),=C'PUTD',WORK,20,GLVPFM                                
*                                                                               
CALXFR20 XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'PRI'                                                 
         MVC   GLVXTOSY,=C'PRI'                                                 
         B     CALXFR24                                                         
*                                                                               
CALXFR24 MVC   GLVXFRPR,=C'FIL'                                                 
         MVC   GLVXTOPR,=C'PFM'                                                 
         CLI   BYTE,C'P'           TEST PFM CALL                                
         BE    *+10                                                             
         MVC   GLVXTOPR,=C'INF'    SET FOR INFO                                 
******** OI    GLVXFLG1,GLV1GOTO                                                
         OI    GLVXFLG1,GLV1SEPS                                                
*                                                                               
CALXFR30 GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,24,GLVXCTL                           
         XC    HDRMSG,HDRMSG                                                    
         MVC   HDRMSG(24),=C'** BACK TO PRINT FILE **'                          
         OI    HDRMSG+6,X'80'      TRANSMIT                                     
         LA    R2,HDRRECH                                                       
         OI    6(R2),X'40'         SET CURSOR                                   
*                                                                               
CALXFRX  J     JUMPXIT1                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* INITIALISATION CODE                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(44),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LA    R3,IOAREA                                                        
         ST    R3,AREC                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CHI   R5,250                                                           
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         AHI   R5,-250                                                          
         B     CLEARWRK                                                         
*                                                                               
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
*                                                                               
VARCLEAR XC    0(0,R4),0(R4)                                                    
*                                                                               
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
*                                                                               
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
*                                                                               
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                OR NON NUMERIC                               
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
*                                                                               
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
*                                                                               
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             +        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
*                                                                               
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
*                                                                               
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
*                                                                               
FILE     NTR1                                                                   
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            +        
               (R2),IOAREA,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         J     JUMPXIT1                                                         
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT1                          
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         LA    R0,WRKREC+1200      INIT WK AREA (TEMP USE)                      
         LHI   R1,400*4                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         LA    R5,WRKREC+1200                                                   
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R5),(R5)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
         USING CTAGYD,RE                                                        
CKTRA20  CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
         DROP  R5,RE                                                            
*                                                                               
CKTRIDX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRAFFACC NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC LIMIT ACCESS            
*                                                                               
         MVI   BYTE3,0             WILL RETURN CODE                             
*                                                                               
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         LA    R5,PCLTREC+33                                                    
         CLC   ELCODE,0(R5)        FOUND IN FIRST ELEM?                         
         BE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         BNE   TRACCX              NO CLIENT TRAFFIC OFFICE CODE ELEM           
         MVC   BYTE3,2(R5)         RETURN CLT TRAFFIC OFFICE CODE               
*                                                                               
TRACCX   J     JUMPXIT1                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PUGENOLD                                                       
         EJECT                                                                  
*                                                                               
         DS    2000C                                                            
*                                                                               
         ORG   IOAREA                                                           
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PAGYREC                                                        
         EJECT                                                                  
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE PDIVREC                                                        
         EJECT                                                                  
*                                                                               
         ORG   IOAREA                                                           
         DS    CL500                                                            
ZZZIO    DS    0CL300                                                           
         DS    CL55                                                             
STDATE   DS    CL6                 START/END DATES OF ESTIMATE IN ZZZIO         
ENDDATE  DS    CL6                                                              
*                                                                               
         ORG   KEY                                                              
KAGY     DS    CL2                                                              
KMED     DS    CL1                                                              
KRCD     DS    CL1                                                              
KCLT     DS    CL3                                                              
KPRD     DS    CL3                                                              
KEST     DS    CL2                                                              
         DS    CL20                                                             
         ORG                                                                    
*                                  NOW AT IOAREA+2000                           
DSPSW    DS    X                                                                
DONESW   DS    X                                                                
PFKEY    DS    XL1                                                              
         DS    XL1                 SPARE                                        
*                                                                               
VGLOBBER DS    A                                                                
ATIOB    DS    A                                                                
         DS    CL40                SPARE WORK AREA BYTES                        
*                                                                               
WRKREC   DS    CL3000              TEMP RECORD WORKING STORAGE AREA             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPLFMFFD                                                       
         EJECT                                                                  
*                                                                               
         ORG   T404FFD                                                          
         DS    CL16                                                             
BREC     DS    CL1                                                              
BACT     DS    CL1                                                              
OLNUM    DS    CL1                                                              
CLTADDR  DS    F                                                                
PRDADDR  DS    F                                                                
ESTADDR  DS    F                                                                
CPROFLE  DS    CL20                                                             
LDONE    DS    CL1                                                              
LACT     DS    CL1                                                              
LREC     DS    CL1                                                              
LMED     DS    CL1                                                              
LCLT     DS    CL3                                                              
LEST     DS    CL2                                                              
SCRNUM   DS    CL1                                                              
SVAGPF12 DS    CL1                                                              
FINANSW  DS    CL1                                                              
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMF2D                                                       
         EJECT                                                                  
*                                                                               
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMF3D                                                       
         EJECT                                                                  
*                                                                               
         ORG   HDRLAST+2000                                                     
WNATION  DS    CL1                                                              
*                                                                               
SVP1USER DS    CL20                PRD USER DESCRIPTION FIELD 1                 
SVP1TYPE DS    CL1                          TYPE                                
SVP1LEN  DS    XL1                          LENGTH                              
SVP1FLG1 DS    XL1                          FLAG                                
SVP1FLG2 DS    XL1                          FLAG                                
SVP2USER DS    CL20                PRD USER DESCRIPTION FIELD 2                 
SVP2TYPE DS    CL1                          TYPE                                
SVP2LEN  DS    XL1                          LENGTH                              
SVP2FLG1 DS    XL1                          FLAG                                
SVP2FLG2 DS    XL1                          FLAG                                
SVE1USER DS    CL20                EST USER DESCRIPTION FIELD 1                 
SVE1TYPE DS    CL1                          TYPE                                
SVE1LEN  DS    XL1                          LENGTH                              
SVE1FLG1 DS    XL1                          FLAG                                
SVE1FLG2 DS    XL1                          FLAG                                
SVE2USER DS    CL20                EST USER DESCRIPTION FIELD 2                 
SVE2TYPE DS    CL1                          TYPE                                
SVE2LEN  DS    XL1                          LENGTH                              
SVE2FLG1 DS    XL1                          FLAG                                
SVE2FLG2 DS    XL1                          FLAG                                
SVULNQ   EQU   *-SVP1USER                                                       
*                                                                               
F0PROF   DS    CL16                F0 PROFILE READ IN 00                        
*                                  WHEN CLIENT IS VALIDATED                     
SVACCAGY DS    CL24                ROOM FOR 12 ACC AGENCYS                      
SVCTAGY  DS    CL2                 CTFILE ID                                    
*                                                                               
SVXFRSY  DS    CL3                 TRANSFERRED FROM SYSTEM                      
SVXFRPR  DS    CL3                 TRANSFERRED FROM PROGRAM                     
*                                                                               
SVCLSTAT DS    XL1                 PCLTSTAT FROM PCLTREC SET IN 00              
*                                  WHEN CLIENT IS VALIDATED                     
SVAGYSW  DS    XL1                 AGENCY "SWITCH" SET IN 00 AT CKVALC          
*                                  X'01' = WESTERN AGENCY (OR SJR)              
SVSTEREO DS    CL1                 'Y' IF STEREO                                
*                                                                               
SVAGPINI DS    XL2                 PAGYPINI (BIN RFP ID #) FROM AGY HDR         
*                                  SET IN 00 AT VALIDATE MEDIA (CKMED)          
SVPOFFCD DS    X'00'               SAVE PRD OFFICE CODE                         
SVPTRACD DS    X'00'               SAVE PRD TRAFFIC CODE                        
*                                                                               
SVBROWSE DS    X                   "B" = RETURNING FROM BROWSE                  
*                                                                               
ELCODE   DS    CL1                                                              
*                                                                               
SVLEST   DS    H                                                                
SVESDT   DS    CL6                 SAVED EST DATE FOR "LAST" CHECKING           
ELEM     DS    CL100                                                            
*                                                                               
SVKEY    DS    CL32                GENERAL WORKING STORAGE KEY                  
SAVEPRD  DS    CL3                                                              
SAVESCRN DS    CL3                                                              
LACT1    DS    CL1                                                              
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
*                                                                               
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLPFMD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
*                                                                               
PPRDOFFD DSECT                                                                  
       ++INCLUDE POFFPRDPP         PRODUCT OFFICE PASSIVE POINTER               
         EJECT                                                                  
*                                                                               
PPRDTRAD DSECT                                                                  
       ++INCLUDE PTRAPRDPP         PRODUCT TRAFFIC PASSIVE POINTER              
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058PPLFM00   07/09/14'                                      
         END                                                                    
