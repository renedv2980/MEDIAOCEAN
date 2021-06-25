*          DATA SET PPPUB00    AT LEVEL 220 AS OF 03/14/16                      
*PHASE T40600A                                                                  
*INCLUDE NUMED                                                                  
*INCLUDE SRCHCALL                                                               
*                                                                               
         TITLE 'CHANGE LOG'                                                     
*                                                                               
* KWAN  01/16    ZONE LOCK - ONLY NAME SCREEN ALLOWED                           
*                                                                               
* SMYE  10/10    ADD "PUB LOCK" HANDLING                                        
*                                                                               
* SMYE  11/05    TWO-CHARACTER OFFICE CHANGES                                   
*                                                                               
* YKAP  08/12/02 ACCESS AWARE                                                   
*                                                                               
* BPLA  06/05/02 FIX ADDRESS RECORD BUG IN VALCLT                               
*                                                                               
* SMYE  04/02    NEW LIMIT ACCESS SECURITY AND PUBVAL CORE-RESIDENT             
*                                                                               
* BPLA   5/00    DISALLOW PUB 0 - IT MAY CAUSE PROBLEMS                         
*                                                                               
* SMYE  12/99    ADD CLT CODE TO PR,1C,(DATE) IN CKPREM..                       
*                                                                               
* BPLA   4/99    REMOVE ADDRESS LIMIT ACCESS (EXCEPT FOR OM)                    
*                                                                               
* BPLA   4/99    LIMIT ADSIZE AND BLEAD TO SJR FOR NOW                          
*                WHEN DOING ADDRESSES CHECK FOR LIMIT ACCESS                    
*                FOR CLIENT "ALL" (OR BLANK)                                    
*                                                                               
* BPLA   3/96    CLEAR PUBFLD1 AND PBLFLD2                                      
*                                                                               
* BPLA  2/96    INCLUDE PUGENEROL (PUB VERSION OF PPGENEROL)                    
*               ALSO USE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                  
*                                                                               
* BPLA 11/22/94  OLD AOR (DUPONT) DISABLED                                      
*                                                                               
* BPLA 7/21/93  IN VALADV READ CONTROL RECORD INTO APPLWRK                      
*               INSTEAD OF IOAREA  - WASN'T BIG ENOUGH                          
* BPLA 5/24/93  CHANGE TO ALLOW FOR AOR PUB LINK SET-UP                         
*               PRIOR TO MAKING AOR "LIVE"                                      
* BPLA 12/10/91 READ CONTROL FILE TO VALIDATE ADVERTISER                        
*                                                                               
* BPLA 11/04/91 ALLOW FOR ADDR,SHIP                                             
*                                                                               
* BPLA 10/31/91 ADVERTISER CODE CHANGE                                          
*                                                                               
* BPLA 9/12/91 PPPUBWRK CREATED                                                 
*                                                                               
* BPLA 4/5/91  ADD CODE FOR ADV SCREEN PPPUB16 AND PPPUBE6                      
*                                                                               
* ROSA 2/9/90  ADD CLIENT SECURITY                                 L02          
*                                                                   L01         
* ROSA 6/6/88  SAVE NATIONALITY CODE WHEN PASSING CONTROL TO OVERL1 L01         
*                                                                   L01         
         TITLE 'T40600 - PRINTPAK PUB LOGICAL MAINT. - BASE MODULE'             
         SPACE                                                                  
T40600   CSECT                                                                  
         NMOD1 WORKL,T40600,RR=R9                                               
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         PRINT NOGEN                                                            
         LA    R9,T40600+4095                                                   
         LA    R9,1(R9)                                                         
         USING T40600+4096,R9      ** NOTE USE OF SECOND BASE REG **            
         BAS   RE,INITL                                                         
*                                                                               
         ST    R1,ASYSPARS                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         MVC   SVRECUP,VRECUP                                                   
         LA    R0,BURECUP                                                       
         ST    R0,VRECUP                                                        
         ST    R9,BASER9                                                        
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
         USING T406FFD,RA                                                       
*                                                                               
         LH    RF,=Y(SECBLK-GENOLD)                                             
         AR    RF,RC                                                            
         ST    RF,ASECBLK                                                       
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AB8'      GET PUBVAL ADDRESS                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPUBVAL,DMCB                STORE PUBVAL ADDRESS                 
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AB9'      GET PUBEDIT ADDRESS                  
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPUBEDIT,DMCB               STORE PUBEDIT ADDRESS                
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'      GET OFFICER ADDRESS                  
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VOFFICER,DMCB               STORE OFFICER ADDRESS                
*                                                                               
*******  CALL TO FASECRET TO BUILD A SECURITY AUTHORIZATION TABLE               
*                                                                               
*  ASECBLK POINTS TO 1024 BYTES OF SAVED STORAGE                                
         L     R0,ASECBLK                                                       
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*****    OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
*****    BNZ   *+14                                                             
*****    OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
*****    BZ    NOSECRET                                                         
*  INITIALIZE SECURITY BLOCK                                                    
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK),0                                 
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
NOSECRET DS    0H                                                               
*                                                                               
         EJECT                                                                  
*        B     NOTVAL                                                           
         XC    PBLMSG(60),PBLMSG                                                
         NI    PBLPUBH+1,X'FE'                                                  
         TM    PBLMEDH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         TM    PBLSCRH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         TM    PBLPUBH+4,X'20'                                                  
         BNO   NOTVAL                                                           
         TM    PBLACTH+4,X'20'                                                  
         BNZ   CKCOMB                                                           
*                                  CHANGE OF ACT AONLY                          
         CLC   PBLACT(3),=C'CHA'   IS IT CHANGE                                 
         BNE   NOTVAL                                                           
         CLI   BACT,4              WAS IT DISPLAY                               
         BNE   NOTVAL                                                           
         MVI   BACT,2                                                           
         OI    PBLACTH+4,X'20'     VAL                                          
*        B     CKCOMB                                                           
*        B     NOTVAL                                                           
         B     CKACT                                                            
NOTVAL   NI    PBLMEDH+4,X'DF'          UNVALIDATE ALL PBL FIELDS               
         NI    PBLACTH+4,X'DF'                                                  
         NI    PBLSCRH+4,X'DF'                                                  
         NI    PBLPUBH+4,X'DF'                                                  
         XC    PUBADDR(8),PUBADDR  CLEAR DISK ADDRS                             
         XC    PBLFLD1(22),PBLFLD1                                              
         XC    PBLFLD2(22),PBLFLD2                                              
         FOUT  PBLFLD1H                                                         
         FOUT  PBLFLD2H                                                         
*                                                                               
*   VALIDATE MEDIA                                                              
         LA    R2,PBLMEDH                                                       
         LA    R3,MEDERR                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),PBLMED                                                  
         MVI   KEY+3,X'01'                                                      
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(4),KEY                                                   
         BNE   ERROR                                                            
         BAS   RE,GETREC                                                        
         MVC   APROF(1),PAGYPROF+16      SAVE SRDS PROF BYTE                    
*                                                                   L01         
         MVC   ANATION,PAGYNAT           SAVE NATIONALALITY         L01         
         MVC   APROF13,PAGYPROF+12                                              
MVMED    MVC   BMED,PBLMED                                                      
         TM    4(R2),X'20'                                                      
         BO    CKACT                                                            
         FOUT  PBLPUBNH,SPACES,20                                               
         FOUT  PBLPUBCH,SPACES,16                                               
         FOUT  PBLZONH,SPACES,20                                                
         FOUT  PBLSRDSH,SPACES,8                                                
         NI    PBLPUBH+4,X'DF'                                                  
         OI    4(R2),X'20'                                                      
         MVI   FORMAT,1            SET ACTION = FORMAT                          
         B     CKACT                                                            
*                                                                               
*        **NAME SEARCH CALL **                                                  
*                                                                               
*        DO PUB NAME SEARCH HERE SO THEY DON'T HAVE TO ENTER                    
*        THE ACTION AND SCREEN TO DO A SEARCH                                   
*                                                                               
CKACT    LA    R2,PBLPUBH                                                       
         SR    R2,RA               GET DISPL OF KEY FIELD                       
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         MVC   DSPARM(DSPARML),SPACES                                           
         MVC   DSMEDCOD,BMED       SET MEDIA CODE                               
         DROP  R3                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
*                                                                               
*                                                                               
*   VALIDATE ACTION                                                             
*                                                                               
CKACT0   LA    R2,PBLACTH                                                       
         LA    R3,ACTERR                                                        
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         LA    R5,ACTIONS                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,RELO                                                          
         LA    R5,6(R5)                                                         
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
CKACT1   EX    R4,COMP                                                          
         BE    CKACT2                                                           
         BXLE  R5,R6,CKACT1                                                     
         B     ERROR                                                            
CKACT2   CLI   7(R5),3                                                          
         BE    CKAPROF                                                          
         CLI   7(R5),5                                                          
         BE    CKAPROF                                                          
         B     CKACT2A                                                          
*                                                                               
CKAPROF  CLI   BMED,C'O'           OUTDOOR - BYPASS PROFILE CHK                 
         BE    CKACT2A                                                          
         CLI   APROF,C'0'                                                       
         BNE   CKACT2A                                                          
         B     ERROR       STND OR COPY INVALID                                 
*                                                                               
CKACT2A  MVC   BACT(1),7(R5)                                                    
         OI    PBLACTH+4,X'20'     VALIDATE                                     
         FOUT  PBLACTH                                                          
         B     ACCESSW                                                          
COMP     CLC   8(0,R2),0(R5)                                                    
*                                                                               
*   ACCESS AWARE                                                                
*                                                                               
ACCESSW  LA    R2,PBLSCRH                  R2 POINTS TO HEADER OF               
*                                          THE SCREEN FIELD                     
         LA    R4,SCRTAB2                  R4 POINTS TO TO TABLE                
         USING SCRTAB2D,R4                                                      
*                                                                               
ACCESSW1 DS    0H                                                               
         CLI   SCRNAME,X'FF'               CHECK IF END OF TABLE                
         BE    CKSCRN                                                           
         CLI   5(R2),3                     IF LESS 3 CHAR GO TO VALID           
         BL    CKSCRN                                                           
*                                                                               
         ZIC   R1,5(R2)                    FIRST CHECK AGAINEST WHAT            
         AHI   R1,-1                       WAS ENTERED ON A SCREEN              
         EX    R1,ACCESSW3                 TO THE TABLE                         
         B     ACCESSW4                                                         
*                                                                               
ACCESSW3 CLC   SCRNAME(0),8(R2)                                                 
ACCESSW4 BE    ACCESSW8                                                         
         ZIC   R1,SCRLENG                  SECOND CHECK WHAT IN THE             
         EX    R1,ACCESSW5                 TABLE TO WHAT WAS ENTERED            
         B     ACCESSW6                    ON SCREEN                            
*                                                                               
ACCESSW5 CLC   8(0,R2),SCRNAME                                                  
ACCESSW6 BE    ACCESSW8                                                         
         LA    R4,SCRLENQ(R4)                                                   
         B     ACCESSW1                                                         
*                                                                               
ACCESSW8 DS    0H                                                               
         LA    R6,=AL1(4)            R6=A(ACTION)                               
         ICM   R6,8,SCRCODE        PASS RECORD NUMBER IN HIGH BIT               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPRACT',ASECBLK),(R6)                              
         CLI   DMCB,SECPYES        CHECK IF DISP IS ALLOWED                     
         BNE   ACCESSW9                                                         
*                                                                               
         LA    R6,7(R5)            BACT             R6=A(ACTION)                
         ICM   R6,8,SCRCODE        PASS RECORD NUMBER IN HIGH BIT               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPRACT',ASECBLK),(R6)                              
         CLI   DMCB,SECPYES         CHECK IF ACTION FROM SCREEN                 
         BNE   ACCESSW9             IS ALLOWED WITH "SCREEN"                    
         B     CKSCRN                                                           
*                                                                               
ACCESSW9 DS    0H                    RETURN ERROR                               
         LA    R3,ACCERR                                                        
         B     ERROR                                                            
         DROP  R4                                                               
*                                                                               
*   VALIDATE SCREEN                                                             
*                                                                               
*                                                                               
CKSCRN   LA    R2,PBLSCRH                                                       
         XC    BCODE,BCODE         NEED TO CLEAR BCODE                          
         CLI   BACT,5                                                           
         BL    CKSCRNA                                                          
         BH    CKSCRN0             ACTION = LIST                                
         LA    R3,COMBERR                                                       
         CLC   AGYALPHA(2),=C'ZZ'                                               
         BE    ERROR                                                            
CKSCRN0  EQU   *                                                                
*                                                                               
         CLI   BACT,6              ACTION=LIST ?                                
         BNE   CKSCRN02            NO                                           
         CLI   5(R2),0                                                          
         BE    CKSCRN04            OKAY                                         
         CLC   8(6,R2),=C'LOCK=Y'                                               
         BE    CKSCRN04            OKAY                                         
         CLC   8(6,R2),=C'LOCK=N'                                               
         BE    CKSCRN04            OKAY                                         
*                                                                               
CKSCRN02 EQU   *                                                                
         LA    R3,SCRNERR                                                       
         CLI   5(R2),0                                                          
         BNE   ERROR                                                            
CKSCRN04 EQU   *                                                                
         MVI   BSCR,0                                                           
         B     PRTSCRN       VALIDATE                                           
*                                                                               
CKSCRNA  LA    R3,SCRNERR                                                       
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         CLC   8(4,R2),=C'DRD,'                                                 
         BE    CKDRD                                                            
         CLC   8(4,R2),=C'SAUR'                                                 
         BE    CKRATE                                                           
         CLC   8(5,R2),=C'RCODE'                                                
         BE    CKRCODE                                                          
         CLC   8(6,R2),=C'CLIENT,'                                              
         BE    CKCLT                                                            
         CLC   8(4,R2),=C'RATE'                                                 
         BE    CKRATE                                                           
         CLC   8(4,R2),=C'RATZ'                                                 
         BE    CKRATE                                                           
         CLC   8(3,R2),=C'PR,'                                                  
         BE    CKPREM                                                           
         CLC   8(8,R2),=C'ADDR,PAY'                                             
         BE    CKADR                                                            
         CLC   8(8,R2),=C'ADDR,TRA'                                             
         BE    CKADR                                                            
         CLC   8(8,R2),=C'ADDR,CON'                                             
         BE    CKADR                                                            
         CLC   8(9,R2),=C'ADDR,SHIP'                                            
         BE    CKSADR                                                           
         CLC   8(6,R2),=C'AORADV,'                                              
         BE    CKADV                                                            
         CLC   8(6,R2),=C'AORADV='                                              
         BE    CKADV                                                            
*                                                                               
         LA    R5,SCREENS                                                       
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         A     R7,RELO                                                          
         LA    R5,6(R5)                                                         
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
CKSCRN1  EX    R4,COMP                                                          
         BE    MUSCRN                                                           
         BXLE  R5,R6,CKSCRN1                                                    
         B     ERROR                                                            
MUSCRN   MVC   BSCR(1),7(R5)                                                    
***                                                                             
***      TEMPORAY CODE UNTIL ADSIZING IS RELEASED                               
***                                                                             
         CLC   AGYALPHA,=C'SJ'                                                  
         BE    PRTSCRN                                                          
         CLI   BSCR,X'17'    NO ADSIZE                                          
         BE    ERROR                                                            
         CLI   BSCR,X'18'    OR BLEED UNLESS SJR                                
         BE    ERROR                                                            
****                                                                            
         B     PRTSCRN       VALIDATE                                           
*                                                                               
*                                                                               
CKCLT    CLI   5(R2),10                                                         
         BH    ERROR                                                            
         CLI   5(R2),9                                                          
         BL    ERROR                                                            
         LA    R6,15(R2)                                                        
         BAS   R8,VALCLT                                                        
         MVI   BSCR,X'0A'                                                       
         B     PRTSCRN                                                          
*                                                                               
*                                                                               
CKADV    CLI   5(R2),12                                                         
         BE    CKADV5                                                           
*                                                                               
         CLI   5(R2),13                                                         
         BNE   ERROR                                                            
CKADV5   LA    R6,15(R2)                                                        
         BAS   R8,VALADV                                                        
         MVI   BSCR,X'16'                                                       
         B     PRTSCRN                                                          
*                                                                               
*                                                                               
*                                                                               
PRTSCRN  OI    PBLSCRH+4,X'20'                                                  
         FOUT  PBLSCRH                                                          
         B     CKCOMB                                                           
*                                                                               
PACKIT   PACK  DUB,0(0,R4)                                                      
SAVER4   DS    F                                                                
*                                                                               
CKRCODE  LA    R4,13(R2)                                                        
         XC    BSPACE,BSPACE                                                    
         CLI   5(R2),5                                                          
         BNE   CKRCD5                                                           
         BAS   R8,GETTODAY                                                      
         B     CKRATX                                                           
*                                                                               
CKRCD5   B     CKRAT2                                                           
*                                                                               
CKRATE   LA    R4,12(R2)                                                        
         XC    BSPACE,BSPACE                                                    
         CLI   5(R2),4                                                          
         BNE   CKRAT1                                                           
         BAS   R8,GETTODAY                                                      
         B     CKRATX                                                           
*                                                                               
CKRAT1   CLC   0(2,R4),=C',,'                                                   
         BNE   CKRAT2                                                           
         CLI   BMED,C'N'                                                        
         BE    ERROR                                                            
         BAS   R8,GETTODAY                                                      
         LA    R4,2(R4)                                                         
         CLI   5(R2),6                                                          
         BE    CKRATX                                                           
         BAS   R8,GETDESC                                                       
         B     CKRATX                                                           
*                                                                               
CKRAT2   CLI   0(R4),C','                                                       
         BNE   ERROR                                                            
         LA    R4,1(R4)                                                         
         GOTO1 VDATVAL,DMCB,(0,0(R4)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   CKRAT2A                                                          
         LA    R3,DATERR                                                        
         B     ERROR                                                            
*                                                                               
CKRAT2A  BAS   R8,CONDATE       CONVERT DATE                                    
         LA    R5,9                                                             
CKRAT2B  CLI   0(R4),C','                                                       
         BE    CKRAT3                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,CKRAT2B                                                       
         B     CKRATX                                                           
*                                                                               
CKRAT3   LA    R4,1(R4)                                                         
         CLI   BMED,C'N'                                                        
         BE    ERROR                                                            
         BAS   R8,GETDESC                                                       
*                                                                               
CKRATX   MVI   BSCR,X'E2'                                                       
         CLC   PBLSCR(4),=C'SAUR'                                               
         BE    PRTSCRN                                                          
         MVI   BSCR,X'E4'                                                       
         CLC   PBLSCR(5),=C'RCODE'   RATE CODES                                 
         BE    PRTSCRN                                                          
         MVI   BSCR,X'07'                                                       
         CLI   BMED,C'N'                                                        
         BE    PRTSCRN                                                          
         MVI   BSCR,X'0E'                                                       
         CLI   BMED,C'O'           OUTDOOR RATES                                
         BE    CKRATX2                                                          
         MVI   BSCR,X'0B'                                                       
         MVI   BSCR,X'0B'                                                       
CKRATX2  CLC   PBLSCR(4),=C'RATZ'                                               
         BNE   PRTSCRN                                                          
         OI    BSCR,X'10'          SECRET RATE                                  
         LA    R3,SCRNERR                                                       
         CLI   BMED,C'O'          OUTDOOR ONLY                                  
         BNE   ERROR                                                            
         CLC   AGYALPHA,=C'ZZ'                                                  
         BNE   ERROR                                                            
         B     PRTSCRN                                                          
*                                                                               
*                                                                               
CKPREM   LA    R3,SCRNERR1                                                      
         CLI   5(R2),5                                                          
         BL    ERROR                                                            
         XC    BCLT,BCLT           PREP FOR POSSIBLE CLT CODE ENTRY             
         CLC   11(2,R2),=C'1C'                                                  
         BE    MVTYPE                                                           
         CLC   11(2,R2),=C'2C'                                                  
         BE    MVTYPE                                                           
         CLC   11(2,R2),=C'3C'                                                  
         BE    MVTYPE                                                           
         CLC   11(2,R2),=C'4C'                                                  
         BNE   ERROR                                                            
MVTYPE   MVC   BCODE(2),11(R2)                                                  
         MVI   BCODE+2,C' '                                                     
         CLI   13(R2),C','                                                      
         BE    CKPDATE                                                          
         CLI   5(R2),5                                                          
         BNE   ERROR                                                            
*                                  GETS TODAY'S DATE                            
CKPREM1  GOTO1 VDATCON,DMCB,(5,0),(3,BDATE)                                     
CKPREM3  MVI   BSCR,X'06'                                                       
         B     PRTSCRN       VALIDATE                                           
*                                                                               
CKPDATE  DS    0H                                                               
         LA    R3,DATERR           FORMAT= PR,1C,DATE      OR                   
         CLI   5(R2),7                     PR,1C,DATE,CLT  OR                   
         BL    ERROR                       PR,1C,CLT                            
         XC    WORK,WORK                                                        
         LA    R7,14(R2)           POINT R7 TO "THIRD" FIELD                    
         LR    R6,R7                  AND ALSO R6                               
CKPD20   CLI   0(R7),C','          COMMA DELIMITER?                             
         BE    CKPD40              YES - ANOTHER FIELD SHOULD "FOLLOW"          
         CLI   0(R7),C' '          ANYTHING ?                                   
         BNH   CKPD60              NO - ONLY ONE FIELD AFTER PR,1C              
         LA    R7,1(R7)            BUMP TO NEXT POSITION                        
         B     CKPD20                                                           
CKPD40   DS    0H           3RD FIELD MUST BE DATE IF 4TH FIELD PRESENT         
         SR    R7,R6               R7 NOW HAS LENGTH OF DATE                    
         CHI   R7,5                                                             
         BL    ERROR               INVALID DATE (TOO SHORT)                     
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   WORK+20(0),14(R2)   MOVE DATE ONLY TO WORK+20                    
         GOTO1 VDATVAL,DMCB,(0,WORK+20),WORK                                    
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR               INVALID DATE                                 
         LA    R6,1(R6,R7)         POINT R6 TO COMMA AFTER DATE                 
         B     CKPD80                                                           
*                                                                               
CKPD60   GOTO1 VDATVAL,DMCB,(0,14(R2)),WORK                                     
         ICM   R7,15,DMCB                                                       
         BZ    CKPD70              NOT A VALID DATE                             
         LA    R6,14(R2)           VALID DATE FOUND                             
         AR    R6,R7               POINT R6 TO POS'N AFTER DATE                 
         CLI   0(R6),C' '          ANYTHING THERE ?                             
         BH    ERROR               YES - ERROR                                  
         B     CKPD80              DATE ALONE ENTERED (NO CLIENT CODE)          
CKPD70   CLI   5(R2),9             MAX LENGTH FOR CLIENT (ALONE)                
         BNH   CKPCLT1             SEE IF VALID CLIENT (WITHOUT DATE)           
         B     ERROR               DATE MUST BE WRONG                           
*                                                                               
CKPD80   GOTO1 VDATCON,DMCB,(0,WORK),(3,BDATE)                                  
         CLI   0(R6),C','          DELIMITER AFTER DATE ?                       
         BNE   CKPREM3             DATE ALONE ENTERED (NO CLIENT CODE)          
*                                  VALIDATE CLIENT (DATE ENTERED)               
         LA    R6,1(R6)            POINT TO CLIENT CODE                         
         BAS   R8,VALCLT                                                        
         B     CKPREM3             OK - USE DATE ENTERED                        
*                                                                               
CKPCLT1  DS    0H                  VALIDATE CLIENT (NO DATE ENTERED)            
         LA    R6,14(R2)           POINT TO CLIENT CODE                         
         BAS   R8,VALCLT                                                        
         B     CKPREM1             OK - USE TODAY'S DATE                        
*                                                                               
*                                                                               
*                                  DRD SCREEN                                   
*                                  VAL CLT/DIV                                  
CKDRD    DS    0H                                                               
         LA    R4,12(R2)                                                        
         MVC   WORK2(3),0(R4)       CLT                                         
         ZIC   R5,5(R2)            TOTAL LENGHT                                 
         SH    R5,=H'8'            ADJUST FOR DRD=XXX,                          
         CLI   WORK2+2,C'A'                                                     
         BNL   CKDRD10                                                          
         MVI   WORK2+2,C' '                                                     
         LA    R5,1(R5)            RESET FOR 2 CHAR CLT                         
         BCTR  R4,R0                                                            
CKDRD10  LTR   R5,R5                                                            
         BNZ   CKDRD15                                                          
CKDRDER  LA    R3,DIVERR                                                        
         B     ERROR                                                            
*                                                                               
CKDRD15  C     R5,=F'3'                                                         
         BH    CKDRDER                                                          
         LR    RE,R5                                                            
         LR    R6,R4                                                            
CKDRD17  CLI   4(R6),C'0'          NUMERICS CHECK                               
         BL    CKDRDER                                                          
         LA    R6,1(R6)                                                         
         BCT   RE,CKDRD17                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,4(0,R4)                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK2+3(3),DUB                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,2                                                          
         MVC   KEY+4(3),WORK2                                                   
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
*                                  CHK FOR LIMIT ACCESS                         
         BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON ?                         
         BNE   CKDRDGO             NO                                           
         BRAS  RE,TRAFFACC         LOOK FOR CLIENT TRAFFIC OFFICE CODE          
         CLI   BYTE3,0             ANYTHING FOUND ?                             
         BE    CKDRDGO             NO                                           
         XC    TSTOFF2,TSTOFF2                                                  
         MVC   PCLTOFF,BYTE3       USE CLIENT TRAFFIC OFFICE CODE TO..          
CKDRDGO  DS    0H                  TEST OFFICE SECURITY (ANY ACCESS)            
         BAS   R5,CKACC                                                         
         FOUT  PBLFLD1H,PCLTNAME,20                                             
*                                                                               
         MVC   BCLT,WORK2                                                       
*                                                                               
         MVI   KEY+3,3                                                          
         MVC   KEY+7(3),WORK2+3     DIV                                         
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         FOUT  PBLFLD2H,PDIVNAME,20                                             
*                                                                               
         MVC   BDIV,WORK2+3                                                     
*                                                                               
         OI    PBLSCRH+4,X'20'                                                  
         MVI   BSCR,X'08'                                                       
         B     CKCOMB                                                           
*                                                                               
CKADR    MVC   BCODE,13(R2)       SAVE PAY,CON,TRA                              
         MVC   BCLT,=3X'FF'                                                     
         CLI   5(R2),8                                                          
         BE    CKADRX                                                           
         CLI   16(R2),C','                                                      
         BNE   ERROR                                                            
         CLI   5(R2),12         CLT CODE MUST BE 2 OR 3 CHARS                   
         BH    ERROR                                                            
         CLI   5(R2),11                                                         
         BL    ERROR                                                            
         LA    R6,17(R2)                                                        
         BAS   R8,VALCLT                                                        
CKADRX   MVI   BSCR,X'0C'                                                       
*                                                                               
         OC    T406FFD+6(2),T406FFD+6   CHECK FOR LIMIT ACCESS                  
         BZ    PRTSCRN                                                          
*                                                                               
         CLC   AGYALPHA,=C'OM'      LIMIT ACCESS ONLY FOR OM                    
         BNE   PRTSCRN              (FOR NOW)                                   
*                                                                               
         CLC   BCLT,=3X'FF'                                                     
         BE    CKACCE             LIMIT ACCESS ERROR                            
*                      (SO THEY CAN'T CHANGE ALL CLT ADDRESSES)                 
         B     PRTSCRN                                                          
*                                                                               
CKSADR   MVC   BCODE,13(R2)       SAVE PAY,CON,TRA                              
         MVC   BCLT,=3X'FF'                                                     
         CLI   5(R2),9                                                          
         BE    CKSADRX                                                          
         CLI   17(R2),C','                                                      
         BNE   ERROR                                                            
         CLI   5(R2),13         CLT CODE MUST BE 2 OR 3 CHARS                   
         BH    ERROR                                                            
         CLI   5(R2),12                                                         
         BL    ERROR                                                            
         LA    R6,18(R2)                                                        
         BAS   R8,VALCLT                                                        
*                                                                               
         CLI   BCLT,X'FF'       ACCEPT 'ALL' OR *                               
         BE    CKSADRX                                                          
         LA    R3,SCRNERR                                                       
         B     ERROR            DISALLOW CLIENT                                 
*                                                                               
CKSADRX  MVI   BSCR,X'0C'                                                       
         B     PRTSCRN                                                          
*                                                                               
*              VALIDATE MEDIA/ACTION/SCREEN COMBINATION & GET OVLY NO.          
CKCOMB   MVC   SAVEMED,BMED                                                     
         CLI   BMED,C'N'                                                        
         BE    *+8                                                              
         MVI   BMED,C'M'                                                        
         LA    R5,COMBOS                                                        
         LA    R6,5                                                             
         LA    R7,COMBOSX-1                                                     
CKCOMB1  CLC   BMED(3),0(R5)                                                    
         BE    CKCOMB5                                                          
         BXLE  R5,R6,CKCOMB1                                                    
CKCOMERR NI    PBLMEDH+4,X'DF'                                                  
         LA    R2,PBLMEDH                                                       
         LA    R3,COMBERR                                                       
         B     ERROR                                                            
*                                                                               
CKCOMB5  CLI   T406FFD+1,C'*'                                                   
         BE    CKPUB               DDS TERM                                     
         CLI   4(R5),0             CHK FOR ACTION RESTRICTION                   
         BE    CKPUB               NONE                                         
         MVC   WORK(1),4(R5)                                                    
         CLI   WORK,X'38'          CHK FOR ADDR SCREEN                          
         BNE   CKCOMB10                                                         
         MVI   WORK,X'20'          SET TO PAY                                   
         CLC   BCODE,=C'PAY'                                                    
         BE    CKCOMB10                                                         
         MVI   WORK,X'10'          TRAFFIC ADDR                                 
         CLC   BCODE,=C'TRA'                                                    
         BE    CKCOMB10                                                         
         CLC   BCODE,=C'SHI'       OR SHIPPING ADDR                             
         BE    CKCOMB10                                                         
         MVI   WORK,X'08'          MUST BE CON                                  
*                                                                               
CKCOMB10 NC    WORK(1),T406FFD+12  'AND' RESTRICTED ACCESS BITS                 
         BZ    CKPUB               RESTRICTED ACCESS BIT NOT ON                 
*                                  IF STILL ON - THEN ERROR                     
         NI    PBLMEDH+4,X'DF'     UNVALIDATE MEDIA                             
         LA    R2,PBLACTH          CURSOR TO ACTION                             
         LA    R3,ACCERR           ACCESS RESTRICTED                            
         B     ERROR                                                            
*                                                                               
*        MEDIA/ACTION/SCRN  NOW OK SO EDIT PUBLICATION                          
*                                                                               
CKPUB    DS    0H                                                               
*                                                                               
         MVC   BMED,SAVEMED        RESTORE MEDIA                                
         MVC   OLNUM(1),3(R5)                                                   
         LA    R2,PBLPUBH                                                       
         LA    R2,PBLPUBH                                                       
         LA    R3,PUBERR                                                        
         XC    KEY,KEY                                                          
         MVC   KAGY,AGYALPHA                                                    
         CLI   BACT,3              SEE IF ACTION=SRDS                           
         BNE   *+10                                                             
         MVC   KAGY,=C'ZZ'                                                      
         MVI   KRCD,X'81'                                                       
         MVC   KMED,BMED                                                        
         CLI   8(R2),C'0'                                                       
         BNL   CKPUB0          NUMBER ENTERED                                   
         CLI   BACT,6            WORD FOR LIST ONLY                             
         BNE   ERROR                                                            
         MVI   DMCB,X'FF'       SO PLIST WILL CHECK FIRST + NEXT                
         B     PLIST                                                            
*                                                                               
CKPUB0   GOTO1 VPUBVAL,DMCB,(5(R2),PBLPUB),(0,KPUB)                             
         CLI   BACT,6              TEST LIST                                    
         BE    PLIST                                                            
         CLI   DMCB,X'FF'          PUB INVALID                                  
         BE    ERROR                                                            
*                                                                               
         CLI   BACT,1               SEE IF ADDING                               
         BNE   CKPUB0B                                                          
         OC    KPUB(4),KPUB        CHECK FOR 0 BASE PUB NUMBER                  
         BZ    ERROR               INVALID PUB                                  
*                                                                               
CKPUB0B  DS    0H                                                               
*                                                                               
         MVC   BPUB(6),KPUB                                                     
         CLI   BACT,5                                                           
         BE    COPY                                                             
         CLI   BSCR,1              SEE IF NAME SCREEN                           
         BNE   *+12                NO                                           
         CLI   BACT,1              SEE IF ADD                                   
         BE    CKADD                                                            
         BAS   RE,HIGHPUB                                                       
         CLC   KEYSAVE(25),KEY                                                  
         BE    PUBOK                                                            
         LA    R3,53       REC NOT FOUND                                        
         CLI   BMED,C'O'           OUTDOOR - ALLOW ACT=STND                     
         BNE   CKPUB0C             EVEN IF INVALID PROFILE                      
         CLI   BACT,3                                                           
         BE    CKPUB0E                                                          
*                                                                               
CKPUB0C  DS    0H                                                               
         CLI   APROF,C'0'             NO DEFAULT TO ZZ                          
         BE    ERROR                                                            
CKPUB0E  MVI   KEYSAVE+9,X'85'                                                  
         CLC   KEYSAVE(25),KEY     SEE IF I'VE GOT LTLREC                       
         BNE   CKPUB1                                                           
         MVC   LTLADDR(4),KEY+27                                                
         B     CKPUB2                                                           
*                                                                               
CKPUB1   MVI   KEYSAVE+9,X'81'                                                  
         MVC   KEYSAVE+7(2),=C'ZZ'                                              
         CLC   KEYSAVE(25),KEY     SEE IF I'VE GOT SRDS REC                     
         BE    PUBOK                                                            
*                                                                               
CKPUB2   MVC   KMED,BMED                                                        
         MVC   KPUB,BPUB                                                        
         MVC   KAGY(2),=C'ZZ'                                                   
         MVI   KRCD,X'81'                                                       
         BAS   RE,READPUB                                                       
         TM    KEY+25,X'01'        PASSIVE POINTER                              
         BZ    *+12                                                             
         LA    R3,53                                                            
         B     ERROR                                                            
*                                                                               
PUBOK    TM    KEY+25,X'01'        PASSIVE POINTER                              
         BZ    *+12                                                             
         LA    R3,53                                                            
         B     ERROR                                                            
         MVC   PUBADDR(4),KEY+27   SAVE ADDRESS                                 
         CLI   BACT,3                                                           
         BE    PUBOK1                                                           
         MVI   KRCD,X'85'          FIND LTLREC,IF IT EXISTS                     
         MVC   KAGY,AGYALPHA                                                    
         BAS   RE,HIGHPUB                                                       
         CLC   KEYSAVE(25),KEY                                                  
         BNE   PUBOK1                                                           
         MVC   LTLADDR(4),KEY+27   SAVE ADDRESS                                 
         B     PUBOK1                                                           
*                                                                               
*        PUBADDR NOW CONTAINS DISK ADDRESS OF DESIRED PUB AND                   
*        LTLADDR CONTAINS THE DISK ADDRESS OF THE LTLREC IF THERE               
*     NOW READ PUBREC TO GET NAME AND CITY                                      
*                                                                               
PUBOK1   MVC   KMED,BMED                                                        
         MVC   KPUB,BPUB       RESTORE KEY                                      
         TM    PBLPUBH+4,X'20'         SEE IF PREVIOUSLY VALIDATED              
         BO    GETOVLY                                                          
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
         LA    R4,PUBIO+33                                                      
         XC    PBLSRDS,PBLSRDS                                                  
         CLC   PUBKAGY,=C'ZZ'                                                   
         BNE   *+10                                                             
         MVC   PBLSRDS,=C'**STND**'                                             
         FOUT  PBLSRDSH                                                         
CKELEM   CLI   0(R4),0                                                          
         BE    NONAMEL                                                          
         CLI   0(R4),X'10'                                                      
         BNE   NEXTEL                                                           
         USING PUBNAMEL,R4                                                      
*                                                                               
         TM    PUBLOCSW,PUBZLCKQ   ZONE LOCKED?                                 
         JZ    PUBOK20                                                          
         OC    PUBIO+5(2),PUBIO+5  HAVE ZONE/EDITION?                           
         JZ    PUBOK20                                                          
         CLI   BSCR,1              NAME SCREEN?                                 
         JE    PUBOK20                                                          
         LA    R3,ACTERR                                                        
         J     ERROR               NAME SCREEN ONLY FOR ZONE LOCKED             
*                                                                               
PUBOK20  FOUT  PBLPUBNH,PUBNAME,20                                              
         FOUT  PBLPUBCH,PUBCITY,16                                              
         FOUT  PBLZONH,PUBZNAME                                                 
         CLC   PUBKAGY(2),=C'ZZ'                                                
         BNE   PUBOKX                                                           
         CLI   BSCR,X'0A'          NO CLT SCREEN FOR AGY ZZ                     
         BE    CKCOMERR                                                         
PUBOKX   OI    PBLPUBH+4,X'20'     VALIDATE                                     
         MVI   FORMAT,1            SET ACTION = FORMAT                          
         B     GETOVLY                                                          
*                                                                               
         DROP  R4                                                               
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CKELEM                                                           
*                                                                               
NONAMEL  DC    H'0'                FATAL ERROR NO NAME ELEMENT                  
*                                                                               
*                                                                               
*                                                                               
*         PUBNAMEL MUST BE FIRST ELEMENT ADDED, IF ACTION =ADD                  
*         AND SCREEN=NAME THEN PUBREC CANNOT BE ON FILE                         
*                                                                               
CKADD    TM    PBLPUBH+4,X'20'        SEE IF PREVIOUSLY VALIDATED               
         BO    GETOVLY                                                          
         LA    R4,=C'DMREAD'                                                    
         LA    R5,=C'PUBDIR'                                                    
         LA    R6,KEY                                                           
         LA    R7,KEYSAVE                                                       
         LA    R1,DMCB                                                          
         STM   R4,R7,0(R1)                                                      
         MVI   0(R1),X'08'         SET TO PASS DELETED RECORDS                  
         MVC   16(1,R1),TERMNAL                                                 
         L     RF,VDATAMGR                                                      
         BASR  RE,RF                                                            
         LA    R3,51                                                            
         TM    8(R1),X'40'         DISK ERROR                                   
         BNZ   ERROR                                                            
         CLC   KEY(25),KEYSAVE                                                  
         BNE   CKADD1                                                           
         CLI   BSCR,1              SEE IF BSCR= NAME, IF YES ERROR              
         LA    R3,PUBERR1                                                       
         BE    ERROR                                                            
         TM    KEYSAVE+25,X'80'    SEE IF RECORD DELETED                        
         LA    R3,56                                                            
         BNZ   ERROR                                                            
         MVC   PUBADDR(4),KEYSAVE+27    SAVE DISK ADDR                          
         B     ADDOK                                                            
*                                                                               
CKADD1   CLI   BSCR,1                                                           
         LA    R3,PUBERR2                                                       
         BNE   ERROR                                                            
ADDOK    MVI   KRCD,X'85'                                                       
         BAS   RE,HIGHPUB          READ PUBFILE FOR LTLREC                      
         CLC   KEYSAVE(25),KEY                                                  
         BNE   *+10                                                             
         MVC   LTLADDR(4),KEY+27   SAVE LTLREC ADDRESS                          
         MVI   KRCD,X'81'                                                       
         MVI   FORMAT,1            SET ACTION = FORMAT                          
         OI    4(R2),X'20'         VALIDATE PUB                                 
         B     GETOVLY                                                          
*                                                                               
*                                                                               
*                                                                               
*                                                                               
COPY     CLI   BMED,C'O'           OUTDOOR                                      
         BNE   COPY10                                                           
COPY05E1 LA    R3,ACTERR                                                        
         CLI   BPUB+5,0            CAN'T COPY P OR R                            
         BNE   ERROR                                                            
*                                                                               
COPY10   BAS   RE,HIGHPUB                                                       
         LA    R3,COPYERR                                                       
         CLC   KEYSAVE(25),KEY                                                  
         BE    ERROR                                                            
         MVC   KEYSAVE+7(2),=C'ZZ'                                              
         CLC   KEYSAVE(25),KEY                                                  
         BE    VALCOPY                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(25),KEYSAVE                                                  
         BAS   RE,READPUB                                                       
*                                                                               
VALCOPY  BAS   RE,GETPUB                                                        
         LA    R4,PUBREC+33                                                     
         CLI   0(R4),X'10'                                                      
         BNE   NONAMEL                                                          
*                                                                               
         USING PUBNAMEL,R4                                                      
         TM    PUBLOCSW,PUBZLCKQ   ZONE LOCKED?                                 
         JZ    COPY30                                                           
         OC    PUBREC+5(2),PUBREC+5                                             
         JNZ   COPY05E1            CAN'T COPY ZONE LOCKED ZONES/EDTS            
*                                                                               
COPY30   FOUT  PBLPUBNH,PUBNAME,20                                              
         FOUT  PBLPUBCH,PUBCITY,16                                              
         OI    PBLPUBH+4,X'20'                                                  
         MVC   PUBKAGY(2),AGYALPHA                                              
         MVC   KEY(25),PUBKEY                                                   
         BAS   RE,ADDPUB                                                        
         MVC   PBLMSG,=CL60'*** ACTION COMPLETED ***'                           
         B     DOREQ                                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
PLIST    EQU   *                                                                
         CLI   DMCB,X'FF'          TEST PUBVAL ERROR                            
         BE    *+14                                                             
         MVC   BPUB,KPUB                                                        
         B     GETOVLY                                                          
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,R0                                                            
         LA    R5,=C'FIRST'                                                     
         EX    R4,PLCOMP                                                        
         BNE   *+14                                                             
         XC    BPUB,BPUB                                                        
         B     GETOVLY                                                          
         LA    R5,=C'NEXT'                                                      
         EX    R4,PLCOMP                                                        
         BNE   ERROR                                                            
         LA    R3,NXTERR                                                        
         OC    BPUB,BPUB                                                        
         BZ    ERROR                                                            
         OI    PBLPUBH+1,X'01'     SET TO MODIFIED                              
         B     GETOVLY                                                          
PLCOMP   CLC   8(0,R2),0(R5)       EXECUTED                                     
*                                                                               
*                                                                               
*                                                                               
*                                                                               
SAVEMED  DS    CL1                                                              
SAVESCRN DS    CL3                                                              
SPACES   DC    CL40' '                                                          
ZEROS    DC    40C'0'                                                           
MAXSZERR EQU   208                                                              
MEDERR   EQU   13                                                               
ACTERR   EQU   12                                                               
SCRNERR  EQU   110                                                              
SCRNERR1 EQU   111                                                              
DATERR   EQU   20                                                               
COMBERR  EQU   112                                                              
PUBERR   EQU   18                                                               
DIVERR   EQU   22                                                               
PUBERR1  EQU   114                                                              
PUBERR2  EQU   44                                                               
COPYERR  EQU   115                                                              
NXTERR   EQU   143                 'NEXT' WITH NO PREVIOUS 'LIST'               
ACCERR   EQU   96                  NOT AUTHORIZED FOR THIS FUNCTION             
CACCERR  EQU   207                 ACCESS TO THIS CLIENT NOT AUTHORIZED         
         EJECT                                                                  
GETTODAY DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,BDATE)                                     
         BR    R8                  RETURN                                       
CONDATE  GOTO1 VDATCON,DMCB,(0,WORK),(3,BDATE)                                  
         BR    R8                                                               
*                                                                               
         EJECT                                                                  
VALADV   DS    0H                                                               
*                           FORMAT IS AOR(2),ADV(3)                             
*                                                                               
*                           READ CONTROL FILE TO VALIDATE                       
         MVC   BAOFR(2),0(R6)                                                   
         CLI   2(R6),C','                                                       
         BNE   ERROR                                                            
         MVC   BADV(3),3(R6)     ADVTERISER                                     
         OC    BADV(3),=C'   '                                                  
*                                                                               
*        MUST SWITCH TO CONTROL                                                 
*                                                                               
         SR    R3,R3               PRINTPAK ERROR CODE                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'0A'               CONTROL SYSTEM                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    VALADV5                                                          
         MVC   PBLMSG,=CL60'** CONTROL SYSTEM NOT ACTIVE **'                    
         LA    R2,PBLMEDH                                                       
         NI    PBLMEDH+4,X'DF'    UNVALIDATE MEDIA                              
         B     EXIT                                                             
*                                                                               
VALADV5  LA    R7,KEY                                                           
         XC    KEY,KEY                                                          
         USING ADVREC,R7                                                        
         XC    ADVKEY,ADVKEY                                                    
         MVI   ADVREC,ADVRECQ                                                   
         MVI   ADVTYP,ADVTYPQ                                                   
         MVI   ADVSYS,C'P'                                                      
         MVC   ADVMED,BMED                                                      
         MVC   ADVAOR,BAOFR                                                     
         MVC   ADVADV,BADV                                                      
         MVC   ADVAGY,AGYALPHA                                                  
         MVC   WORK(32),KEY                                                     
         GOTO1 VDATAMGR,DMCB,(0,=CL8'DMRDHI'),=CL8'GENDIR',KEY,KEY,    X        
               (TERMNAL,0)                                                      
         CLC   KEY(32),WORK                                                     
         BE    VALADV6                                                          
         LA    R3,SCRNERR                                                       
         B     VALAERR            NOT FOUND                                     
*                                                                               
         DROP  R7                                                               
*                                                                               
VALADV6  DS    0H                                                               
         LA    R7,PUBNAMEL                                                      
         USING PUBNAMEL,R7                                                      
         LA    R3,APPLWRK                                                       
         ST    R3,ADVIO                                                         
         SR    R3,R3                                                            
         DROP  R7                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,=CL8'GETREC',=CL8'GENFIL',KEY+36,ADVIO,   X        
               DMWORK                                                           
         L     R7,ADVIO                                                         
         USING ADVKEYD,R7                                                       
         LA    R1,ADVFRST                                                       
         SR    R0,R0                                                            
VALADV8  CLI   0(R1),0                                                          
         BNE   VALADV10                                                         
         LA    R3,SCRNERR                                                       
         B     VALAERR                                                          
*                                                                               
VALADV10 CLI   0(R1),X'10'                                                      
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VALADV8                                                          
*                              SEE IF PUB LINK REQUIRED/ALLOWED                 
         CLI   AGYCNTL-AGYD(R1),C'Y'                                            
         BE    VALADVX                                                          
         CLI   AGYCNTL-AGYD(R1),C'S'  OR FOR SET-UP                             
         BE    VALADVX                                                          
         LA    R3,SCRNERR                                                       
         B     VALAERR                                                          
*                                                                               
         DROP  R7                                                               
*                                                                               
*                                                                               
VALAERR  DS    0H                                                               
VALADVX  DS    0H                                                               
*                                                                               
*        MUST SWITCH BACK                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LTR   R3,R3                 CHK FOR ERROR                              
         BNZ   ERROR                                                            
*                                                                               
         BR    R8                    RETURN                                     
*                                                                               
*                                                                               
         EJECT                                                                  
VALCLT   DS    0H                                                               
         MVC   BCLT,=3X'FF'                                                     
         CLC   0(3,R6),=C'ALL'                                                  
         BE    VALCLTX                                                          
         CLI   0(R6),C'*'           OFFICE                                      
         BNE   VALCLT40                                                         
*SMY*    CLI   2(R6),0                                                          
         CLI   3(R6),0                                                          
         BNE   ERROR          MUST BE *N OR *NN                                 
*                                  CHECK FOR ANY LIMIT ACCESS                   
*        FOLLOWING CHECK MAKES NO SENSE                                         
*        WE CAN'T LIMIT TO AN OFFICE OR CLIENT                                  
*        USING ACCESS AWARE                                                     
*                                                                               
*******  OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
*******  BNZ   VALCLT10                                                         
*                                                                               
         MVC   TSTOFF2,1(R6)                                                    
         OC    TSTOFF2,SPACES                                                   
         BAS   RE,OFCCHG           GET 1-CHR OFFICE                             
         BNE   ERROR                                                            
*                                                                               
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    VALCLT20            NOTHING TO CHECK                             
*                                                                               
VALCLT10 DS    0H                                                               
         CLI   6(RA),C'*'          CHK FOR OFFICE LIMIT                         
         BE    VALCLT14                                                         
         CLI   6(RA),C'$'          CK FOR OFFICE LIST             L02           
         BNE   CKACCE              ERROR                          L02           
VALCLT14 DS    0H                                                               
         MVI   PCLTOFF,0                                                        
         MVC   TSTOFF2,1(R6)                                      L02           
         OC    TSTOFF2,SPACES                                                   
         BAS   R5,CKACC            CHECK OFFICE FOR LIMIT ACCESS                
VALCLT20 DS    0H                                                               
         LA    R1,WORK                                                          
         USING OFFICED,R1          REPLACE ENTERED OFFICE CODE WITH             
         MVC   PCLTOFF,OFCOFC         "INTERNAL" 1-CHR OFIICE                   
         DROP  R1                       (MAY BE IDENTICAL)                      
*                                                                               
         MVI   BCLT,X'FF'                                                       
         MVC   BCLT+1(1),PCLTOFF                                                
         MVI   BCLT+2,C' '                                                      
         XC    BDIV,BDIV           MUST CLEAR BDIV FOR OFFICES                  
         B     VALCLTX                                                          
*                                                                               
VALCLT40 CLC   BCODE,=C'PAY'                                                    
         BE    ERROR         NO CLT OVERRIDES FOR PAY ADDRS                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),BMED                                                    
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),0(R6)                                                   
         OC    KEY+4(3),SPACES                                                  
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON ?                         
         BNE   ACCTSTD             NO                                           
         BRAS  RE,TRAFFACC         LOOK FOR CLIENT TRAFFIC OFFICE CODE          
         CLI   BYTE3,0             ANYTHING FOUND ?                             
         BE    ACCTSTD             NO                                           
         XC    TSTOFF2,TSTOFF2                                                  
         MVC   PCLTOFF,BYTE3       USE CLIENT TRAFFIC OFFICE CODE TO..          
ACCTSTD  DS    0H                  TEST OFFICE SECURITY (ANY ACCESS)            
         BAS   R5,CKACC            CHK LIMIT ACCESS                             
*                                                                               
         FOUT  PBLFLD1H,PCLTNAME,20                                             
         MVC   BCLT(3),KEY+4                                                    
*                                                                               
*        CLEAR OLD AOR (DUPONT STYLE)                                           
         XC    BAOFR,BAOFR                                                      
         XC    BACTL,BACTL                                                      
***                                                                             
***      OLD AOR (DUPONT STYLE) DISABLED                                        
***                                                                             
****     MVC   BAOFR,PCLTAGYR                                                   
****     MVC   BACTL,PCLTACTL                                                   
VALCLTX  BR    R8        RETURN                                                 
*                                                                               
CKACC    DS    0H                                                               
*                                                                               
         BAS   RE,PPCLIVER                                                      
         BE    CLT4                                                             
CKACCE   LA    R3,207                                           L01             
         B     ERROR                                            L01             
*                                                                               
**************                                                  L01             
CLT4     BR    R5             VALID RETURN                                      
*                                                                               
*                  R4 POINTS TO START OF DESCRIPTION                            
GETDESC  DS    0H                                                               
         CLI   BMED,C'O'        OUTDOOR                                         
         BNE   GETD1                                                            
         CLC   0(5,R4),=C'SHOW='                                                
         BNE   GETD1                                                            
         MVI   BSPACE,X'FF'                                                     
         GOTO1 =V(NUMED),DMCB,5(R4),DUB,RR=RELO                                 
         ZAP   BSHOW,DUB                                                        
         CP    BSHOW,=P'0'                                                      
         BE    GETDERR                                                          
         L     R5,DMCB                                                          
         CLI   0(R5),C' '                                                       
         BH    GETDERR                                                          
         BR    R8                                                               
*                                                                               
GETD1    MVC   BSPACE,0(R4)                                                     
         OC    BSPACE,SPACES                                                    
         BR    R8                                                               
*                                                                               
GETDERR  LA    R3,2                                                             
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
         CNOP  2,4                                                              
ACTIONS  DC    H'8'                                                             
         DC    A(ACTIONSX-1)                                                    
         DC    CL7'ADD'                                                         
         DC    X'01'                                                            
         DC    CL7'CHANGE'                                                      
         DC    X'02'                                                            
         DC    CL7'STND'                                                        
         DC    X'03'                                                            
         DC    CL7'DISPLAY'                                                     
         DC    X'04'                                                            
         DC    CL7'COPY'                                                        
         DC    X'05'                                                            
         DC    CL7'LIST'                                                        
         DC    X'06'                                                            
ACTIONSX EQU   *                                                                
         EJECT                                                                  
         CNOP  2,4                                                              
SCREENS  DC    H'8'                                                             
         DC    A(SCREENSX-1)                                                    
         DC    CL7'NAME'                                                        
         DC    X'01'                                                            
         DC    CL7'PROD'                                                        
         DC    X'02'                                                            
         DC    CL7'CIRC'                                                        
         DC    X'E3'                                                            
         DC    CL7'RATE'                                                        
         DC    X'07'                                                            
         DC    CL7'RATZ'                                                        
         DC    X'1B'                                                            
         DC    CL7'COMMENT'                                                     
         DC    X'0D'                                                            
         DC    CL7'CLE'                                                         
         DC    X'E1'                                                            
         DC    CL7'SAUR'                                                        
         DC    X'E2'                                                            
         DC    CL7'CIRCNEW'                                                     
         DC    X'E3'                                                            
         DC    CL7'RCODE'                                                       
         DC    X'E4'                                                            
         DC    CL7'TAX'                                                         
         DC    X'E5'                                                            
         DC    CL7'ADSIZE'         NON-BLEED AD SIZE                            
         DC    X'17'                                                            
         DC    CL7'BLDADS'         BLEED AD SIZE                                
         DC    X'18'                                                            
SCREENSX EQU   *                                                                
         EJECT                                                                  
*                      ACCESS BYTE X'80'=NO RATES OR PREMS OR CLES              
*                                  X'40'=NO NAME,PROD                           
*                                  X'20'=NO PAY ADDRS                           
*                                  X'10'=NO TRA ADDRS                           
*                                  X'08'=NO CON ADDRS                           
*                                  X'04'=NO CLT OR AORADV SCREENS               
*                                  X'02'=NO CIRC CHGS                           
*                                  X'01'=NO DRD CHANGES                         
*                                                                               
*                                MEDIA,ACTION,SCRN,OVERLAY,ACCESS               
COMBOS   DC    X'D501010140'     NAME SCREEN                                    
         DC    X'D502010140'                                                    
         DC    X'D503010100'                                                    
         DC    X'D504010100'                                                    
         DC    X'D501020240'     PRODUCTION                                     
         DC    X'D502020240'                                                    
         DC    X'D503020200'                                                    
         DC    X'D504020200'                                                    
         DC    X'D501060680'     PREMIUMS                                       
         DC    X'D502060680'                                                    
         DC    X'D503060600'                                                    
         DC    X'D504060600'                                                    
         DC    X'D501070780'     RATES                                          
         DC    X'D502070780'                                                    
         DC    X'D503070700'                                                    
         DC    X'D504070700'                                                    
         DC    X'D501080801'     DIV/REG/DST                                    
         DC    X'D502080801'                                                    
         DC    X'D504080800'                                                    
         DC    X'D5010A0A04'        NEW CLIENT SCREEN                           
         DC    X'D5020A0A04'                                                    
         DC    X'D5040A0A00'                                                    
         DC    X'D5010C0C38'       ADDR SCREEN                                  
         DC    X'D5020C0C38'                                                    
         DC    X'D5040C0C00'                                                    
         DC    X'D501E11180'       CLE SCREEN                                   
         DC    X'D502E11180'       CLE SCREEN                                   
         DC    X'D503E11100'       CLE SCREEN                                   
         DC    X'D504E11100'       CLE SCREEN                                   
         DC    X'D501E21280'       SAU RATE SCREEN                              
         DC    X'D502E21280'                                                    
         DC    X'D503E21200'                                                    
         DC    X'D504E21200'                                                    
         DC    X'D501E31302'       NEW CIRC                                     
         DC    X'D502E31302'       NEW CIRC                                     
         DC    X'D503E31300'       NEW CIRC                                     
         DC    X'D504E31300'       NEW CIRC                                     
         DC    X'D501E41480'       RATE CODE SCREEN                             
         DC    X'D502E41480'                                                    
         DC    X'D503E41400'                                                    
         DC    X'D504E41400'                                                    
         DC    X'D501E51580'       TAX SCREEN                                   
         DC    X'D502E51580'                                                    
         DC    X'D503E51500'                                                    
         DC    X'D504E51500'                                                    
         DC    X'D505000000'   NEWS COPY                                        
         DC    X'D506000900'   NEWS LIST                                        
         DC    X'D501161604'   AORADV                                           
         DC    X'D502161604'   AORADV                                           
         DC    X'D504161604'   AORADV                                           
************************************   NON-NEWS                                 
         DC    X'D401010140'       NON-NEWS NAME SCREEN                         
         DC    X'D402010140'                                                    
         DC    X'D403010100'                                                    
         DC    X'D404010100'                                                    
         DC    X'D401020340'       NON-NEWS PRODUCTION                          
         DC    X'D402020340'                                                    
         DC    X'D403020300'                                                    
         DC    X'D404020300'                                                    
         DC    X'D401080801'      NON-NEWS DIV/REG/DST                          
         DC    X'D402080801'                                                    
         DC    X'D404080800'                                                    
         DC    X'D4010A0A04'      NON-NEWS CLIENT                               
         DC    X'D4020A0A04'                                                    
         DC    X'D4040A0A00'                                                    
         DC    X'D4010B0B80'        MAG RATES                                   
         DC    X'D4020B0B80'                                                    
         DC    X'D4040B0B00'                                                    
         DC    X'D4010C0C38'       NON-NEWS ADDR                                
         DC    X'D4020C0C38'                                                    
         DC    X'D4040C0C00'                                                    
         DC    X'D4010D0D00'       COMMENT SCREEN                               
         DC    X'D4020D0D00'                                                    
         DC    X'D4030D0D00'                                                    
         DC    X'D4040D0D00'                                                    
         DC    X'D4010E0E80'       OUTDOOR RATES                                
         DC    X'D4020E0E80'                                                    
         DC    X'D4030E0E00'                                                    
         DC    X'D4040E0E00'                                                    
         DC    X'D4011E0E80'       OUTDOOR SECRET RATES                         
         DC    X'D4021E0E80'                                                    
         DC    X'D4031E0E00'                                                    
         DC    X'D4041E0E00'                                                    
         DC    X'D401E31302'       CIRC                                         
         DC    X'D402E31302'                                                    
         DC    X'D403E31300'                                                    
         DC    X'D404E31300'                                                    
         DC    X'D401E51580'       TAX SCREEN - SAME FOR ALL MEDIAS             
         DC    X'D402E51580'                                                    
         DC    X'D403E51500'                                                    
         DC    X'D404E51500'                                                    
         DC    X'D405000000'        MAG COPY                                    
         DC    X'D406000900'       MAG LIST                                     
         DC    X'D401161604'       AORADV                                       
         DC    X'D402161604'       AORADV                                       
         DC    X'D404161604'       AORADV                                       
         DC    X'D401171700'       AD SIZE (NON-BLEED)                          
         DC    X'D402171700'                                                    
         DC    X'D403171700'                                                    
         DC    X'D404171700'                                                    
         DC    X'D401181800'       AD SIZE (BLEED)                              
         DC    X'D402181800'                                                    
         DC    X'D403181800'                                                    
         DC    X'D404181800'                                                    
COMBOSX  EQU   *                                                                
*                                                                               
*                                                                               
****************************************************************                
*                                                                               
*                                                                               
SCRTAB2  DS    0H                                                               
         DC    CL9'NAME'                                                        
         DC    X'01'                                                            
         DC    X'03'                                                            
         DC    CL9'ADDR,CON'                                                    
         DC    X'02'                                                            
         DC    X'07'                                                            
         DC    CL9'ADDR,PAY'                                                    
         DC    X'03'                                                            
         DC    X'07'                                                            
         DC    CL9'ADDR,SHIP'                                                   
         DC    X'04'                                                            
         DC    X'08'                                                            
         DC    CL9'ADDR,TRA'                                                    
         DC    X'05'                                                            
         DC    X'07'                                                            
         DC    CL9'AORADV'                                                      
         DC    X'06'                                                            
         DC    X'05'                                                            
         DC    CL9'CIRC'                                                        
         DC    X'07'                                                            
         DC    X'03'                                                            
         DC    CL9'CLE'                                                         
         DC    X'08'                                                            
         DC    X'02'                                                            
         DC    CL9'CLIENT'                                                      
         DC    X'09'                                                            
         DC    X'05'                                                            
         DC    CL9'COMMENT'                                                     
         DC    X'0A'                                                            
         DC    X'06'                                                            
         DC    CL9'DRD,'                                                        
         DC    X'0B'                                                            
         DC    X'03'                                                            
         DC    CL9'PROD'                                                        
         DC    X'0D'                                                            
         DC    X'03'                                                            
         DC    CL9'PR,'                                                         
         DC    X'0C'                                                            
         DC    X'02'                                                            
         DC    CL9'RATE'                                                        
         DC    X'0E'                                                            
         DC    X'03'                                                            
         DC    CL9'RCODE'                                                       
         DC    X'0F'                                                            
         DC    X'04'                                                            
         DC    CL9'SAUR'                                                        
         DC    X'10'                                                            
         DC    X'03'                                                            
         DC    CL9'TAX'                                                         
         DC    X'11'                                                            
         DC    X'02'                                                            
*                                                                               
         DC    X'FF'                                                            
****************************************************************                
****************************************************************                
****************************************************************                
****************************************************************                
*                                                                               
*                                                                               
*                                                                               
*                 RESTORE FIRST 3 BYTES OF OVERLAY SCREEN - NO ERROR            
*                 IN HEADER FIELDS                                              
*                                                                               
GETOVLY  DS    0H                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),OLNUM                                                    
*                                                                  L01          
         MVC   ANATION(1),ANATION PASS ADDRESS TO CALLED PGM       L01          
*                                                                  L01          
*                                                                  L01          
         ST    RA,DMCB+4                                                        
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
         BASR  RE,RF                                                            
         TM    4(R1),X'FF'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RC,DMCB                                                          
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD              ANY ERROR                                    
         CLI   BACT,4                                                           
         BE    ACTCOMP                                                          
         CLI   BYTE3,1           SEE IF DONE                                    
         BNE   EXXMOD                                                           
*                                                                               
ACTCOMP  LA    R8,PBLLAST                                                       
         CLI   1(R8),X'20'                                                      
         BNE   UNSET                                                            
         SR    R0,R0                                                            
         IC    R0,0(R8)                                                         
         AR    R8,R0                                                            
UNSET    NI    6(R8),X'BF'         UNSET CURSOR                                 
         MVC   PBLMSG,=CL60'*** ACTION COMPLETED ***'                           
         CLI   BACT,2                                                           
         BH    GETOUT     DON'T GENERATE TURNAROUND REQ                         
         CLI   FORMAT,1                                                         
         BE    GETOUT              NO TURNAROUND ON FORMAT                      
*                                                                               
         EJECT                                                                  
DOREQ    DS    0H                                                               
         LA    R8,IOAREA+250                                                    
         USING REQRECD,R8                                                       
         XC    QHDR(26),QHDR                                                    
         MVI   QIAREA,C' '                                                      
         MVC   QIAREA+1(79),QIAREA                                              
         MVI   QHNO,X'6A'                                                       
         MVI   QHCODE,X'2E'                                                     
         MVC   QCODE(2),=C'46'                                                  
         MVC   QAGY,AGYALPHA                                                    
         MVC   QMED,PBLMED                                                      
         GOTO1 VPUBVAL,DMCB,(PBLPUBH+5,PBLPUB),(X'01',QPUB)                     
         MVC   QREQST(12),=C'ACCOUNTING  '                                      
         JIF   QMED,NE,C'O',OR,AGYALPHA,NE,=C'ZZ',DOREQ2,JUMP=N                 
         MVI   QHCODE,X'44'        68 IF ZZZ OUTDOOR                            
         MVC   QCODE(2),=C'68'                                                  
         MVC   QPUB+8(3),=C'   '       BLANK ZONE AND EDT                       
*                                                                               
DOREQ2   DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QHDR,QHDR                   
         CLI   DMCB+8,0                                                         
         BNE   DMERRS+8                                                         
         DROP  R8                                                               
*                                                                               
*                                                                               
GETOUT   LA    R2,PBLMEDH                                                       
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*               LIMIT ACCESS TESTING                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                  / **************************\                                
PPCLIVER NTR1 ***** NOTE- I/O AREA M/B IN AREC ******                           
*                  \ **************************/                                
         SPACE 2                                                                
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF                                                   
         MVC   OFCOFC2,TSTOFF2                                                  
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         MVC   OFCSECD,ASECBLK     SET A(SECRET BLOCK)                          
         DROP  R1                                                               
*                                                                               
         GOTOR VOFFICER,DMCB,(C'2',WORK),(0,ACOMFACS),0                         
         CLI   0(R1),0                                                          
         XIT1                                                                   
         SPACE 2                                                                
OFCCHG   NTR1                   **** CONVERT 2-CHAR OFFICE TO 1 *****           
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC2,TSTOFF2     TSTOFF JUST HOLDING OFFICE HERE              
         DROP  R1                                                               
*                                                                               
         GOTOR VOFFICER,DMCB,(C'2',WORK),(0,ACOMFACS),0                         
         CLI   0(R1),0                                                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PUGENEROL                                                      
         CNOP  0,4                                                              
         USING *,RF                                                             
BURECUP  NTR1                                                                   
         L     RB,BASERB                                                        
         L     R9,BASER9                                                        
         B     BURECUP1                                                         
*                                                                               
BASERB   DS    F                                                                
BASER9   DS    F                                                                
BASERD   DS    F                                                                
*                                                                               
         DROP  RF                                                               
*                                                                               
BURECUP1 DS    0H                                                               
         L     R8,0(R1)                                                         
         LA    R8,0(R8)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,25(R8)                                                      
         C     R8,4(R1)                                                         
         BH    BURECUP2                                                         
         AR    R8,R0                                                            
         C     R8,4(R1)                                                         
         BH    BURECUP4                                                         
*                                                                               
BURECUP2 L     RE,4(R1)                                                         
         ZIC   RF,1(RE)                                                         
         AR    R0,RF                                                            
         CH    R0,=H'3976'                                                      
         BNH   BURECUP4                                                         
         LA    R3,MAXSZERR                                                      
         L     RD,BASERD           SO I WON'T EXIT TO OVERLAY                   
         B     ERROR                                                            
*                                                                               
BURECUP4 GOTO1 SVRECUP,(R1)                                                     
         XIT1                                                                   
*                                                                               
SVRECUP  DS    A                                                                
*                                                                               
ADVIO    DS    A                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         LH    RE,=Y(APPLWRK-GENOLD)                                            
         AR    RE,RC                                                            
         LA    RF,1600                                                          
         XCEF                                                                   
*                                                                               
         LH    R4,=Y(APPLWRK-GENOLD)                                            
         AR    R4,RC                                                            
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                
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
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
         DROP  R4,RE                                                            
*                                                                               
CKTRIDX  DS    0H                                                               
         CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRAFFACC NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC LIMIT ACCESS            
*                                                                               
         MVI   BYTE3,0             FOR RETURN CODE                              
*                                                                               
         LA    R6,PCLTREC+33                                                    
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         BAS   RE,TRNXTEL                                                       
         BNE   TRACCX              NO CLIENT TRAFFIC OFFICE CODE ELEM           
         MVC   BYTE3,2(R6)         SAVE CLIENT TRAFFIC OFFICE CODE              
*                                                                               
TRACCX   XIT1                                                                   
*                                                                               
*                                                                               
TRNXTEL  CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                GOT TO HAVE AT LEAST ONE ELEM!               
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
TRNXTEL5 SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                FOUND ELEM, SET CC TO EQUAL                  
         CLI   0(R6),0                                                          
         BNE   TRNXTEL5                                                         
         LTR   R6,R6               NOT FOUND, SET CC TO NOT EQUAL               
         BR    RE                                                               
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DSECT FOR SCREEN TABLE #2  FOR ACCESS AWARE                                 
*                                                                               
SCRTAB2D DSECT                                                                  
SCRNAME  DS    CL9                                                              
SCRCODE  DS    CL1                                                              
SCRLENG  DS    CL1                                                              
SCRLENQ  EQU   *-SCRTAB2D                                                       
*                                                                               
*                                                                               
*** PPPUBWRK                                                                    
       ++INCLUDE PPPUBWRK                                                       
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE PPSRCHPARM                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENADVD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASECRETD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'220PPPUB00   03/14/16'                                      
         END                                                                    
