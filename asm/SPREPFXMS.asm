*          DATA SET SPREPFXMS  AT LEVEL 043 AS OF 05/01/02                      
*PHASE SPFX02GA                                                                 
*INCLUDE SORTER                                                                 
*                                                                               
***********************************************************************         
*                                                                               
*   TITLE     : MARKET-STATION RECORDS IN STATION-FILES FIX.                    
*               ALPHA-NUMERIC MARKET RECORDS.                                   
*                                                                               
*   COMMENTS  : *MAKE SPOT-STATION FILE TO V/L FILE                             
*                 -INCORPORATE LENGTH INTO S,A,M, AND R RECORDS                 
*                 -SHORTEN KEY FROM 17 TO 15 BYTES.                             
*               *CREATE PSEUDO-PASSIVE POINTERS FOR S RECORDS                   
*                 -DEFINE TO BE TYPE 'N' RECORDS                                
*                 -PUT CALL LETTERS IN KEY UNDER MARKET                         
*                 -MSPACK MARKET+CALL LETTERS TO FIT IN KEY.                    
*               *CREATE PASSIVE RECORDS FOR M RECORDS                           
*                 -DEFINE TO BE TYPE 'L' RECORDS                                
*                 -HAVE ALPHA/NUMERIC MARKET IN KEY.                            
*               *OFF-LINE PROCESS                                               
*                                                                               
*   OUTPUT    : NEW STATION FILES TO DATASET GLEE.STATION                       
*                                                                               
*   REG USAGE : R0 - WORK.                                                      
*               R1 - WORK.                                                      
*               R2 -                                                            
*               R3 -                                                            
*               R4 -                                                            
*               R5 -                                                            
*               R6 -                                                            
*               R7 -                                                            
*               R8 - 2ND BASE REGISTER.                                         
*               R9 - 2ND REG USED BY SPWORKD.                                   
*               RA - 1ST REG USED BY SPWORKD.                                   
*               RB - BASE REGISTER.                                             
*               RC -                                                            
*               RD - REGISTER D CHAIN.                                          
*               RE -                                                            
*               RF -                                                            
***********************************************************************         
         TITLE 'SPREPFXMS<===>SPFX02G - STATION-FILES FIX'                      
***********************************************************************         
*=========================== MAIN PROGRAM ============================*         
*                                                                               
SPFX02G  CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   SPFX02G                                                          
         NMOD1 0,SPFX02G,R8                                                     
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    RQF                                                              
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ MODE=REQFRST ===========================*         
RQF      OPEN  (TAPEOUT,OUTPUT)                                                 
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
         XC    CHNGCNT,CHNGCNT     CLEAR COUNTERS.                              
         XC    INVLCNT,INVLCNT                                                  
         XC    READCNT,READCNT                                                  
         XC    TYPACNT,TYPACNT                                                  
         XC    TYPLCNT,TYPLCNT                                                  
         XC    TYPMCNT,TYPMCNT                                                  
         XC    TYPNCNT,TYPNCNT                                                  
         XC    TYPRCNT,TYPRCNT                                                  
         XC    TYPSCNT,TYPSCNT                                                  
         XC    KEY,KEY             CLEAR KEY TO GET FIRST RECORD.               
*                                                                               
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     RQF20                                                            
*                                                                               
RQF10    GOTO1 SEQSTA                                                           
*                                                                               
RQF20    L     R5,ADSTAT           ADSTAT-->RECORD FOUND.                       
         L     R1,READCNT                                                       
         LA    R1,1(R1)            INCREMENT RECORDS READ.                      
         ST    R1,READCNT                                                       
*                                                                               
         MVI   FOUNDFLG,0          CLEAR WHATEVER WAS FOUND BEFORE.             
         CLI   0(R5),TYPEA         IS RECORD OF TYPE 'A'?                       
         BNE   RQF30                                                            
         L     R1,TYPACNT                                                       
         LA    R1,1(R1)            INCREMENT # OF TYPE-'A'S.                    
         ST    R1,TYPACNT                                                       
         B     RQF110                                                           
*                                                                               
RQF30    CLI   0(R5),TYPEM         IS RECORD OF TYPE 'M'?                       
         BNE   RQF32                                                            
         L     R1,TYPMCNT                                                       
         LA    R1,1(R1)            INCREMENT # OF TYPE-'M'S.                    
         ST    R1,TYPMCNT                                                       
         B     RQF100                                                           
*                                                                               
RQF32    CLI   0(R5),TYPER         IS RECORD OF TYPE 'R'?                       
         BNE   RQF34                                                            
         L     R1,TYPRCNT                                                       
         LA    R1,1(R1)            INCREMENT # OF TYPE-'R'S.                    
         ST    R1,TYPRCNT                                                       
         B     RQF110                                                           
*                                                                               
RQF34    CLI   0(R5),TYPES         IS RECORD OF TYPE 'S'?                       
         BNE   RQF36                                                            
         L     R1,TYPSCNT                                                       
         LA    R1,1(R1)            INCREMENT # OF TYPE-'S'S.                    
         ST    R1,TYPSCNT                                                       
         B     RQF105                                                           
*                                                                               
RQF36    L     R1,INVLCNT          RECORD IS NONE OF ABOVE TYPE.                
         LA    R1,1(R1)                                                         
         ST    R1,INVLCNT                                                       
         MVC   P1(117),0(R5)                                                    
         GOTO1 REPORT                                                           
*                                                                               
         CLC   LASTRECD,0(R5)      CHECK IF LAST RECORD.                        
         BE    RQF200                                                           
         B     RQF10               DO NEXT RECORD.                              
*                                                                               
*----------------------- CHANGE TO V/L RECORDS -----------------------*         
*                                                                               
RQF100   OI    FOUNDFLG,FFLGMKQ    WORKING WITH A MARKET RECORD.                
         B     *+8                                                              
RQF105   OI    FOUNDFLG,FFLGSTQ    WORKING WITH A STATION RECORD.               
RQF110   XC    WIO,WIO                                                          
         MVC   WIO(117),0(R5)      MOVE RECORD INTO WORKING IO AREA.            
         MVC   WIO+15(2),=H'144'   FORCE LENGTH INTO RECORD.                    
         TM    FOUNDFLG,FFLGMKQ    IF MARKET RECORD,                            
         BZ    *+8                                                              
         BAS   RE,CHKALIST          THEN CHECK VALIDITY OF MKTALST              
         XCEF  RECOUT,4004                                                      
         MVC   RECOUT(2),=H'148'   144+4 FOR IBM RECORDS.                       
         MVC   RECOUT+4(144),WIO                                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECOUT                                   
         L     R1,CHNGCNT          INCREMENT # OF CHANGED RECORDS.              
         LA    R1,1(R1)                                                         
         ST    R1,CHNGCNT                                                       
         L     R1,TAPECNT          INCREMENT # OF RECORDS IN TAPEOUT.           
         LA    R1,1(R1)                                                         
         ST    R1,TAPECNT                                                       
*                                                                               
         TM    FOUNDFLG,FFLGSTQ    IS THIS AN 'S' RECORD?                       
         BO    RQF150                                                           
         TM    FOUNDFLG,FFLGMKQ    IS THIS AN 'M' RECORD?                       
         BZ    RQF10               NO, DO NEXT RECORD.                          
*                                                                               
*------------------ PASSIVE RECORDS FOR M-RECORDS --------------------*         
*                                                                               
         USING MKTRECD,R5                                                       
*                                                                               
         OC    MKTALST,MKTALST     ANYTHING IN ALPHA MARKET?                    
         BZ    RQF10               IF NOT, THEN DON'T CREATE RECORD.            
*                                                                               
         XC    WIO,WIO             CLEAR AREA FIRST TO BUILD KEY.               
         LA    R4,WIO                                                           
         USING ANMRECD,R4                                                       
*                                                                               
** START BUILDING KEY                                                           
*                                                                               
         MVI   ANMKTYPE,TYPEL      THESE RECORDS WILL BE OF TYPE 'L'.           
         MVC   ANMKAGCY,MKTKAGY    AGENCY CODE (2 CHAR).                        
         MVC   ANMKMED,MKTKMED     MEDIA.                                       
         MVC   ANMKAMRK,MKTALST    ALPHA MARKET.                                
         MVC   ANMKNMRK,MKTKMKT    NUMERIC MARKET.                              
         MVC   ANMKFILL,ZEROES     FILLER.                                      
         MVC   ANMRCLEN,=H'20'     L(L-RECORD) = 20.                            
         DROP  R4,R5                                                            
*                                                                               
         XCEF  RECOUT,4004         CLEAR AREA FIRST.                            
         MVC   RECOUT(2),=H'24'    GET SET TO OUTPUT.                           
         MVC   RECOUT+4(20),WIO                                                 
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECOUT                                   
         L     R1,TYPLCNT          INCREMENT # OF CREATED 'L'-RECORDS.          
         LA    R1,1(R1)                                                         
         ST    R1,TYPLCNT                                                       
         L     R1,TAPECNT          INCREMENT # OF RECORDS IN TAPEOUT.           
         LA    R1,1(R1)                                                         
         ST    R1,TAPECNT                                                       
         B     RQF10                                                            
*                                                                               
*-------------- PSEUDO PASSIVE POINTERS FOR S-RECORDS ----------------*         
*                                                                               
RQF150   DS    0H                                                               
         USING STARECD,R5                                                       
*                                                                               
         XC    WIO,WIO             CLEAR AREA FIRST TO BUILD KEY.               
         LA    R4,WIO                                                           
         USING SFXRECD,R4                                                       
*                                                                               
** START BUILDING KEY                                                           
*                                                                               
         MVI   SFXKTYPE,TYPEN      THESE RECORDS WILL BE OF TYPE 'N'.           
         MVC   SFXKAGCY,STAKAGY    AGENCY CODE (2 CHAR).                        
         MVC   SFXKMED,STAKMED     MEDIA.                                       
         GOTO1 MSPACK,DMCB,SMKT,STAKCALL,SFXKMKCL                               
*                                  MSPACKED MKT CODE AND CALL LETTERS.          
         MVC   SFXKCLT,STAKCLT     CLIENT.                                      
         MVC   SFXKFILL,ZEROES     FILLER.                                      
         MVC   SFXRCLEN,=H'20'     L(N-RECORD) = 20.                            
         DROP  R4,R5                                                            
*                                                                               
         XCEF  RECOUT,4004         CLEAR AREA FIRST.                            
         MVC   RECOUT(2),=H'24'    GET SET TO OUTPUT.                           
         MVC   RECOUT+4(20),WIO                                                 
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECOUT                                   
         L     R1,TYPNCNT          INCREMENT # OF CREATED 'N'-RECORDS.          
         LA    R1,1(R1)                                                         
         ST    R1,TYPNCNT                                                       
         L     R1,TAPECNT          INCREMENT # OF RECORDS IN TAPEOUT.           
         LA    R1,1(R1)                                                         
         ST    R1,TAPECNT                                                       
         B     RQF10                                                            
*                                                                               
*                                                                               
*-------------------- PROCESSING END, SHOW QUOTAS --------------------*         
*                                                                               
RQF200   XCEF  RECOUT,4004                                                      
         LA    R4,RECOUT+4                                                      
         MVI   14(R4),1            DO THE FIRST RECORD                          
         MVC   15(2,R4),=H'144'                                                 
         MVI   18(R4),C' '                                                      
         MVC   19(144-18,R4),18(R4)                                             
         MVC   RECOUT(2),=H'148'                                                
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECOUT    PUT OUT 1ST RECORD             
         MVI   0(R4),X'FF'                                                      
         MVC   1(14,R4),0(R4)                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECOUT    PUT OUT LAST RECORD            
         L     R1,TAPECNT                                                       
         LA    R1,2(R1)                                                         
         ST    R1,TAPECNT                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P1(25),=C'NUMBER OF RECORDS READ = '                             
         EDIT  READCNT,(10,P1+27),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(26),=C'NUMBER OF NON-TYPE READ = '                            
         EDIT  INVLCNT,(10,P1+28),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(24),=C'NUMBER OF A-TYPE READ = '                              
         EDIT  TYPACNT,(10,P1+26),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(24),=C'NUMBER OF M-TYPE READ = '                              
         EDIT  TYPMCNT,(10,P1+26),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(24),=C'NUMBER OF R-TYPE READ = '                              
         EDIT  TYPRCNT,(10,P1+26),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(24),=C'NUMBER OF S-TYPE READ = '                              
         EDIT  TYPSCNT,(10,P1+26),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(30),=C'NUMBER OF L-RECORDS CREATED = '                        
         EDIT  TYPLCNT,(10,P1+32),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(30),=C'NUMBER OF N-RECORDS CREATED = '                        
         EDIT  TYPNCNT,(10,P1+32),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(28),=C'NUMBER OF RECORDS CHANGED = '                          
         EDIT  CHNGCNT,(10,P1+30),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P1(27),=C'NUMBER OF RECORDS PUTTED = '                           
         EDIT  TAPECNT,(10,P1+29),COMMAS=YES,ALIGN=LEFT,WRK=MYWORK,    +        
               ZERO=NOBLANK                                                     
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
*                                                                               
RQF300   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R4,15,4(R1)                                                      
         BZ    XRQF                                                             
         MVC   RECOUT(200),0(R4)   IN CASE PROGRAM DUMPS.                       
         PUT   TAPEOUT,RECOUT                                                   
         B     RQF300                                                           
*                                                                               
XRQF     GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLOSE (TAPEOUT,)                                                       
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== VALIDATE MKTALST =========================*         
CHKALIST NTR1                                                                   
*        ELIMINATE ANY NON-VALID ALPHA MARKET FROM MKTALST                      
*         AT ENTRY,                                                             
*           MKTALST  CONTAINS ALPHAMKT LIST AS IS ON FILE                       
*         AT EXIT,                                                              
*           MKTALST  CONTAINS SYNTACTICALLY CORRECT ALPHAMKTS                   
*                                                                               
         LA    R4,WIO                                                           
         USING MKTRECD,R4                                                       
*                                                                               
         OC    MKTALST,MKTALST     ANYTHING IN THE FIELD?                       
         BZ    XCHKAMKT             NOPE                                        
*                                                                               
         XC    ALIST,ALIST         "GOOD" LIST                                  
         LA    R2,ALIST                                                         
         LA    R3,MKTALST                                                       
         LA    R1,L'MKTALST/3                                                   
CAL10    BAS   RE,CHKAMKT          CHECK ALPHAMKT @ 0(R3)                       
         BNE   CAL20               CC<>0 ==> BAD ALPHAMKT                       
         MVC   0(3,R2),0(R3)       MOVE ALPHAMKT INTO GOOD LIST                 
         LA    R2,3(R2)            BUMP TO NEXT LOCATION IN GOOD LIST           
CAL20    LA    R3,3(R3)            BUMP TO NEXT ALPHAMKT ENTRY                  
         BCT   R1,CAL10                                                         
         MVC   MKTALST,ALIST       REPLACE OLD LIST WITH NEW ONE                
*                                                                               
         DROP  R4                                                               
XCHKAMKT B     EXIT                                                             
         EJECT                                                                  
*------------------------- CHECK THE ALPHAMKT ------------------------*         
CHKAMKT  NTR1                                                                   
*         AT ENTRY,                                                             
*           0(3,R3) = ALPHAMKT                                                  
*                                                                               
         LA    R1,3                                                             
CAM05    LA    R4,ALPHABET                                                      
CAM10    CLI   0(R4),C'/'          END OF VALID CHAR LIST?                      
         BE    XCHKMKT              IF YES, THEN EXIT RTNE W/. CC<>0            
         CLC   0(1,R3),0(R4)                                                    
         BE    CAM20                                                            
         LA    R4,1(R4)            MATCH AGAINST NEXT VALID CHAR                
         B     CAM10                                                            
*                                                                               
CAM20    LA    R3,1(R3)            CHECK NEXT CHAR IN ALPHAMKT                  
         BCT   R1,CAM05                                                         
*                                                                               
XCHKMKT  LTR   R1,R1               RETURN W/. CC SET                            
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*=========================== MISCELLANEOUS ===========================*         
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),                     +        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
** COUNTERS **                                                                  
CHNGCNT  DS    F                   COUNTS # OF RECORDS CHANGED.                 
INVLCNT  DS    F                   COUNTS # OF RECORDS NOT A,M,R, OR S.         
READCNT  DS    F                   COUNTS # OF RECORDS READ.                    
TAPECNT  DS    F                   COUNTS # OF PUTS ONTO TAPE.                  
TYPACNT  DS    F                   COUNTS # OF TYPE-'A' RECORDS.                
TYPLCNT  DS    F                   COUNTS # OF TYPE-'L' RECORDS.                
TYPMCNT  DS    F                   COUNTS # OF TYPE-'M' RECORDS.                
TYPNCNT  DS    F                   COUNTS # OF TYPE-'N' RECORDS.                
TYPRCNT  DS    F                   COUNTS # OF TYPE-'R' RECORDS.                
TYPSCNT  DS    F                   COUNTS # OF TYPE-'S' RECORDS.                
*                                                                               
** FLAGS **                                                                     
FOUNDFLG DS    C                   FLAGS ON WHICH RECORD IS FOUND.              
FFLGSTQ  EQU   X'80'                STATION RECORD FOUND.                       
FFLGMKQ  EQU   X'40'                MARKET  RECORD FOUND.                       
FFLGSJQ  EQU   X'10'                AGENCY = SJ.                                
*                                                                               
** RECORD TYPES **                                                              
TYPEA    EQU   C'A'                                                             
TYPEL    EQU   C'L'                ALPHA/NUMERIC PASSIVE RECORDS.               
TYPEM    EQU   C'M'                                                             
TYPEN    EQU   C'N'                FOR PSEUDO-PASSIVE POINTERS.                 
TYPER    EQU   C'R'                                                             
TYPES    EQU   C'S'                                                             
*                                                                               
** SORTER'S CARDS **                                                            
SORTCARD DC    CL80'SORT FIELDS=(5,15,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4004'                                  
*                                                                               
LASTRECD DC    (SFXKEYLQ+2)X'FF'   LAST RECORD MARKER.                          
ALPHABET DC    C' ABCDEFGHIJKLMNOPQRSTUVWXYZ/'                                  
ZEROES   DC    80C'0'                                                           
MYWORK   DS    CL17                                                             
ALIST    DS    CL(L'MKTALST)                                                    
RECOUT   DS    CL4004                                                           
WIO      DS    CL210                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== FIXED-RECORDS DSECT ========================*         
*                                                                               
       ++INCLUDE SPGENMSTA                                                      
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------*         
*                                                                               
       ++INCLUDE SPGENANMK                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= STATION-RECORDS DSECT =======================*         
*                                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= MARKET-RECORDS DSECT ========================*         
*                                                                               
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= ADDRESS-RECORDS DSECT =======================*         
*                                                                               
ADRRECD  DSECT                                                                  
       ++INCLUDE SPGENADD1                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================= REP-RECORDS DSECT =========================*         
*                                                                               
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREPT                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE SPREPMODES                                                     
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
       ++INCLUDE SPREPWORKD                                                     
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043SPREPFXMS 05/01/02'                                      
         END                                                                    
