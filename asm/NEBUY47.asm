*          DATA SET NEBUY47    AT LEVEL 017 AS OF 02/26/20                      
*                                                                               
*  FRONTRUNNER RECORD IN AIOAREA4                                               
*                                                                               
*PHASE T31147B,+0                                                               
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 017 05DEC19 <SPEC-39994> ALLOW UNITS TO USE THE COS2 FACTOR    *         
*                               FROM THE PACKAGE                      *         
***********************************************************************         
         TITLE 'NETPAK BUY PUP TRANSFER FRONTRUNNER'                            
T31147   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BY47*,RA,RR=RE                                               
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING T31147+8192,RC                                                   
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK          R7 POINTS TO LOCAL WORKING STORAGE           
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
*        LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
*        USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,R5                                                       
         LA    RE,PACKREC                                                       
         ST    RE,APACKREC                                                      
*                                                                               
*  MOVE PUP RECORD TO IOAREA5                                                   
*                                                                               
         LA    RE,IOAREA5                                                       
         LA    RF,2000                                                          
         LR    R1,RF                                                            
         L     R0,AIOAREA4                                                      
         MVCL  RE,R0               MOVE IT TO BUY VALUES                        
*                                                                               
         L     RE,AIOAREA4                                                      
         LA    RF,3000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
*  CHECK NO UNITS ON PACKAGE AND SET THE STATUS                                 
*  ON THE PACKAGE RECORD                                                        
*                                                                               
         LA    R6,IOAREA5                                                       
         USING NPURECD,R6                                                       
         TM    NPGDSTAT,X'80'      TEST FIRST PASS                              
         BZ    MA100                                                            
         DROP  R6                                                               
*                                                                               
         L     R6,APACKREC                                                      
         USING NPRECD,R6                                                        
         MVC   KEY(20),NPKTYPE                                                  
         MVI   FERN,PAKERR                                                      
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BNE   ERROR                                                            
         GOTO1 AIO,DMCB,UNT+FILE+GET+UPDATE,APACKREC                            
         MVI   FERN,FRPKGERR                                                    
         TM    NPAKCNTL,X'20'                                                   
         BZ    ERROR                                                            
         NI    NPAKCNTL,X'DF'                                                   
         GOTO1 AIO,DMCB,UNT+FILE+PUT,APACKREC                                   
         DROP  R6                                                               
*                                                                               
*  TEST FOR FROZEN CLIENT                                                       
*                                                                               
MA100    TM    CLIOPT2,X'08'                                                    
         BZ    MA300                                                            
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,CLIFRERR                                                    
         B     ERROR                                                            
         SPACE 2                                                                
*                                                                               
MA300    DS    0H                                                               
         MVC   XTRA,SPACES                                                      
*                                                                               
         BAS   RE,SETWEEKS         ADD UNIT RECORDS                             
*  SET UP RETURN GLOBBER BLOCK TO NANAV52                                       
         LA    R4,IOAREA5                                                       
*                                                                               
*  WRITE TO TEMPSTORE                                                           
***BLDR280  XC    DMCB(24),DMCB                                                 
***         L     RE,ATWA                                                       
***         MVC   DMCB+10(2),2(RE)    TERMINAL NUMBER                           
***         MVI   DMCB+8,1            PAGE NUMBER                               
***         MVI   DMCB+9,0                                                      
***         MVC   DMCB+20(2),=C'L='                                             
***         MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                         
***         MVC   WORK(4),DMCB+8       SAVE FOR GLOBBER CALL                    
***         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R4),0                   
***         CLI   8(R1),0             BLOW ON ANY ERROR HERE                    
***         BE    *+6                                                           
***         DC    H'0'                                                          
*          DATA SET NEBUY45    AT LEVEL 188 AS OF 01/29/10                      
*                                                                               
* DO WSSVR CALL PASS RECORD TO NAVIGATOR                                        
BLDR280  XC    WORK,WORK+4                                                      
         LA    R2,WORK                                                          
         USING FAWSSVRD,R2                                                      
         MVC   FAWSTOKN,=CL4'NBUY'                                              
         MVI   FAWSACTN,FAWSUSVE                                                
         MVC   FAWSADR,AIOAREA4                                                 
         MVC   FAWSLEN,=H'1000'                                                 
         PRINT GEN                                                              
         L     RF,ACOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTOR (RF),FAWSSVRD                                                    
         PRINT NOGEN                                                            
         CLI   FAWSRTN,FAWSROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(4),=CL4'NBUY'    PASS TOKEN TO NAVIGATOR                    
         DROP  R2                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,4,GLVBUY2                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* SUB-ROUTINE CALCULATE UNITS PER WEEK                                          
*                                                                               
SETWEEKS NTR1                                                                   
         LA    R6,IOAREA5                                                       
         USING NPURECD,R6                                                       
*                                                                               
*  MOVE LENGTH/UNITS/COST TO TABLE                                              
         MVC   LENS,NPUBLNS                                                     
         MVC   UNTNUM,NPUBUNS                                                   
         MVC   ACTCOST,NPUBAMT                                                  
*                                                                               
*  GET TOTAL UNITS                                                              
*                                                                               
         LA    R0,8                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R3,UNTNUM                                                        
SETW040  ICM   RF,3,0(R3)                                                       
         AR    RE,RF                                                            
         LA    R3,2(R3)                                                         
         BCT   R0,SETW040                                                       
         STCM  RE,3,UNITS                                                       
*                                                                               
*  CALCULATE NUMBER OF WEEKS/ OR DAYS                                           
*                                                                               
         PRINT GEN                                                              
         GOTO1 VDATCON,DMCB,(2,NPAKSTRT),(0,FSTDTE)                             
         GOTO1 VDATCON,DMCB,(2,NPAKEND),(0,WORK+12)                             
         PRINT NOGEN                                                            
         SR    R4,R4                                                            
         SR    R2,R2                                                            
         CLI   NPGDROT,0           TEST FOR DAILY BUYING                        
         BE    SETW160             NO                                           
*                                                                               
* UNITS PER DAYS                                                                
*                                                                               
SETW050  XC    NXTDTE,NXTDTE                                                    
         XC    CURRDAY,CURRDAY                                                  
SETW052  GOTO1 VADDAY,DMCB,FSTDTE,WORK,(R2)                                     
         CLC   WORK(6),WORK+12                                                  
         BH    SETW100                                                          
         GOTO1 VGETDAY,DMCB,WORK,FULL                                           
*                                                                               
*  TRANSLATE DAY INTO BIT FORMAT                                                
         LA    RE,DAYTAB                                                        
SETW055  CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID DAY                                  
         CLC   0(3,RE),FULL                                                     
         BE    SETW058                                                          
         LA    RE,4(RE)                                                         
         B     SETW055                                                          
SETW058  MVC   BYTE,3(RE)                                                       
*                                                                               
*  CHECK TO SEE IF DAY IS PART OF THE PROGRAMS RECORDS ROTATION                 
         OC    BYTE,NPGDROT                                                     
         CLC   BYTE,NPGDROT                                                     
         BNE   SETW070                                                          
         LA    R4,1(R4)                                                         
         OC    CURRDAY,CURRDAY                                                  
         BNZ   *+10                                                             
         MVC   CURRDAY,3(RE)       SAET FIRST DAY                               
         OC    NXTDTE,NXTDTE                                                    
         BNZ   SETW070                                                          
         GOTO1 VDATCON,DMCB,(0,WORK),(2,NXTDTE)    SET FIRST DATE               
SETW070  LA    R2,1(R2)                                                         
         B     SETW052                                                          
SETW100  STC   R4,NUMWKS                                                        
         B     SETW190                                                          
*                                                                               
DAYTAB   DC    CL3'MON',XL1'40'                                                 
         DC    CL3'TUE',XL1'20'                                                 
         DC    CL3'WED',XL1'10'                                                 
         DC    CL3'THU',XL1'08'                                                 
         DC    CL3'FRI',XL1'04'                                                 
         DC    CL3'SAT',XL1'02'                                                 
         DC    CL3'SUN',XL1'01'                                                 
         DC    XL1'FF'                                                          
*                                                                               
* UNITS PER WEEK                                                                
*                                                                               
SETW140  GOTO1 VADDAY,DMCB,FSTDTE,WORK,(R2)                                     
         CLC   WORK(6),WORK+12                                                  
         BH    SETW180                                                          
SETW160  LA    R2,7(R2)                                                         
         LA    R4,1(R4)                                                         
         B     SETW140                                                          
SETW180  STC   R4,NUMWKS                                                        
*                                                                               
*  CALCULATE UNITS PER WEEK                                                     
*                                                                               
SETW190  XC    FULL,FULL                                                        
         STC   R4,FULL+3                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,UNITS                                                       
         D     RE,FULL                                                          
         STC   RF,NUMUNAD                                                       
         STC   RE,NUMPUAD                                                       
*  ADD THE UNITS                                                                
         BAS   RE,ADDUNTS                                                       
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A UNIT RECORD FOR EACH PROGRAM ASS. RECORD               
*                                                                               
ADDUNTS  NTR1                                                                   
         LA    R6,IOAREA5                                                       
         USING NPURECD,R6                                                       
         OC    VEDIT,VEDIT                                                      
         BNZ   AU100                                                            
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB                                                       
         MVC   OVRBITS,NPUBOVRD                                                 
AU100    CLI   NPGDROT,0           CHECK FOR DAILY BUY                          
         BNE   *+10                NO/NXTDTE ALREADY SET                        
         MVC   NXTDTE,NPAKSTRT                                                  
         MVC   ENDDTE2,NPAKEND                                                  
*                                                                               
         LA    R4,BLOCK            ** R4 = UNIT BLOCK **                        
         USING UNBLOCKD,R4                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         MVC   UNALOCAL,AIOAREA2                                                
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'80'      TEST IF INTEGRATION TABLE LOOK UP            
         BNO   AU200                                                            
         DROP  RE                                                               
         LA    RF,INTGTBL                                                       
         ST    RF,UNINGTBL         PRGSW BACK INTG RATES HERE                   
         LA    RF,SVINTTBL                                                      
         ST    RF,INTHLDIT                                                      
         MVI   INTGREAD,0          DO 1ST READ                                  
         MVC   INTGAIO,AIOAREA3                                                 
         MVC   INTGSTDT,NXTDTE                                                  
         ZIC   RE,UNITS                                                         
         BCTR  RE,0                                                             
         MH    RE,=H'6'                                                         
         AR    RE,R2                                                            
         MVC   INTGEDDT,ENDDTE2                                                 
         SPACE                                                                  
AU200    XC    STEWLEN,STEWLEN                                                  
         MVC   STEWDAT,NXTDTE                                                   
         L     R3,AIOAREA1                                                      
         LR    RE,R3                                                            
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING NURECD,R3                                                        
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,AGYMED                                                     
         MVC   NUKCLT,CLIPK                                                     
         MVC   NUKDATE,NXTDTE                                                   
         MVC   NUKNET,NET                                                       
         MVC   NUKPROG,PROG                                                     
         MVC   NUKEST,EST                                                       
*                                                                               
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUKDP,NPAKDP                                                     
         DROP  RE                                                               
         SPACE                                                                  
         GOTO1 VEDIT,DMCB,(C'I',(R4))                                           
         SPACE                                                                  
         GOTO1 VGETPROG,DMCB,NUKDATE                                            
         CLI   FERN,0                                                           
         BNE   AUERR               ERROR ROUTINE                                
         SPACE                                                                  
         MVC   INTGSTDT,NUKDATE                                                 
         GOTO1 VEDIT,DMCB,(C'N',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    *+14                TEMP FIX TO BYPASS BAD PLANS***              
         MVC   FERN,UNERROR                                                     
         B     AUERR                                                            
*                                                                               
         CLI   NPGDROT,0           CHECK FOR DAILY BUY                          
         BE    *+16                NO                                           
         MVC   NUDAY,CURRDAY       RESET DAY                                    
         MVC   NUKDATE,NXTDTE      RESET DATE                                   
*                                                                               
         GOTO1 VEDIT,DMCB,(C'D',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    *+14                TEMP FIX TO BYPASS BAD PLANS***              
         MVC   FERN,UNERROR                                                     
         B     AUERR                                                            
         GOTO1 (RF),(R1),(C'F',(R4))                                            
         SPACE                                                                  
         BAS   RE,CHKRATE                                                       
*--CREATE DEFAULT TRAFFIC 21 ELEMENT                                            
         XC    ELMWORK,ELMWORK                                                  
         MVI   ELMWORK,X'21'                                                    
         MVI   ELMWORK+1,80                                                     
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,ELMWORK,0                    
         SPACE                                                                  
*--CREATE TVQ BOOK ELEMENT                                                      
         OC    NPAKTVQ,NPAKTVQ                                                  
         BZ    AU260                                                            
         GOTO1 VHELLO,DMCB2,(C'D',UNTFILE),(X'60',(R3)),(1,=C'J')               
         GOTO1 VDATCON,DMCB,(2,NPAKTVQ),(0,DUB)                                 
*                                                                               
         GOTO1 VNETWEEK,DMCB2,DUB,VGETDAY,VADDAY                                
         XC    ELMWORK,ELMWORK                                                  
         MVI   ELMWORK,X'60'                                                    
         MVI   ELMWORK+1,7                                                      
         MVI   ELMWORK+2,C'J'                                                   
         MVC   ELMWORK+3(1),4(R1)                                               
         MVC   ELMWORK+4(1),8(R1)                                               
         MVC   ELMWORK+5(2),PLNTVQBK                                            
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,ELMWORK,0                    
         SPACE                                                                  
*--CREATE SECONDARY 01 ELEMENT SET PUP CREATION INDICATOR                       
AU260    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',0(R3))                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         ICM   RE,15,12(R1)                                                     
         USING NUSDRD,RE                                                        
         OI    NUSDST4,X'08'                                                    
*--SET POSTING TYPE CONTROL BIT                                                 
         MVI   CTPOSTYP,0                                                       
***      CLI   NUPOSTYP,C'N'     NETWORK                                        
         CLI   NETTRTYP,C'N'     NETWORK                                        
         BE    AU280                                                            
         CLI   NETTRTYP,C'H'     HISPANIC                                       
         BE    AU280                                                            
***      CLI   NUPOSTYP,C'C'     CABLE                                          
         CLI   NETTRTYP,C'C'     CABLE                                          
         BNE   *+12                                                             
         OI    CTPOSTYP,X'01'                                                   
         B     AU280                                                            
***      CLI   NUPOSTYP,C'S'     SYNDICATION                                    
         CLI   NETTRTYP,C'S'     SYNDICATION                                    
         BNE   *+12                                                             
         OI    CTPOSTYP,X'02'                                                   
         B     AU280                                                            
         OI    CTPOSTYP,X'03'      OTHER                                        
         DROP  RE                                                               
         SPACE                                                                  
AU280    TM    OVRBITS,X'F0'                                                    
         BZ    AU360                                                            
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),=XL6'DD0C00000001'                                       
         TM    OVRBITS,X'80'                                                    
         BZ    AU300                                                            
         MVI   WORK+4,C'S'                                                      
         MVC   WORK+10(2),NPUBSHR                                               
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
AU300    TM    OVRBITS,X'40'                                                    
         BZ    AU320                                                            
         MVI   WORK+4,C'P'                                                      
         MVC   WORK+10(2),NPUBHUT                                               
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
AU320    TM    OVRBITS,X'20'                                                    
         BZ    AU340                                                            
         MVI   WORK+4,C'R'                                                      
         MVC   WORK+10(2),NPUBRTG                                               
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
*                                                                               
*  VPH/IMPRESSION OVERRIDES                                                     
*                                                                               
AU340    TM    OVRBITS,X'10'                                                    
         BZ    AU360                                                            
         MVI   WORK+4,C'V'                                                      
         TM    CTPOSTYP,X'01'      CHECK FOR CABLE                              
         BZ    *+8                                                              
         MVI   WORK+4,C'T'                                                      
         LA    R2,NPUNDVPH                                                      
AU350    CLI   1(R2),0                                                          
         BE    AU360                                                            
         MVC   WORK+3(1),0(R2)     NAD NUMBER                                   
         MVC   WORK+5(1),1(R2)     CATEGORY                                     
         MVC   WORK+8(4),2(R2)     VALUE                                        
*                                                                               
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
         LA    R2,6(R2)                                                         
         B     AU350                                                            
*                                                                               
*  MOVE IN SAVED PROGRAM INFORMATION                                            
*                                                                               
AU360    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'18',0(R3))                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         L     R2,12(R1)                                                        
         USING NUDTAD,R2                                                        
*  SUB PROGRAM TYPE                                                             
         OC    NPGDSTYP,NPGDSTYP                                                
         BZ    *+10                                                             
         MVC   NUDTASPT,NPGDSTYP                                                
*  PROGRAM TYPE                                                                 
         OC    NPGDTYP,NPGDTYP                                                  
         BZ    *+10                                                             
         MVC   NUDTATYP,NPGDTYP                                                 
*  TIER                                                                         
         OC    NPGDTIER,NPGDTIER                                                
         BZ    *+22                                                             
         MVI   NUDTATIR,0                                                       
         CLI   NPGDTIER,C'B'                                                    
         BE    *+10                                                             
         MVC   NUDTATIR,NPGDTIER                                                
*  NEW OR RETURNING                                                             
         OC    NPGDNEW,NPGDNEW                                                  
         BZ    *+22                                                             
         MVI   NUDTANEW,0                                                       
         CLI   NPGDNEW,C'B'                                                     
         BE    *+10                                                             
         MVC   NUDTANEW,NPGDNEW                                                 
*  CONTENT/RATING                                                               
         OC    NPGDCNT,NPGDCNT                                                  
         BZ    *+10                                                             
         MVC   NUDTARAT,NPGDCNT                                                 
*                                                                               
*  MOVE IN SAVED PROGRAM VPH'S                                                  
*                                                                               
AU370    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'33',0(R3))                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         L     R2,12(R1)                                                        
         USING NUEVD,R2                                                         
         CLI   NUEVLEN,119                                                      
         BNL   AU380                                                            
*  CREATE NEW 33 ELEMENT WITH LARGER LENGTH                                     
         XC    ELMWORK,ELMWORK                                                  
         MVC   ELMWORK(3),=XL3'337742'                                          
         MVC   ELMWORK+3(116),NPUCVPHS                                          
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'33',(R3)),0                        
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,ELMWORK,0                    
         B     AU390                                                            
AU380    MVC   NUEVPHS,NPUCVPHS                                                 
*                                                                               
***AU390 TM    OVRBITS,X'40'                                                    
****     BNZ   AU400                                                            
****     CLI   CALCHUT,C'N'                                                     
****     BE    AU400                                                            
AU390    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'35',0(R3))                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         L     R2,12(R1)                                                        
****     ICM   RE,3,OVHUT                                                       
****     SRDA  RE,32                                                            
****     M     RE,=F'10'                                                        
****     ST    RF,WORK                                                          
****     MVC   5(2,R2),WORK+2                                                   
         MVC   5(2,R2),NPUBHUT                                                  
         ICM   RE,3,5(R2)          SCALE THE HUT TO TWO DECIMAL PLACES          
         MH    RE,=H'10'           FOR THE DEMO ELEMENT                         
         STCM  RE,3,5(R2)                                                       
         MVC   3(2,R2),NPUBRTG                                                  
         MVC   7(2,R2),NPUBSHR                                                  
***      OC    OVSHR,OVSHR                                                      
***      BNZ   AU400                                                            
***      BAS   RE,CALCSHAR                                                      
***      MVC   7(2,R2),WORK                                                     
***      SPACE                                                                  
***AU400 BAS   RE,CALSHRAT         CALC SHARE OR RATING OVERRIDE                
*                                                                               
         OC    NPAKGUA,NPAKGUA     NEW PACKAGE GUARANTEE                        
         BZ    AU405                                                            
         XC    WORK(8),WORK                                                     
         MVC   WORK(2),=XL2'B308'                                               
         MVC   WORK+2(4),NPAKGUA                                                
         BAS   RE,PUTEL                                                         
AU405    OC    NPAKDGUA,NPAKDGUA                                                
         BZ    AU410                                                            
         OC    NPAKDCAT,NPAKDCAT                                                
         BZ    AU410                                                            
         XC    WORK(11),WORK                                                    
         MVC   WORK(2),=XL2'B40B'                                               
         MVC   WORK+2(3),NPAKDCAT                                               
         MVC   WORK+5(4),NPAKDGUA                                               
         BAS   RE,PUTEL                                                         
         L     R3,AIOAREA1                                                      
*                                                                               
* MOVE IN SUB DAYPART OVERRIDE                                                  
*                                                                               
AU410    OC    NPGDSDPT,NPGDSDPT                                                
         BZ    AU418                                                            
* REMOVE CURRENT SUB DAYPART                                                    
         GOTO1 VHELLO,DMCB2,(C'D',UNTFILE),(X'60',(R3)),(1,=C'U')               
*                                                                               
         GOTO1 VALDAYPT,DMCB,(0,NPGDSDPT)                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,X'60'                                                       
         MVI   WORK+2,C'U'                                                      
         MVC   WORK+3(2),KEY+21     DAYPART CODE                                
         MVC   WORK+5(14),KEY+6     DAYPART LITERAL                             
*                                                                               
* GET DESCRIPTION LENGTH                                                        
         LA    RE,14                                                            
*                                                                               
* START AT END OF DESCRIPTION FIND FIRST NON BLANK CHARACTER                    
*                                                                               
PRGN50   LA    RF,WORK+5                                                        
         AR    RF,RE                                                            
         BCTR  RF,0                 ACCOUNT FOR ZERO OFFSET                     
         CLI   0(RF),X'40'                                                      
         BH    PRGN52                                                           
         BCT   RE,PRGN50                                                        
PRGN52   LA    RE,5(RE)             FOR THE REST OF THE ELEMENT                 
         STCM  RE,1,WORK+1                                                      
         BAS   RE,PUTEL                                                         
*                                                                               
AU418    BAS   RE,GETSUB           ASSIGN SUB-LINE                              
         OI    NUUNST2,X'02'                                                    
         ZIC   R2,NUMUNAD                                                       
         SR    RE,RE                                                            
         ICM   RE,1,NUMPUAD                                                     
         BZ    AU420                                                            
         LA    R2,1(R2)                                                         
         BCTR  RE,0                                                             
         STC   RE,NUMPUAD                                                       
*                                                                               
* LOOP TO ADD RECORDS UPDATEING LINE # ONLY                                     
AU420    STC   R2,LOOP1                                                         
*                                                                               
         LA    R0,8                FIND LENGTH                                  
         LA    RF,LENS                                                          
         LA    R1,UNTNUM                                                        
         LA    R2,ACTCOST                                                       
         SR    RE,RE                                                            
AU422    ICM   RE,3,0(R1)                                                       
         BNZ   AU423                                                            
         LA    R1,2(R1)                                                         
         LA    R2,5(R2)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,AU422                                                         
         DC    H'0'                                                             
AU423    BCTR  RE,0                                                             
         STCM  RE,3,0(R1)                                                       
         ZIC   RE,0(RF)                                                         
         STC   RE,NULEN                                                         
         MVC   STEWLEN,NULEN                                                    
         MVC   NUACTUAL,0(R2)                                                   
         NI    NUUNITST,X'DF'      RESET ACTUAL COST INPUTTED                   
         TM    4(R2),X'80'         CHECK IF COST INPUTTED                       
         BZ    AU424                                                            
         BAS   RE,CHKCOS2          CHECK COST2 SETTING                          
         PRINT GEN                                                              
         GOTO1 VCALCASH,DMCB,AIOAREA1,ACOMFACS,(X'01',CLICPRD)                  
         PRINT NOGEN                                                            
         OI    NUUNITST,X'20'      ACTUAL COST INPUTTED                         
AU424    OC    NURSTAT,CTPOSTYP    SET STATUS BIT                               
*                                                                               
         CLC   NUALPHA,=C'SJ'      SJR?                                         
         BE    AU425                                                            
         CLC   NUALPHA,=C'H9'      STARCOM?                                     
         BNE   AU430                                                            
*                                                                               
AU425    LA    RE,NEBLOCKA                                                      
         USING NEBLOCKD,RE                                                      
         CLI   NBSTATYP,C'S'       SYNDICATION?                                 
         BNE   AU430                                                            
         GOTO1 =A(ADDMCST),RR=MYRELO                                            
         DROP  RE                                                               
*                                                                               
AU430    DS    0H                                                               
         CLI   NUKSUB,193                                                       
         BL    *+12                                                             
******   BAS   RE,SRTPTRS          ADD REMAINING PASSIVES                       
         MVI   FERN,SUBERR                                                      
         B     AUERR                                                            
         GOTO1 AIO,DMCB,UNT+FILE+ADDREC,(R3)                                    
         MVC   UNITDA,NDXDA        SAVE RETURNED DISK ADDRESS                   
         GOTO1 VEDIT,DMCB,(C'P',(R4)),WORK                                      
         LA    R2,WORK                                                          
         LA    R1,NDIRPTRS         N'PRGSWIVE POINTERS                          
         L     RE,NPTRS            N'POINTERS IN TABLE                          
         LR    RF,RE                                                            
         AR    RF,R1               UPDATE POINTER COUNT                         
         ST    RF,NPTRS                                                         
         MH    RE,=Y(NDIRLEN)      INDEX INTO POINTER TABLE                     
         L     R0,AIOAREA4         POINT TO NEXT ENTRY POSITION                 
         AR    RE,R0                                                            
         SPACE                                                                  
AU440    MVC   0(NDIRLEN,RE),0(R2) EXTRACT PRGSWIVE POINTER                     
         MVI   NDIRCTL(RE),0       ZERO CONTROL BYTE                            
         LA    RE,NDIRLEN(RE)      NEXT TABLE POSITION                          
         LA    R2,NDIRLEN(R2)      NEXT POINTER                                 
         BCT   R1,AU440                                                         
         CH    RF,=H'120'          TEST FOR TABLE OVERFLOW                      
         BL    *+8                                                              
         BAS   RE,SRTPTRS                                                       
         SPACE                                                                  
         CLI   NUKSUB,2                                                         
         BNE   *+8                                                              
         BAS   RE,SUBPRT                                                        
*                                                                               
         ZIC   R1,NUKSUB           SAME DATE SO INCREMENT THE                   
         LA    R1,1(R1)            LAST SUB-LINE FOR DATE                       
         CH    R1,=H'193'                                                       
         BNH   AU490                                                            
         MVI   FERN,SUBERR                                                      
         B     AUERR                                                            
AU490    STC   R1,NUKSUB                                                        
         ZIC   R2,LOOP1                                                         
         BCT   R2,AU420                                                         
*                                                                               
AU500    GOTO1 VDATCON,DMCB,(2,NUKDATE),(0,FSTDTE)                              
         LA    R2,7                                                             
         CLI   NPGDROT,0           CHECK FOR DAILY BUY                          
         BNE   AU560               YES                                          
         GOTO1 VADDAY,DMCB,FSTDTE,FSTDTE,(R2)                                   
         GOTO1 VDATCON,DMCB,(0,FSTDTE),(2,NXTDTE)                               
AU520    OC    UNTNUM,UNTNUM                                                    
         BNZ   AU200                                                            
         SPACE                                                                  
         BAS   RE,SRTPTRS                                                       
         B     EXXMOD                                                           
         SPACE 2                                                                
* UNITS PER DAYS                                                                
*                                                                               
AU560    LA    R2,1                                                             
         GOTO1 VADDAY,DMCB,FSTDTE,FSTDTE,(R2)                                   
         GOTO1 VGETDAY,DMCB,FSTDTE,FULL                                         
*                                                                               
*  TRANSLATE DAY INTO BIT FORMAT                                                
         LA    RE,DAYTAB                                                        
AU570    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID DAY                                  
         CLC   0(3,RE),FULL                                                     
         BE    AU580                                                            
         LA    RE,4(RE)                                                         
         B     AU570                                                            
AU580    MVC   BYTE,3(RE)                                                       
*                                                                               
*  CHECK TO SEE IF DAY IS PART OF THE PROGRAMS RECORDS ROTATION                 
         OC    BYTE,NPGDROT                                                     
         CLC   BYTE,NPGDROT                                                     
         BNE   AU560                                                            
*                                                                               
         MVC   CURRDAY,3(RE)                                                    
         GOTO1 VDATCON,DMCB,(0,FSTDTE),(2,NXTDTE)                               
AU600    OC    UNTNUM,UNTNUM                                                    
         BNZ   AU200                                                            
         SPACE                                                                  
         BAS   RE,SRTPTRS                                                       
         B     EXXMOD                                                           
*                                                                               
SRTPTRS  NTR1                                                                   
         L     R2,NPTRS            SORT POINTERS IN DESCENDING SEQUENCE         
         LTR   R2,R2               0 IF POINTERS JUST = 80                      
         BZ    SP100                                                            
         L     R3,AIOAREA4         R3=POINTS TO PRGSWIVE POINTER TABLE          
         GOTO1 VXSORT,DMCB,(X'FF',0(R3)),(R2),NDIRLEN,L'NUKEY,0                 
         BAS   RE,NEWPTR           ADD NEW POINTER                              
         LA    R3,NDIRLEN(R3)      NEXT POINTER                                 
         BCT   R2,*-8                                                           
SP100    XC    NPTRS,NPTRS                                                      
         L     RE,AIOAREA4                                                      
         LA    RF,3000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         B     EXXMOD                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE SHARE OR RATING VALUE                                
*                                                                               
CALSHRAT NTR1                                                                   
         TM    OVRBITS,X'A0'       IS BOTH SHARE AND RATING OVERRIDDEN          
         BO    EXXMOD                                                           
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),=XL6'DD0C00000001'                                       
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'35',0(R3))                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         L     RF,12(R1)                                                        
         SR    R2,R2                                                            
         ICM   R2,3,5(RF)          SHARE = RATING / HUT                         
*                                                                               
         TM    OVRBITS,X'40'       IS HUT OVERRIDDEN                            
         BZ    CALSR020                                                         
         SR    RF,RF                                                            
         ICM   RF,3,OVHUT          MULT OVERRIDDEN HUT VALUE BY 10              
         M     RE,=F'10'                                                        
         LR    R2,RF                                                            
CALSR020 TM    OVRBITS,X'80'       IS SHARE OVERRIDDEN                          
         BO    CALSR050                                                         
         TM    OVRBITS,X'20'       IS RATING OVERRIDDEN                         
         BZ    EXXMOD                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,OVRTG          SHARE = RATING / HUT                         
         M     R0,=F'100000'       SCALE THE DIVIDEND                           
         LTR   RE,R2               GET HUT VALUE                                
         BZ    EXXMOD              ZERO DIVISOR                                 
         DR    R0,RE               COMPUTE SHARE TO 1 DECIMAL PLACE             
         AH    R1,=H'5'            ROUND BACK UP TO 1                           
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MVI   WORK+4,C'S'                                                      
         STCM  R1,3,WORK+10                                                     
         B     CALSR100                                                         
         SPACE 1                                                                
CALSR050 SR    R0,R0               RATING = SHARE X HUT                         
         ICM   R0,3,OVSHR                                                       
         LR    R1,R2               GET HUT VALE                                 
         MR    R0,R0                                                            
         AH    R1,=H'5000'         ROUND TO ONE DECIMAL PLACE                   
         D     R0,=F'10000'                                                     
         MVI   WORK+4,C'R'                                                      
         STCM  R1,3,WORK+10                                                     
*                                                                               
CALSR100 BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
         B     EXXMOD                                                           
*                                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE SHARE                                                
*                                                                               
CALCSHAR NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,3,OVRTG          SHARE = RATING / HUT                         
         M     R0,=F'100000'       SCALE THE DIVIDEND                           
         OC    OVHUT,OVHUT                                                      
         BZ    EXXMOD              ZERO DIVISOR                                 
         SR    RE,RE                                                            
         ICM   RE,3,OVHUT          GET HUT VALUE                                
         DR    R0,RE               COMPUTE SHARE TO 1 DECIMAL PLACE             
         AH    R1,=H'50'            ROUND BACK UP TO 1                          
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         STCM  R1,3,WORK                                                        
         B     EXXMOD                                                           
         SPACE 2                                                                
*                                                                               
* RATE TYPE VALIDATOR                                                           
*                                                                               
CHKRATE NTR1                                                                    
         XC    HALF,HALF                                                        
*--CHECK ESTIMATE RECORD                                                        
CHK20    GOTO1 FLDCHK,DMCB2,RTRECTAB,ESTRATE,HALF      CHECK RATE               
         CLI   ESTRATEC,X'40'                                                   
         BL    *+10                                                             
         MVC   HALF+1(1),ESTRATEC                                               
         CLI   HALF,0                                                           
         BNE   CHK200                                                           
*--CHECK CLIENT RECORD                                                          
CHK40    MVC   BYTE,CLIPRO+14                                                   
         GOTO1 FLDCHK,DMCB2,RTRECTAB,BYTE,HALF         CHECK RATE               
         CLI   CLIEXTRA+14,X'40'                                                
         BL    *+10                                                             
         MVC   HALF+1(1),CLIEXTRA+14                                            
         CLI   HALF,0                                                           
         BE    EXXMOD                                                           
*                                                                               
CHK200   GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',0(R3))                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         ICM   RE,15,12(R1)                                                     
         USING NUSDRD,RE                                                        
*                                                                               
         MVC   NUSDSRT,HALF        MOVE IN RATE                                 
         MVI   NUSDRTCV,0                                                       
         CLI   HALF+1,C'A'         DONT MOVE DEFAULT VALUE OUT                  
         BE    *+10                                                             
         MVC   NUSDRTCV,HALF+1     MOVE IN COVERAGE                             
         B     EXXMOD                                                           
         DROP  RE                                                               
*                                                                               
*****   INPUT          P1=A(TABLE)                                              
*                      P2=RATE                                                  
*       OUTPUT         P3=1 BYTE OUTPUT SET TO ZERO NOT FOUND                   
FLDCHK   NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
FLDCH50  CLI   0(R2),C' '                                                       
         BE    FLDCH100                                                         
         CLC   0(1,R2),0(R3)                                                    
         BE    FLDCH70                                                          
         LA    R2,2(R2)                                                         
         B     FLDCH50                                                          
FLDCH70  MVC   0(1,R4),1(R2)                                                    
         B     FLDCHEX                                                          
FLDCH100 MVI   0(R4),0                                                          
FLDCHEX  B     EXXMOD                                                           
*                                                                               
RTRECTAB DC    CL9'2F8C9WYY '                                                   
CVFLDTAB DC    CL7'AAIITT '                                                     
         EJECT                                                                  
*                                                                               
* IF SECOND COST OPTION INPUTTED CALCULATE THE                                  
* SECOND COST AND MOVE INTO THE ASSIGNED COST FIELD                             
*                                                                               
CHKCOS2  NTR1                                                                   
         MVC   FULL,CLICOST2       MOVE CLIENT PCT.                             
         OC    ESTCOST2,ESTCOST2   WAS ESTIMATE LEVEL COST INPUTTED             
         BZ    *+10                                                             
         MVC   FULL,ESTCOST2                                                    
         OC    PKGCOST2,PKGCOST2   WAS PACKAGE LEVEL COST INPUTTED              
         BZ    *+10                                                             
         MVC   FULL,PKGCOST2                                                    
         OC    FULL,FULL           WAS ANY COST PCT INPUTTED                    
         BZ    CHCS2EX             NO, EXIT                                     
*                                                                               
         NI    NUUNITST,X'F7'                                                   
         XC    NUASSIGN,NUASSIGN                                                
         TM    FULL,X'80'           WAS COST PCT SET TO ZERO                    
         BZ    CHCS250              NO CALCULATE                                
         OI    NUUNITST,X'08'                                                   
         B     CHCS2EX                                                          
*                                                                               
CHCS250  ZAP   WORK(16),=PL1'0'                                                 
         ICM   R1,15,NUACTUAL       COST                                        
         CVD   R1,WORK+8                                                        
         ICM   R1,15,FULL           PERCENTAGE X.XXXXXX                         
         CVD   R1,DUB                                                           
         MP    WORK(16),DUB+4(4)    MULT COST BY PERCENTAGE                     
         AP    WORK(16),=PL4'500000'  ROUND                                     
         DP    WORK(16),=PL4'1000000'                                           
         CVB   R1,WORK+4                                                        
         STCM  R1,15,NUASSIGN                                                   
         OC    NUASSIGN,NUASSIGN                                                
         BNZ   CHCS2EX                                                          
         OI    NUUNITST,X'08'                                                   
CHCS2EX  B     EXXMOD                                                           
         SPACE 2                                                                
*                                                                               
* ERROR EXIT                                                                    
*                                                                               
AUERR    BAS   RE,SRTPTRS          ADD REMAINING PASSIVES                       
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET NEXT SUB-LINE NUMBER                                       
*                                                                               
GETSUB   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         L     R1,AIOAREA1                                                      
         MVI   NUKPTYPE,X'84'      USE PASSIVE KEY TO FIND NEXT NUMBER          
         MVC   NUKPAM,NUKAM-NUKEY(R1)                                           
         MVC   NUKPCLT,NUKCLT-NUKEY(R1)                                         
         MVC   NUKPNET,NUKNET-NUKEY(R1)                                         
         MVC   NUKPPROG,NUKPROG-NUKEY(R1)                                       
         MVC   NUKPDATE,NUKDATE-NUKEY(R1)                                       
         MVC   NUKPEST,NUKEST-NUKEY(R1)                                         
         LA    R0,UPDATE+PASSDEL+UNT+DIR+HIGH                                   
         SPACE                                                                  
GETSUB2  GOTO1 AIO,DMCB,(R0)                                                    
         CLC   KEY(NUKPSUB-NUKPKEY),KEYSAVE                                     
         BNE   GETSUB4                                                          
         LA    R0,UPDATE+PASSDEL+UNT+DIR+SEQ                                    
         B     GETSUB2                                                          
         SPACE                                                                  
GETSUB4  LA    R4,KEYSAVE                                                       
         ZIC   R1,NUKPSUB                                                       
         LA    R1,1(R1)                                                         
         L     R4,AIOAREA1         POINT BACK TO RECORD                         
         USING NURECD,R4                                                        
         STC   R1,NUKSUB                                                        
         B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR NEW POINTERS (AT ENTRY, R3 ADDRESSES POINTER)                 
*                                                                               
NEWPTR   ST    RE,SAVEREG                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R3)  READ FOR NEW POINTER                         
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE TEST IF KEY FOUND                           
         BE    NEWPTR2                                                          
         MVC   KEY(NDIRLEN),0(R3)  RESET ENTIRE POINTER                         
         OC    KEY+20(1),CTPOSTYP    SET STATUS BIT                             
         GOTO1 (RF),(R1),UNT+DIR+ADD                                            
         B     NEWPTRX                                                          
         SPACE                                                                  
NEWPTR2  MVC   KEY(NDIRLEN),0(R3)                                               
         OC    KEY+20(1),CTPOSTYP    SET STATUS BIT                             
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         SPACE                                                                  
NEWPTRX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO UPDATE FIRST UNIT FOR DATE WHEN SECOND UNIT IS ADDED           
*                                                                               
SUBPRT   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,AGYMED                                                    
         MVC   NUKPCLT,CLIPK                                                    
         MVC   NUKPNET,NET                                                      
         MVC   NUKPPROG,PROG                                                    
         MVC   NUKPDATE,NXTDTE                                                  
         MVC   NUKPEST,EST                                                      
         MVI   NUKPSUB,1            SUB-LINE 1                                  
         L     R1,AIOAREA1                                                      
         MVC   NUKPDP,NUKDP-NUKEY(R1) EXTRACT DAYPART                           
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(NUKPDP-NUKPKEY+1),KEYSAVE                                    
         BNE   SUBPRTX             COULD NOT FIND IT-DELETED                    
         SPACE                                                                  
SUBPRT2  L     R4,AIOAREA2         GET THE RECORD                               
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,(R4)                                
         USING NURECD,R4                                                        
*        CLI   NPROGS,1                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   NUSUBPRT,1                                                       
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         SPACE                                                                  
SUBPRTX  B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO ADD THE PRECISION FACTOR TO OVERRIDE ELEMENTS                  
* R4 POINTS TO THE ELEMENT AREA                                                 
*                                                                               
SETPRE   NTR1                                                                   
         LA    R4,WORK             ELEMENT AREA                                 
         LA    R5,DEMPREC          CONVERSION TABLE                             
         LA    RE,7                                                             
*                                                                               
SETP20   CLC   0(1,R5),4(R4)                                                    
         BE    SETP40                                                           
         LA    R5,2(R5)                                                         
         BCT   RE,SETP20                                                        
         DC    H'0'                                                             
SETP40   MVC   7(1,R4),1(R5)                                                    
         B     EXXMOD                                                           
         EJECT                                                                  
PUTEL    LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
*                                                                               
QTBL     DC    0CL8' '                                                          
QTBL1    DC    XL1'01',XL1'03'                                                  
QTBL2    DC    XL1'04',XL1'06'                                                  
QTBL3    DC    XL1'07',XL1'09'                                                  
QTBL4    DC    XL1'09',XL1'0C'                                                  
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
* MODULE AND SUB-ROUTINE EXIT                                                   
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
*                                                                               
ADDMCST  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'D',APUNTFIL),(X'15',(R3)),0                      
*                                                                               
         XC    ELMWORK,ELMWORK                                                  
*&&DO                                                                           
         LA    RF,ELMWORK                                                       
         USING NUMCSTD,RF                                                       
*                                                                               
         MVI   NUMCSEL,X'15'                                                    
         MVI   NUMCSLEN,NUMCSLNQ                                                
         MVC   NUMCMCS,NUACTUAL                                                 
         DROP  RF                                                               
*&&                                                                             
*                                                                               
         MVI   ELMWORK,X'15'                                                    
         MVI   ELMWORK+1,X'0B'                                                  
         MVC   ELMWORK+2(4),NUACTUAL                                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',APUNTFIL),AIOAREA1,ELMWORK,0                   
*                                                                               
ADDMCSTX DS    0H                                                               
         XMOD1 1                                                                
         DROP  R3                                                               
*                                                                               
APUNTFIL DC    CL8'UNTFILE'                                                     
* EXECUTED INSTRUCTIONS                                                         
*                                                                               
YESCOMP  CLC   FLD(0),=C'YES'                                                   
NOCOMP   CLC   FLD(0),=C'NO'                                                    
*                                                                               
PRINCOMP CLC   FLD(0),=C'PRINT'                                                 
ZONECOMP CLC   FLD(0),=C'ZONE'                                                  
         SPACE 2                                                                
* TABLE OF DAYPART CODES AND NAMES                                              
*&&DO                                                                           
DPTTAB   DS    0CL9                                                             
         DC    C'D',CL8'DAYTIME'                                                
         DC    C'B',CL8'CBLSPORT'                                               
         DC    C'U',CL8'UNWIRED'                                                
         DC    C'F',CL8'FRINGE'                                                 
         DC    C'P',CL8'PRIME'                                                  
         DC    C'K',CL8'KIDS'                                                   
         DC    C'T',CL8'TEENS'                                                  
         DC    C'Y',CL8'YOUTH'                                                  
         DC    C'S',CL8'SPORTS'                                                 
         DC    C'N',CL8'NEWS'                                                   
         DC    C'E',CL8'EARLY'                                                  
         DC    C'L',CL8'LATE'                                                   
         DC    C'C',CL8'CABLE'                                                  
         DC    C'O',CL8'OLYMPICS'                                               
         DC    C'R',CL8'RADIO'                                                  
         DC    C'X',CL8'SYND.'                                                  
         DC    C'X',CL8'X'                                                      
         DC    C'I',CL8'SPECIAL'                                                
         DC    C'I',CL8'I'                                                      
         DC    C'V',CL8'OVERNITE'                                               
         DC    C'V',CL8'V'                                                      
         DC    C'W',CL8'WKNDPM'                                                 
         DC    C'M',CL8'WKNDAM'                                                 
         DC    C'M',CL8'M'                                                      
         DC    C'A',CL8'ACCESS'                                                 
DAYPARTS EQU   (*-DPTTAB)/L'DPTTAB                                              
*&&                                                                             
         SPACE 2                                                                
* TABLE OF STATUS ACTIONS (COVERED BY STATABD)                                  
*              BYTES 0-7 = STATUS KEYWORD                                       
*              BYTE  8   = MINIMUM COMPARE LENGTH                               
*              BYTE  9   = OR MASK FOR STATUS                                   
*              BYTE 10   = AND MASK FOR STATUS                                  
*              BYTE 11   = STATUS INDICATORS                                    
*                                                                               
STATAB   DS    0CL(STATABL)                                                     
         DC    CL8'LOCKED',AL1(1),AL1(LOCKED)                                   
         DC    X'FF',AL1(UNITFIX)                                               
*                                                                               
         DC    CL8'UNLOCKED',AL1(3),X'00'                                       
         DC    AL1(UNLOCKED),AL1(UNITFIX)                                       
*                                                                               
         DC    CL8'FROZEN',AL1(1),AL1(FROZEN)                                   
         DC    X'FF',X'00'                                                      
*                                                                               
         DC    CL8'UNFROZEN',AL1(3),X'00'                                       
         DC    AL1(UNFROZEN),X'00'                                              
*                                                                               
STAENT   EQU   (*-STATAB)/L'STATAB                                              
*                                                                               
MAXPTRS  EQU   80                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* PPU TRANSFER SCREEN - IT SMALLER SO IT GOES 1ST                               
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYF3D                                                       
         EJECT                                                                  
* PACKAGE SCREEN                                                                
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYFDD                                                       
         ORG   TWAD+PAGELEN                                                     
SVAREA18 DS    0CL256                                                           
SVUAKEY  DS    CL20                SAVED UNIT ASS. KEY                          
SVACTLIN DS    CL61                LAST DPT,PLAN,DATES,LEN                      
PLAN     DS    CL4                 PLAN CODE                                    
PLNDPT   DS    CL1                 PLAN DAYPART                                 
PLNTVQBK DS    CL2                 PLAN TVQ BOOK                                
STRTDTE  DS    XL6                 START DATE                                   
ENDDTE   DS    XL6                 END DATE                                     
TRGDEMO  DS    XL1                 TARGET DEMO                                  
LENS     DS    XL8                 LENGTHS FROM PUP RECORD                      
SVLEN1   DS    XL1                 FILTER LENGTHS 1                             
SVLEN2   DS    XL1                 FILTER LENGTHS 2                             
SVQTBLS  DS    0XL4                WHICH QUARTERS ARE IN USE                    
SVQTBL1  DS    XL1                 QUARTERS 1                                   
SVQTBL2  DS    XL1                 QUARTERS 2                                   
SVQTBL3  DS    XL1                 QUARTERS 3                                   
SVQTBL4  DS    XL1                 QUARTERS 4                                   
PLANYR   DS    XL1                 CURRENT PLAN YEAR                            
PLHAVE   DS    XL1                 PLANS HUT AVERAGE                            
SVACTION DS    XL1                 LAST ACTION #                                
SVSTDTE  DS    XL3                 DATE TO START ADDING UNITS                   
SVENDTE  DS    XL3                 LAST DATE TO END ADDDING UNITS               
PACKGU   DS    XL2                 PACKAGE GUARENTEE FACTOR                     
NPACKGU  DS    XL4                 NEW PACKAGE GUARENTEE FACTOR                 
NDEMOGU  DS    XL4                 NEW DEMO GUARENTEE FACTOR                    
NDEMOCAT DS    XL3                 NEW DEMO CATEGORY FOR GUARENTEE              
DEMOGU   DS    XL2                 DEMO GUARENTEE FACTOR                        
DEMOCAT  DS    XL1                 DEMO CATEGORY FOR GUARENTEE                  
CALCHUT  DS    XL1                 IF C'Y' RECALCULATE PUP HUT                  
PLNDEMS  DS    XL18                                                             
PLNUPLST DS    XL1                                                              
IOAREA5  DS    2000X               READ IN ASS. RECORD                          
         ORG                                                                    
         EJECT                                                                  
* LOCAL WORKING STORAGE                                                         
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
VEDIT    DS    A                                                                
UNITDA   DS    A                                                                
NPTRS    DS    F                   NUMBER OF POINTERS                           
PUNITS   DS    F                   PROGRAM UNITS                                
UNITTAB  DS    F                   UNIT TABLE POINTER                           
LENTAB   DS    F                   LENGTH TABLE POINTER                         
COSTTAB  DS    F                   COST TABLE POINTER                           
*                                                                               
STATSW   DS    C                   Y=UPDATE STATUS FIELD ON UNITS               
ORMASK   DS    X                                                                
ANDMASK  DS    X                                                                
OLDSTAT  DS    X                                                                
NEWSTAT  DS    X                                                                
*                                                                               
         DS    0F                                                               
BLOCK    DS    CL150                                                            
ELMWORK  DS    CL130                                                            
OVRBITS  DS    XL1                 BITS FOR OVERRIDES                           
OVSHR    DS    XL2                 SHARE FROM ASSIGNMENT RECORD                 
OVHUT    DS    XL2                 HUT FROM ASSIGNMENT RECORD                   
OVRTG    DS    XL2                 RATING FROM ASSIGNMENT RECORD                
OVVPH    DS    XL2                 VPH FROM ASSIGNMENT RECORD                   
UNTNUM   DS    XL16                UNITS/LENGTH ASS. RECORD                     
UNITS    DS    XL2                 TOTAL UNITS                                  
ACTCOST  DS    XL40                ACT CST/STA BIT FOR 1-8 LENGTHS              
COUNT    DS    XL1                 TEMPORARY # WEEKS COUNTER                    
FSTDTE   DS    XL6                 FIRST DATE OF PROGRAM                        
NXTDTE   DS    XL2                 NEXT UNIT DATE - COMPRESSED                  
ENDDTE2  DS    XL2                 END DATE - COMPRESSED                        
NUMWKS   DS    XL1                 NUMBER OF WEEKS IN PERIOD                    
NUMUNAD  DS    XL1                 NUMBER OF UNITS                              
NUMPUAD  DS    XL1                 NUMBER OF PARTIAL WEEKS TO ADD UNIT          
CURRDAY  DS    XL1                 CURRENT DAY IF DAILY BUYING SET              
CTPOSTYP DS    XL1                 POSTING TYPE CONTROL BIT SETTING             
PRGSW    DS    XL1                 USED TO CALCULATE # IO'S                     
STLEN    DS    XL1                 UNIT LENGTH                                  
LOOP1    DS    XL1                 NUMBER OF UNITS TO ADD FOR DATE              
NPROGS   DS    XL1                 NUMBER OF PROGRAMS PROCESSED                 
SVXTRA   DS    XL7                 WHICH QTRS WERE PREVIOUSLY UPLOADED          
PUPDEMS  DS    XL150               OVERRIDE PUP DEMO AMOUNTS                    
SVINTTBL DS    1900C                                                            
LCLSPACE EQU   *-MYRELO                                                         
         SPACE 2                                                                
* DSECT TO COVER STATUS TABLE                                                   
*                                                                               
STATABD  DSECT                                                                  
STANAME  DS    CL8                 NAME                                         
STAMIN   DS    X                   MINIMUM COMPARE LENGTH                       
STAOR    DS    X                   OR MASK                                      
STAND    DS    X                   AND MASK                                     
STACTL   DS    X                   CONTROL VALUES (X'80'=CHANGE UNITS)          
STATABL  EQU   *-STATABD                                                        
         SPACE 2                                                                
* MODULE EQUATES                                                                
*                                                                               
UNITFIX  EQU   X'80'               UNITS MUST BE FIXED FOR THIS STATUS          
FROZEN   EQU   X'80'                                                            
LOCKED   EQU   X'20'                                                            
NOPRINT  EQU   X'10'                                                            
UNFROZEN EQU   X'FF'-X'80'                                                      
UNLOCKED EQU   X'FF'-X'20'                                                      
PRINT    EQU   X'FF'-X'10'                                                      
EQUAL    EQU   C'='                                                             
         SPACE 2                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 2                                                                
* NEGENPLAN                                                                     
         PRINT OFF                                                              
       ++INCLUDE NEGENPLAN                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENUNIV                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENUNIV                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENMKG                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENMKG                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENEST (ESTHDRD)                                                            
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENREP (REPRECD)                                                            
         PRINT OFF                                                              
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         PRINT ON                                                               
* NAVDSECTS                                                                     
       ++INCLUDE NAVDSECTS                                                      
         PRINT OFF                                                              
* GLOBBER EQUATES                                                               
       ++INCLUDE DDGLOBEQUS                                                     
         SPACE 2                                                                
* WSSVR BLOCK                                                                   
       ++INCLUDE FAWSSVRD                                                       
         SPACE 2                                                                
* COMFACS                                                                       
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017NEBUY47   02/26/20'                                      
         END                                                                    
