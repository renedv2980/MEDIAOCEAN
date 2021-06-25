*          DATA SET NEBUY19    AT LEVEL 091 AS OF 12/13/10                      
*PHASE T31119A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - DIS/CHA ESTIMATED ACT. T31119'             
T31119   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ESTA**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK          R7 POINTS TO LOCAL STORAGE                   
         USING TEMPD,R7                                                         
         ST    R1,MYPARM                                                        
         ST    RE,MYRELO                                                        
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
*                                                                               
DEM      TM    MODE,FIRST          FIRST TIME CLEAR SAVE AREA                   
         BZ    *+10                                                             
         XC    SVAREA,SVAREA                                                    
*                                                                               
         BAS   RE,ACTED            EDIT ACTION FIELD                            
*                                                                               
         BAS   RE,GETUNIT          GET THE UNIT RECORD                          
         BAS   RE,SAVGUAR          SAVE THE GUARANTEE FACTOR                    
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         GOTO1 VDISPROG                                                         
         GOTO1 VGETNADS                                                         
*                                                                               
         CLI   ACTION,DEA          TEST FOR ACTION DISPLAY                      
         BE    DEMDISP                                                          
*                                                                               
         TM    MODE,FIRST                                                       
         BO    DEMDISP                                                          
*                                                                               
* ACTION CHANGE - CEA                                                           
*                                                                               
DEMCHA   DS    0H                                                               
         BAS   RE,LOCKPACK                                                      
         BAS   RE,VCAT             VALIDATE CATEGORY                            
*                                                                               
         LA    R2,DEARAT1H                                                      
         ST    R2,FADDR                                                         
*                                                                               
         MVI   FERN,MISERR                                                      
         CLI   DEARAT1H+5,0                                                     
         BNE   *+12                                                             
         CLI   DEAIMP1H+5,0                                                     
         BE    ERROR                                                            
*                                                                               
         BAS   RE,VRAT             VALIDATE RATING                              
         BAS   RE,VIMP             VALIDATE IMPRESSION                          
         BAS   RE,ADDDC            ADD DC ELEMENT TO RECORD                     
*                                                                               
         LA    R4,BLOCK                                                         
         USING UNBLOCKD,R4                                                      
*                                                                               
         L     R4,AIOAREA1         POINT TO RECORD                              
         USING NURECD,R4                                                        
         MVC   KEY(L'NUKEY),0(R4)  USE RECORD KEY                               
         GOTO1 AIO,DMCB,UNT+DIR+READ  RE-READ RECORD AND REPLACE                
         GOTO1 (RF),(R1),UPDATE+UNT+FILE+GET,AIOAREA4                           
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         GOTO1 VBLDRQST             GENERATE TURN-AROUND REQUEST                
         DROP  R4                                                               
*                                                                               
* ACTION DISPLAY                                                                
*                                                                               
DEMDISP  DS    0H                                                               
         GOTO1 VCLEARF,DMCB,DEACAT1H,DEALAST                                    
         GOTO1 (RF),(R1),(1,DEACAT1H),DEALAST                                   
*                                                                               
         BAS   RE,DCAT                                                          
         BAS   RE,DRAT             DISPLAY RATINGS                              
         BAS   RE,DIMP             DISPLAY IMPRESSIONS                          
         B     DEMX                                                             
*                                                                               
DEMX     DS    0H                                                               
         BAS   RE,MSG              SET MESSAGE AND EXIT MODULE                  
         NI    MODE,X'FF'-DISPLAY                                               
         B     EXXMOD                                                           
********************************************************************            
* SUB-ROUTINE TO EDIT ACTION FIELD                                              
********************************************************************            
ACTED    ST    RE,SAVEREG                                                       
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         SEARCH FOR COMMA AT END OF ACTION            
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BNE   ACTM                NO                                           
*                                                                               
ACTED2   XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    ACTM                                                             
         MVI   FERN,INVERR                                                      
         MVI   FNDX,2                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),C',=,-'                              
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
*                                                                               
ACTED4   MVI   FERN,DATERR                                                      
         GOTO1 VDATVAL,(R1),(0,WORK+12),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ACTED4A                                                          
         CLC   WORK(1),3(R1)       DOES DATE MAKE UP FIRST HALF OF FLD          
         BNE   ERROR               NO                                           
         CLC   DUB(6),ESTSTART     TEST IF DATE BEFORE EST START                
         BL    ERROR               YES                                          
         SR    R0,R0                                                            
         ICM   R0,1,BUYPROF+14                                                  
         BZ    ERROR                                                            
         GOTO1 VADDAY,(R1),ESTEND,DUB2,(R0)                                     
         CLC   DUB(6),DUB2         TEST IF OUTSIDE OF EST + PROF DAYS           
         BH    ERROR               NO-SAME YEAR AS EST START                    
         B     ACTED5                                                           
*                                                                               
ACTED4A  GOTO1 VDATVAL,(R1),(1,WORK+12),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         CLC   WORK(1),3(R1)       DOES DATE MAKE UP FIRST HALF OF FLD          
         BNE   ERROR               NO                                           
         MVC   DUB(2),ESTSTART     ESTIMATE START YEAR (INPUT=MMDD)             
         CLC   ESTSTART(2),ESTEND  TEST IF EST START/END IN SAME YEAR           
         BE    ACTED5              YES                                          
         CLC   DUB+2(4),ESTSTART+2 TEST IF INPUT MMDD LT EST ST MMDD            
         BNL   *+10                NO-SAME YEAR AS EST START                    
         MVC   DUB(2),ESTEND       YES-MUST BE YEAR OF ESTIMATE END             
*                                                                               
ACTED5   GOTO1 VDATCON,(R1),DUB,(2,DATE)                                        
*                                                                               
ACTED6   MVI   SUB,1               DEFAULT IS SUB-LINE=1                        
         CLI   WORK+1,0            TEST FOR SUB-LINE NOTATION                   
         BE    ACTED8                                                           
         MVI   FNDX,0                                                           
         MVI   FERN,INVERR                                                      
         MVC   XTRA,SPACES                                                      
         MVC   XTRA(8),=C'SUB-LINE'                                             
         CLI   WORK+1,3                                                         
         BH    ERROR                                                            
         TM    WORK+3,X'80'        TEST IF NUMERIC                              
         BO    *+12                                                             
         MVI   FERN,NUMERR                                                      
         B     ERROR                                                            
*                                                                               
         ICM   R0,15,WORK+8                                                     
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,SUB                                                           
         B     ACTED8                                                           
*                                                                               
ACTED8   CLI   FSTOP,COMMA         TEST FOR COMMA                               
         BNE   ACTEDX                                                           
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         MVC   XTRA,SPACES                                                      
         GOTO1 AFVAL,0                                                          
         MVI   FNDX,3                                                           
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
*                                                                               
         MVI   FERN,INVERR                                                      
         CLC   FLD(3),=C'RS='      CHECK REASON CODE                            
         BNE   ACTED10                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),0                                    
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK+1,4                                                         
         BH    ERROR                                                            
         MVC   AUDREASN,WORK+22                                                 
         GOTO1 VCKREASN,DMCB,AUDREASN                                           
         B     ACTED8                                                           
*                                                                               
ACTED10  CLC   FLD(4),=C'EST='     EDIT FOR RE-LOOK UP OF EST DEMOS             
         BNE   ERROR                                                            
         CLI   FLDH+5,5                                                         
         BL    ERROR                                                            
         CLI   FLDH+5,7                                                         
         BH    ERROR                                                            
         ZIC   R1,FLDH+5                                                        
         SH    R1,=H'5'                                                         
         EX    R1,YESCOMP                                                       
         BNE   ERROR                                                            
         MVI   FERN,PAKFERR                                                     
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'80'      TEST FOR FROZEN PACKAGE                      
         BO    ERROR               YES-CANNOT CHANGE ESTIMATED DEMOS            
         MVI   ESTLOOK,YES                                                      
         B     ACTED8                                                           
         DROP  RE                                                               
*                                                                               
ACTM     MVI   FERN,MISERR                                                      
         MVC   XTRA(9),=C'UNIT DATE'                                            
         B     ERROR                                                            
*                                                                               
ACTEDX   CLI   BUYACT,C'D'         IS ACTION DISPLAY                            
         BE    ACTEDX30            DONT CHECK REASON CODE                       
         MVI   FERN,AUDITERR                                                    
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'02'      IS REASON CODE REQUIRED                      
         BO    ACTEDX10            YES                                          
         OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ACTEDX30                                                         
         B     ERROR                                                            
ACTEDX10 OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ERROR                                                            
*                                                                               
ACTEDX30 MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  RE                                                               
*                                                                               
YESCOMP  CLC   FLD+4(0),=C'YES'                                                 
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO CHECK FOR LOCKED PACKAGE                                       
********************************************************************            
LOCKPACK LR    R0,RE               SAVE RETURN POINT                            
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED PACKAGE                      
         BZ    LOCKPACX                                                         
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,PAKLERR                                                     
         B     ERROR                                                            
*                                                                               
LOCKPACX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO GET UNIT RECORD                                                
********************************************************************            
GETUNIT  ST    RE,SAVEREG                                                       
         L     RE,APACKREC                                                      
         MVC   NBSELDP,NPAKDP-NPRECD(RE)                                        
         GOTO1 VDATCON,DMCB,(2,DATE),NBSELSTR                                   
         MVC   NBSELEND,NBSELSTR                                                
         MVC   NBSELPRG,PROG                                                    
         MVC   NBSELSUB,SUB                                                     
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBDIRECT,YES                                                     
         MVI   NBUSER+13,NO        **FORCE PRE-EMPTS TO RE RETURNED             
         MVI   NBSELMOD,NBPROCUN                                                
         MVI   NBESTOPT,C'M'       LOOK UP HOMES VALUES                         
         OI    NBSPLOPT,X'10'      DONT BREAK OUT COPY SPLIT NUMBERS            
         MVC   NBAIO,AIOAREA1                                                   
         MVI   FERN,NOTFOUND                                                    
         XC    NBHUNOPT,NBHUNOPT                                                
         CLI   NBPOSTYP,C'S'                                                    
         BE    GETUNIT2                                                         
         CLI   NBPOSTYP,C'H'                                                    
         BE    GETUNIT2                                                         
         CLI   NBPOSTYP,C'N'                                                    
         BE    GETUNIT2                                                         
GETUNIT1 MVI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
*                                                                               
GETUNIT2 GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN     TEST FOR UNIT RETURNED                       
         BE    GETUNITX            YES                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    ERROR                                                            
         B     GETUNIT2                                                         
         SPACE                                                                  
GETUNITX L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO SAVE PACKAGE GUARANTIES AND CHANGE TO 100 PERCENT              
********************************************************************            
SAVGUAR  NTR1                                                                   
         CLC   NBACTDAT,=XL2'B32B' SEP11/89                                     
         BL    SAVGUAR8                                                         
         USING NUNGUD,R3                                                        
         XC    PNGUAHLD,PNGUAHLD                                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'B3',NBAIO),0                       
         CLI   12(R1),0            TEST IF PACKAGE GUARANTEE FOUND              
         BNE   SAVGUAR4            NO                                           
         L     R3,12(R1)                                                        
*                                                                               
         MVC   PNGUAHLD,NUNGUFAC                                                
         MVC   NUNGUFAC,=XL4'000F4240'                                          
*                                                                               
SAVGUAR4 XC    DGUARHLD,DGUARHLD                                                
         XC    DNGUAHLD,DNGUAHLD                                                
*                                                                               
         USING NUNDGD,R3                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'B4',NBAIO),0                       
         CLI   12(R1),0            TEST IF DEMO GUARANTEE FOUND                 
         BNE   SAVGUAR8            NO                                           
         L     R3,12(R1)                                                        
*                                                                               
         MVC   DNGUAHLD,NUNDGFAC                                                
         MVC   NUNDGFAC,=XL4'000F4240'                                          
*                                                                               
SAVGUAR8 B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO RESTORE PACKAGE GUARANTIES                                     
**********************************************************************          
RESGUAR  NTR1                                                                   
         CLC   NBACTDAT,=XL2'B32B' SEP11/89                                     
         BL    RESGUAR8                                                         
         USING NUNGUD,R3                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'B3',NBAIO),0                       
         CLI   12(R1),0            TEST IF PACKAGE GUARANTEE FOUND              
         BNE   RESGUAR4            NO                                           
         L     R3,12(R1)                                                        
*                                                                               
         MVC   NUNGUFAC,PNGUAHLD                                                
         MVC   NBNGUFAC,PNGUAHLD    RESTORE PACKAGE GUARENTEE                   
*                                                                               
         USING NUNDGD,R3                                                        
RESGUAR4 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'B4',NBAIO),0                       
         CLI   12(R1),0            TEST IF DEMO GUARANTEE FOUND                 
         BNE   RESGUAR8            NO                                           
         L     R3,12(R1)                                                        
*                                                                               
         MVC   NUNDGFAC,DNGUAHLD                                                
*                                                                               
RESGUAR8 B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* SUB-ROUTINE TO ADD DC ELEMENTS TO UNIT                                        
*********************************************************************           
ADDDC    NTR1                                                                   
*                                  DELETE ALL DC ELEMENTS                       
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'DC',NBAIO),0                       
*                                                                               
         XC    OVEREL,OVEREL                                                    
         LA    R4,OVEREL                                                        
         USING NUOVD,R4                                                         
*                                                                               
         MVI   NUOVEL,X'DC'        ELEMENT TYPE                                 
         MVI   NUOVLEN,X'0C'       ELEMENT LENGTH                               
         MVI   NUOVMOD,C'R'                                                     
         MVC   NUOVNUM,TMPDEMO     DEMO VALUE                                   
         MVC   NUOVPRE,TMPRPRE     RATING PRECISION                             
         MVC   NUOVVAL,TMPRAT      DEMO RATING                                  
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),NBAIO,OVEREL,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         XC    OVEREL,OVEREL                                                    
         LA    R4,OVEREL                                                        
         USING NUOVD,R4                                                         
*                                                                               
         MVI   NUOVEL,X'DC'        ELEMENT TYPE                                 
         MVI   NUOVLEN,X'0C'       ELEMENT LENGTH                               
         MVI   NUOVMOD,C'T'                                                     
         MVC   NUOVNUM,TMPDEMO     DEMO VALUE                                   
         MVC   NUOVPRE,TMPIPRE     IMP. PRECISION                               
         MVC   NUOVVAL,TMPIMP      DEMO IMPRESSION                              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),NBAIO,OVEREL,0                        
         CLI   12(R1),0                                                         
         BE    ADDDCX                                                           
         DC    H'00'                                                            
*                                                                               
ADDDCX   DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
* SUB-ROUTINE TO VALIDATE IMPRESSION                                            
*********************************************************************           
VIMP     NTR1                                                                   
         XC    TMPIMP,TMPIMP                                                    
         XC    TMPIPRE,TMPIPRE                                                  
*                                                                               
         LA    R2,DEAIMP1H                                                      
         CLI   5(R2),0                                                          
         BE    VIMPX                                                            
*                                                                               
         ST    R2,FADDR                                                         
*                                                                               
         LA    RE,DEMPREC          GET PRECISION FACTOR                         
         LA    RF,7                                                             
*                                                                               
VIMP10   DS    0H                                                               
         CLI   0(RE),C'T'          FOUND ENTRY IN TABLE                         
         BE    VIMP20                                                           
         LA    RE,2(RE)                                                         
         BCT   RF,VIMP10                                                        
         DC    H'00'                                                            
*                                                                               
VIMP20   DS    0H                                                               
         MVC   TMPIPRE,1(RE)       SAVE PRECISION                               
*                                                                               
         CLI   1(RE),X'42'         CABLE STATION?                               
         BE    VIMP200                                                          
*                                                                               
         MVI   FERN,INVERR         VALIDATE FOR NETWORK STATION                 
         ZIC   R1,5(R2)                                                         
         LA    R5,8(R2)                                                         
         CH    R1,=H'7'                                                         
         BH    ERROR               NO MORE THAN 5 DIGITS                        
*                                                                               
         XC    FLD,FLD                                                          
         XC    FLDH,FLDH                                                        
*                                                                               
         MVI   FLDH,X'10'                                                       
         MVC   FLDH+5(1),5(R2)                                                  
*                                                                               
         LA    RE,FLD                                                           
*                                                                               
VIMP30   DS    0H                                                               
         CLI   0(R5),C'.'          DECIMAL POINT INVALID FOR IMP.               
         BE    ERROR                                                            
         CLI   0(R5),C'0'          NUMERIC?                                     
         BL    ERROR                                                            
         CLI   0(R5),C'9'                                                       
         BH    ERROR                                                            
*                                                                               
         MVC   0(1,RE),0(R5)       BUILD FLD FOR CVB                            
         LA    RE,1(RE)                                                         
         LA    R5,1(R5)                                                         
         BCT   R1,VIMP30                                                        
*                                                                               
         ZIC   R1,FLDH+5           NO DECIMAL ENTERED                           
         BCTR  R1,0                FORCE 10TH'S PLACE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
*                                                                               
         ST    R0,TMPIMP                                                        
         B     VIMPX                                                            
*                                                                               
VIMP200  DS    0H                  VALIDATE FOR CABLE (2 DECIMAL)               
         MVI   FERN,INVERR                                                      
         ZIC   R1,5(R2)                                                         
         LA    R5,8(R2)                                                         
         CH    R1,=H'8'                                                         
         BH    ERROR               NO MORE THAN 5 DIGITS                        
*                                                                               
         XC    FLD,FLD                                                          
         XC    FLDH,FLDH                                                        
*                                                                               
         MVI   FLDH,X'10'                                                       
         MVC   FLDH+5(1),5(R2)                                                  
*                                                                               
         LA    RE,FLD                                                           
*                                                                               
VIMP210  DS    0H                                                               
         CLI   0(R5),C'.'          TEST FOR DECIMAL POINT                       
         BE    VIMP240                                                          
         CLI   0(R5),C'0'          NUMERIC?                                     
         BL    ERROR                                                            
         CLI   0(R5),C'9'                                                       
         BH    ERROR                                                            
*                                                                               
         MVC   0(1,RE),0(R5)       BUILD FLD FOR CVB                            
         LA    RE,1(RE)                                                         
         LA    R5,1(R5)                                                         
         BCT   R1,VIMP210                                                       
*                                                                               
         ZIC   R1,FLDH+5           NO DECIMAL ENTERED                           
         BCTR  R1,0                FORCE 10TH'S PLACE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
*                                                                               
         MH    R0,=H'10'          SCALE TO THE 10THS PLACE                      
         ST    R0,TMPIMP                                                        
         B     VIMPX                                                            
*                                                                               
VIMP240  DS    0H                  VALIDATE W/ DECIMAL                          
         CH    R1,=H'2'            ONLY 1 DIGIT AFTER DECIMAL                   
         BNE   ERROR                                                            
         CLI   1(R5),C'0'          TEST FOR NUMBER AFTER DECIMAL                
         BL    ERROR                                                            
         CLI   1(R5),C'9'                                                       
         BH    ERROR                                                            
*                                                                               
         MVC   0(1,RE),1(R5)       SAVE AWAY NUMBER W/O DECIMAL                 
*                                                                               
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                DECREMENT TWICE SINCE WE REMOVED             
         BCTR  R1,0                DECIMAL POINT                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
*                                                                               
         ST    R0,TMPIMP                                                        
         B     VIMPX                                                            
*                                                                               
VIMPX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*********************************************************************           
* SUB-ROUTINE TO VALIDATE RATING                                                
*********************************************************************           
VRAT     NTR1                                                                   
         XC    TMPRAT,TMPRAT                                                    
         XC    TMPRPRE,TMPRPRE                                                  
*                                                                               
         LA    R2,DEARAT1H                                                      
         CLI   5(R2),0                                                          
         BE    VRATX                                                            
*                                                                               
         ST    R2,FADDR                                                         
*                                                                               
         LA    RE,DEMPREC          GET PRECISION FACTOR                         
         LA    RF,7                                                             
*                                                                               
VRAT10   DS    0H                                                               
         CLI   0(RE),C'R'          FOUND ENTRY IN TABLE                         
         BE    VRAT20                                                           
         LA    RE,2(RE)                                                         
         BCT   RF,VRAT10                                                        
         DC    H'00'                                                            
*                                                                               
VRAT20   DS    0H                                                               
         MVC   TMPRPRE,1(RE)       SAVE PRECISION                               
*                                                                               
         CLI   1(RE),X'82'         CABLE STATION?                               
         BE    VRAT200                                                          
*                                                                               
         MVI   FERN,INVERR         VALIDATE FOR NETWORK STATION                 
         ZIC   R1,5(R2)                                                         
         LA    R5,8(R2)                                                         
         CH    R1,=H'7'                                                         
         BH    ERROR               NO MORE THAN 5 DIGITS                        
*                                                                               
         XC    FLD,FLD                                                          
         XC    FLDH,FLDH                                                        
*                                                                               
         MVI   FLDH,X'10'                                                       
         MVC   FLDH+5(1),5(R2)                                                  
*                                                                               
         LA    RE,FLD                                                           
*                                                                               
VRAT30   DS    0H                                                               
         CLI   0(R5),C'.'          TEST FOR DECIMAL POINT                       
         BE    VRAT40                                                           
         CLI   0(R5),C'0'          NUMERIC?                                     
         BL    ERROR                                                            
         CLI   0(R5),C'9'                                                       
         BH    ERROR                                                            
*                                                                               
         MVC   0(1,RE),0(R5)       BUILD FLD FOR CVB                            
         LA    RE,1(RE)                                                         
         LA    R5,1(R5)                                                         
         BCT   R1,VRAT30                                                        
*                                                                               
         ZIC   R1,FLDH+5           NO DECIMAL ENTERED                           
         BCTR  R1,0                FORCE 10TH'S PLACE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
*                                                                               
         MH    R0,=H'10'           SCALE TO THE 10THS PLACE                     
         ST    R0,TMPRAT                                                        
         B     VRATX                                                            
*                                                                               
VRAT40   DS    0H                  VALIDATE W/ DECIMAL                          
         CH    R1,=H'2'            ONLY 1 DIGIT AFTER DECIMAL                   
         BNE   ERROR                                                            
         CLI   1(R5),C'0'          TEST FOR NUMBER AFTER DECIMAL                
         BL    ERROR                                                            
         CLI   1(R5),C'9'                                                       
         BH    ERROR                                                            
*                                                                               
         MVC   0(1,RE),1(R5)       SAVE AWAY NUMBER W/O DECIMAL                 
*                                                                               
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                DECREMENT TWICE SINCE WE REMOVED             
         BCTR  R1,0                DECIMAL POINT                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
*                                                                               
         ST    R0,TMPRAT                                                        
         B     VRATX                                                            
*                                                                               
VRAT200  DS    0H                  VALIDATE FOR CABLE (2 DECIMAL)               
         MVI   FERN,INVERR                                                      
         ZIC   R1,5(R2)                                                         
         LA    R5,8(R2)                                                         
         CH    R1,=H'8'                                                         
         BH    ERROR               NO MORE THAN 5 DIGITS                        
*                                                                               
         XC    FLD,FLD                                                          
         XC    FLDH,FLDH                                                        
*                                                                               
         MVI   FLDH,X'10'                                                       
         MVC   FLDH+5(1),5(R2)                                                  
*                                                                               
         LA    RE,FLD                                                           
*                                                                               
VRAT210  DS    0H                                                               
         CLI   0(R5),C'.'          TEST FOR DECIMAL POINT                       
         BE    VRAT240                                                          
         CLI   0(R5),C'0'          NUMERIC?                                     
         BL    ERROR                                                            
         CLI   0(R5),C'9'                                                       
         BH    ERROR                                                            
*                                                                               
         MVC   0(1,RE),0(R5)       BUILD FLD FOR CVB                            
         LA    RE,1(RE)                                                         
         LA    R5,1(R5)                                                         
         BCT   R1,VRAT210                                                       
*                                                                               
         ZIC   R1,FLDH+5           NO DECIMAL ENTERED                           
         BCTR  R1,0                FORCE 100TH'S PLACE                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
*                                                                               
         MH    R0,=H'100'          SCALE TO THE 100THS PLACE                    
         ST    R0,TMPRAT                                                        
         B     VRATX                                                            
*                                                                               
VRAT240  DS    0H                  VALIDATE W/ DECIMAL                          
         CH    R1,=H'3'            ONLY 2 DIGITS AFTER DECIMAL                  
         BNE   ERROR                                                            
         CLI   1(R5),C'0'          TEST FOR NUMBER AFTER DECIMAL                
         BL    ERROR                                                            
         CLI   1(R5),C'9'                                                       
         BH    ERROR                                                            
         CLI   2(R5),C'0'          TEST FOR NUMBER AFTER DECIMAL                
         BL    ERROR                                                            
         CLI   2(R5),C'9'                                                       
         BH    ERROR                                                            
*                                                                               
         MVC   0(2,RE),1(R5)       SAVE AWAY NUMBER W/O DECIMAL                 
*                                                                               
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                DECREMENT TWICE SINCE WE REMOVED             
         BCTR  R1,0                DECIMAL POINT                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
*                                                                               
         ST    R0,TMPRAT                                                        
         B     VRATX                                                            
*                                                                               
VRATX    DS    0H                                                               
         B     EXXMOD                                                           
*********************************************************************           
* SUB-ROUTINE TO VALIDATE DEMO CATEGORY NAME                                    
*********************************************************************           
VCAT     NTR1                                                                   
         LA    R4,DBLOCKA          INIT DBLOCK                                  
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         LA    RF,DEACAT1H                                                      
         ST    RF,WORK                                                          
*                                                                               
         LA    R2,DEACAT1H                                                      
         ST    R2,FADDR                                                         
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+8                                                              
         BAS   RE,DCAT                                                          
*                                                                               
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
*                                                                               
         MVI   FERN,INVERR                                                      
         GOTO1 CDEMOVAL,DMCB,DEACAT1H,(1,FULL),DBLOCKA                          
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
*                                                                               
         CLI   FULL+2,X'01'        HOMES?                                       
         BE    ERROR                                                            
*                                                                               
         MVC   TMPMOD,FULL+1       MODIFIER                                     
         MVC   TMPDEMO,FULL+2      DEMO                                         
*                                                                               
VCATX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4,R3                                                            
*********************************************************************           
* SUB-ROUTINE TO DISPLAY DEMO CATEGORY NAMES                                    
*********************************************************************           
DCAT     NTR1                                                                   
         LA    R4,DBLOCKA          INIT  DBLOCK                                 
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         LA    R2,DEACAT1H                                                      
         XC    DEACAT1,DEACAT1                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   THREE,ESTDEMSE      DEFAULT TO ESTIMATE DEMO                     
         CLI   ESTDEMSE+2,X'01'    HOMES IS RESTRICTED                          
         BNE   *+10                                                             
         MVC   THREE,ESTDEMSE+3                                                 
*                                                                               
         MVI   THREE+1,C'I'        FORCE ONLY DEMO DISPLAY                      
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'DC',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BE    DCAT10                                                           
*                                                                               
         CLI   ACTION,DEA                                                       
         BE    DCATX                                                            
         B     DCAT50                                                           
*                                                                               
DCAT10   DS    0H                                                               
         L     RE,12(R1)                                                        
         USING NUOVD,RE                                                         
         MVC   THREE,NUOVCAT                                                    
         DROP  RE                                                               
*                                                                               
         MVI   THREE+1,C'I'        FORCE ONLY DEMO DISPLAY                      
*                                                                               
DCAT50   DS    0H                                                               
         GOTO1 VDEMOCON,DMCB,THREE,(10,WORK),(C'S',DBLOCK),ESTUSNS              
         MVC   8(10,R2),WORK       CATEGORY NAME                                
         XC    WORK(10),WORK                                                    
         CLI   1(R3),USERMOD       TEST FOR USER DEMO                           
         BNE   DCATX               NO                                           
         ZIC   R1,2(R3)            EXTRACT USER DEMO NAME BY INDEXING           
         MH    R1,=H'7'            INTO NAME LIST WITH CATEGORY                 
         LA    RE,ESTUSNS-7(R1)                                                 
         XC    8(10,R2),8(R2)      CLEAR FIELD                                  
         MVC   8(7,R2),0(RE)                                                    
*                                                                               
DCATX    B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO DISPLAY IMPRESSION                                             
********************************************************************            
DIMP     NTR1                                                                   
         LA    R2,DEAIMP1H                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    TMPVALUE,TMPVALUE                                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'DC',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   DCATX                                                            
         L     R4,12(R1)                                                        
         USING NUOVD,R4                                                         
*                                                                               
DIMP10   DS    0H                                                               
         CLI   0(R4),X'DC'         NO IMPRESSION DC ELEMENT FOUND               
         BNE   DIMPX                                                            
*                                                                               
         CLI   NUOVMOD,C'T'        IMPRESSION ELEMENT?                          
         BE    DIMP20                                                           
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     DIMP10                                                           
*                                                                               
DIMP20   DS    0H                                                               
         OC    NUOVVAL,NUOVVAL                                                  
         BZ    DIMPX                                                            
*                                                                               
         MVC   TMPVALUE,NUOVVAL    SAVE AWAY VALUE                              
         L     R0,TMPVALUE                                                      
*                                                                               
         LA    RE,DEMPREC                                                       
         LA    RF,7                                                             
*                                                                               
DIMP30   DS    0H                                                               
         CLI   0(RE),C'T'                                                       
         BE    DIMP40                                                           
         LA    RE,2(RE)                                                         
         BCT   RF,DIMP30                                                        
         DC    H'00'                                                            
*                                                                               
DIMP40   DS    0H                                                               
         CLI   1(RE),X'42'         CABLE STATION?                               
         BE    DIMP70              YES                                          
*                                                                               
         EDIT  (R0),(6,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
         B     DIMPX                                                            
*                                                                               
DIMP70   DS    0H                  CABLE STATION (1 DECIMAL)                    
         EDIT  (R0),(6,8(R2)),1,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
DIMPX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO DISPLAY RATING                                                 
********************************************************************            
DRAT     NTR1                                                                   
         LA    R2,DEARAT1H                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    TMPVALUE,TMPVALUE                                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'DC',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   DCATX                                                            
         L     R4,12(R1)                                                        
         USING NUOVD,R4                                                         
*                                                                               
DRAT10   DS    0H                                                               
         CLI   0(R4),X'DC'         NO RATING DC ELEMENT FOUND                   
         BNE   DRATX                                                            
*                                                                               
         CLI   NUOVMOD,C'R'        RATING ELEMENT?                              
         BE    DRAT20                                                           
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     DRAT10                                                           
*                                                                               
DRAT20   DS    0H                                                               
         OC    NUOVVAL,NUOVVAL                                                  
         BZ    DRATX                                                            
*                                                                               
         MVC   TMPVALUE,NUOVVAL    SAVE AWAY VALUE                              
         L     R0,TMPVALUE                                                      
*                                                                               
         LA    RE,DEMPREC                                                       
         LA    RF,7                                                             
*                                                                               
DRAT30   DS    0H                                                               
         CLI   0(RE),C'R'                                                       
         BE    DRAT40                                                           
         LA    RE,2(RE)                                                         
         BCT   RF,DRAT30                                                        
         DC    H'00'                                                            
*                                                                               
DRAT40   DS    0H                                                               
         CLI   1(RE),X'82'         CABLE STATION?                               
         BE    DRAT70              YES                                          
*                                                                               
         EDIT  (R0),(6,8(R2)),1,ALIGN=LEFT,ZERO=NOBLANK                         
         B     DRATX                                                            
*                                                                               
DRAT70   DS    0H                  CABLE STATION (2 DECIMALS)                   
         EDIT  (R0),(6,8(R2)),2,ALIGN=LEFT,ZERO=NOBLANK                         
*                                                                               
DRATX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
********************************************************************            
* SUB-ROUTINE TO PUT AN OVERRIDE ELEMENT TO RECORD                              
********************************************************************            
PUTEL    LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,OVEREL,0                     
         CLI   12(R1),0            TEST IF OK                                   
         BE    PUTELX              YES                                          
         CLI   12(R1),5            TEST FOR RECORD OVERFLOW                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FERN,TOOLARGE                                                    
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         SPACE                                                                  
PUTELX   LR    RE,R0                                                            
         BR    RE                                                               
**********************************************************************          
* SUB-ROUTINE TO FORMAT MESSAGE AND TO SET CURSOR                               
**********************************************************************          
MSG      ST    RE,SAVEREG                                                       
         MVC   BUYMSG(17),=C'ESTIMATED ACTUALS'                                 
         TM    MODE,DISPLAY                                                     
         BO    MSG2                                                             
         CLI   ACTION,CEA          TEST FOR ACTION CHANGE                       
         BNE   MSG2                                                             
         MVC   BUYMSG+18(8),=C'CHANGED.'                                        
         MVC   BUYMSG+28(17),=C'ENTER NEXT ACTION'                              
         B     MSGX                                                             
         SPACE                                                                  
MSG2     MVC   BUYMSG+18(10),=C'DISPLAYED.'                                     
         LA    RE,BUYMSG+30                                                     
         MVC   0(13,RE),=C'ENTER CHANGES'                                       
         CLI   MORESW,YES          TEST FOR MORE DEMOS                          
         BNE   *+10                                                             
         MVC   14(L'MOREMSG,RE),MOREMSG                                         
         CLI   ACTION,CEA          TEST FOR ACTION CHANGE                       
         BE    MSGX                                                             
         MVC   0(17,RE),=C'ENTER NEXT ACTION'                                   
         CLI   MORESW,YES          TEST FOR MORE DEMOS                          
         BNE   *+14                                                             
         MVI   18(RE),C' '                                                      
         MVC   19(L'MOREMSG,RE),MOREMSG                                         
         OI    DEARAT1H+6,X'01'     MAKE FIELD MODIFIED FOR RE-ENTRY            
         SPACE                                                                  
MSGX     LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
NOMORE   DC    C'**NO MORE DEMOS TO BE DISPLAYED - ENTER NEXT ACTION**'         
MOREMSG  DC    C'(MORE DEMOS)'                                                  
HOMES    DC    X'00',C'T',X'01'                                                 
HOMRAT   DC    C'R',X'01'                                                       
HOMIMP1  DC    C'T',X'01'                                                       
HOMIMP2  DC    C'H',X'01'                                                       
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* DEMO SCREEN                                                                   
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYD0D                                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVAREA   DS    0XL80                                                            
SVDATE   DS    XL2                                                              
SVSUB    DS    X                                                                
SVLPAGE  DS    X                   LAST PAGE DISPLAYED                          
SVLACT   DS    X                   LAST ACTION                                  
SVNDEMS  DS    X                                                                
SVDEMOS  DS    XL60                                                             
*                                                                               
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
SAVEREG3 DS    A                                                                
SAVEREG4 DS    A                                                                
SAVEREGE DS    A                                                                
AVALTAB  DS    A                   ADDRESS OF VALTAB                            
VDISPLAY DS    V                   V(GENERAL UNIT DISPLAY)                      
VEDIT    DS    V                   V(GENERAL UNIT EDIT)                         
*                                                                               
DATE     DS    XL2                 DATE EXTRACTED FROM ACTION FIELD             
SUB      DS    X                   SUBLINE EXTRACTED FROM ACTION FIELD          
*                                                                               
DEMS     DS    X                   DEMOS FOR SCREEN                             
DISP     DS    X                   DISPLACEMENT INDEX                           
LINE     DS    X                   SCREEN LINE INDEX                            
WEIGHT   DS    X                   WEIGHTED DEMO NUMBER                         
RATSW    DS    C                   RATING OVERRIDE IN ESTIMATED VPH Y/N         
ESTLOOK  DS    C                   FORCE LOOKUP OF ESTIMATED DEMOS (Y)          
MORESW   DS    C                   MORE DEMOS TO COME (Y)                       
*                                                                               
OVEREL   DS    XL20                OVERRIDE ELEMENT AREA                        
USERNMS  DS    CL28                USER DEMO NAMES                              
*                                                                               
THISLINE DS    A                   SCREEN LINE POINTER                          
LDISP    DS    H                   DISPLACEMENT INTO VALUE LIST                 
PNGUAHLD DS    F                   NEW PACKAGE GUARANTEE HOLD AREA              
DNGUAHLD DS    F                   NEW DEMO GUARANTEE HOLD AREA                 
PGUARHLD DS    H                   PACKAGE GUARANTEE HOLD AREA                  
DGUARHLD DS    H                   DEMO GUARANTEE HOLD AREA                     
*                                                                               
TMPMOD   DS    CL1                 MODIFIER (FOR IMPRESSION)                    
TMPDEMO  DS    XL1                 DEMO                                         
TMPPRE   DS    XL1                 TEMPORARTY PRECISION FACTOR                  
TMPRPRE  DS    XL1                 RATING PRECISION                             
TMPIPRE  DS    XL1                 IMPRESSION PRECICION                         
TMPVALUE DS    F                   DEMO VALUE                                   
TMPRAT   DS    F                   DEMO RATING                                  
TMPIMP   DS    F                   DEMO IMPRESSION                              
*                                                                               
* DEMO VALUES INPUT BY USER                                                     
*                                                                               
NEWARHM  DS    F                   NEW ACTUAL RATING HOMES                      
*                                                                               
OVERSTOR DS    D                   STORAGE FOR OVERIDE DEMO FILTER              
SVUSRUNV DS    4F                                                               
BLOCK    DS    CL256                                                            
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
USERMOD  EQU   33                  USER DEMO MODIFIER CODE                      
PERIOD   EQU   C'.'                                                             
         EJECT                                                                  
* DSECT TO COVER DEMO VALUE TABLE                                               
*                                                                               
VALTABD  DSECT                                                                  
VALFDISP DS    X                   DISPLACEMENT INTO SCREEN LINE                
VALODISP DS    AL3                 DISPLACEMENT INTO OLD VALUE LIST             
VALNDISP DS    AL3                 DISPLACEMENT INTO NEW VALUE LIST             
VALOVER  DS    X                   OVERRIDE ELEMENT CODE                        
VALUSER  DS    X                   USER DEMO OVERRIDE CODE                      
VALMOD   DS    C                   MODIFIER CODE                                
VALDIS   DS    X                   DISPLAY CONTROL BITS                         
*                                  X'01' = OUTPUT HAS ONE DECIMAL PLACE         
*                                  X'02' = CABLE HAS ONE DECIMAL PLACE          
*                                  X'04' = USE PRECISION TABLE FOR EDIT         
VALEDIT  DS    X                   EDIT CONTROL BITS                            
*                                  X'10' = 'R' PREFIX ALLOWED IN FIELD          
*                                  X'01' = VALUE HAS ONE DECIMAL PLACE          
*                                  X'02' = CABLE HAS ONE DECIMAL PLACE          
*                                  X'04' = USE PRECISION TABLE FOR EDIT         
VALOVCTL DS    X                   OVERRIDE CONTROL BITS                        
*                                  X'80' = GENERATE OVERRIDE VPH                
*                                  X'40' = DON'T OVERRIDE EST IMP               
VALTABL  EQU   *-VALTABD                                                        
         SPACE 2                                                                
* NETDEMOD (DSECT COVERING NETWORK DEMO BLOCK)                                  
*                                                                               
         PRINT OFF                                                              
NETDEMOD DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* NETDEMOE (DSECT COVERING NETWORK DEMO BLOCK SUPPORTS 50 DEMOS)                
*                                                                               
         PRINT OFF                                                              
NETDEMOE DSECT                                                                  
       ++INCLUDE NETDEMOP                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* NETUNIVD (DSECT COVERING NETWORK UNIVERSE BLOCK)                              
*                                                                               
         PRINT OFF                                                              
NETUNIVD DSECT                                                                  
       ++INCLUDE NETUNIVD                                                       
         PRINT ON                                                               
*                                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091NEBUY19   12/13/10'                                      
         END                                                                    
