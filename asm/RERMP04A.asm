*          DATA SET RERMP04A   AT LEVEL 165 AS OF 05/01/02                      
*PHASE T81004A,+0                                                               
*INCLUDE REBKLSTB                                                               
         TITLE 'T81004 - REPPAK FILE MAINT - GLOBAL ADD'                        
********************************************************************            
T81004   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81004,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T81004+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
         ST    R5,RELO                                                          
*                                                                               
         L     RF,=V(BKLST)                                                     
         L     RE,RELO                                                          
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,VREBKLST                                                      
*                                                                               
         OI    GENSTAT4,CONFDEL    CONFIRM DELETES                              
         OI    GENSTAT3,OKVALSEL   VALIDATE SELECT FIELDS MYSELF                
         NI    MYFLAG2,X'FF'-BADTRACK                                           
         NI    MYFLAG2,X'FF'-NONEWHED                                           
         NI    MYFLAG3,X'FF'-GLBLPROT                                           
         NI    MYFLAG3,X'FF'-NOTCLOSE                                           
*                                                                               
         LH    RF,=H'34'                                                        
         STH   RF,DATADISP                                                      
*                                                                               
         MVI   IOOPT,C'Y'      CONTROL MY OWN ADDREC AND PUTREC                 
         LR    R3,RA           MOVE PROFILE TO LOCAL WORKING STORAGE            
         AH    R3,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R3                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
         DROP  R3                                                               
*                                                                               
         DS    0H                   SET PRECISION TYPE FOR DEMO CALCS           
         MVI   TAPEOPT,0            BOOK (RTG) BASED DEMO CALCULATIONS          
         TM    RMPPROFS+RMPIMPSB,RMPIMPSA                                       
         BZ    *+8                                                              
         MVI   TAPEOPT,C'Y'         TAPE (IMP) BASED, AS PER PROFILE            
*                                                                               
         TM    MTFLAG,RELOMAIN     RELOAD MAINT SCREEN?                         
         BO    MAIN5               YES                                          
*                                                                               
         CLI   MTSCRN,X'FF'         FINISHED WITH PROCESSING SCREEN -           
         BNE   MAIN10               AND CAME BACK TO REQUEST SCREEN?            
         TM    MTFLAG,SRCEFF                                                    
         BZ    *+10                                                             
         MVC   TWAKEYSV,RELOKEY                                                 
*                                                                               
         OI    MTFLAG,RELOMAIN      RELO MAINT SCRN NEXT TIME THROUGH           
         MVC   RERROR,=AL2(ALLDONE)                                             
         B     ERREND2                                                          
*                                                                               
MAIN5    DS    0H                                                               
         GOTO1 CALLOV,DMCB,(X'C5',GL1HEREH),0     GET MAINT. SCREEN             
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVI   MTSCRN,X'C5'                                                     
         BAS   RE,CLRKEY           CLEAR KEY FIELDS                             
*                                                                               
         NI    MTFLAG,X'FF'-RELOMAIN                                            
*                                                                               
         LA    R3,MENUTAB                                                       
         XCEF  (R3),305                                                         
         XC    MAINTAB,MAINTAB                                                  
         XC    MENUCNT,MENUCNT                                                  
         XC    MTFLAG,MTFLAG                                                    
         LA    R3,TRACKTAB         CLEAR TRACK TABLE                            
         XCEF  (R3),432            L'TRACK TABLE                                
         BAS   RE,VKEY             YES - PROCESS NEW REQUEST                    
*                                                                               
MAIN10   MVC   AIO,AIO1                                                         
         LA    R6,MENUTAB          TABLE FOR IDENTIFIERS                        
         ST    R6,AMENUTAB                                                      
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
         MVC   MYSCRNUM,TWASCR     SET SCREEN NUMBER                            
*                                                                               
         GOTO1 CALLOV,DMCB,(X'30',0),(RA)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VT81030,DMCB        A(T81030)                                    
*                                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVALID                                                    
         CLI   ACTNUM,ACTCHA       MUST BE ACTION CHANGE!                       
         BNE   ERREND                                                           
         EJECT                                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+8                                                              
         BAS   RE,VKEY                                                          
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         BNE   EXIT                                                             
         MVC   RERROR(2),=AL2(INVACT)                                           
         LA    R2,CONACTH                                                       
         B     ERREND2                                                          
*                                                                               
EXIT     DS    0H                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
         XIT1                                                                   
         EJECT                                                                  
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
VKEY     NTR1                                                                   
         XC    KEY,KEY                                                          
         XC    STAHLD(15),STAHLD                                                
         XC    MAINTAB,MAINTAB     HOLD DATE FROM REQUEST SCREEN                
         XC    MTFLAG,MTFLAG                                                    
         XC    MENUCNT,MENUCNT                                                  
         XC    SV1STA,SV1STA                                                    
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,GL1SSTAH         STATION                                      
         CLI   5(R2),0             ANY STATION?                                 
         BE    ERREND              NO - MISSING INPUT                           
*                                                                               
         CLI   8(R2),C'*'          MUST BE SET IDENTIFER (*XXX)                 
         BNE   ERREND                                                           
         LA    R3,MENUTAB                                                       
         XCEF  (R3),305                                                         
         XC    MAINTAB,MAINTAB                                                  
         GOTO1 INVMENU,DMCB,9(R2),MENUTAB                                       
*                                                                               
VK10     DS    0H                                                               
         CLI   MENUCNT,0           FINISHED ALL STATIONS                        
         BE    EXIT                                                             
*                                                                               
VK20     DS    0H                                                               
         L     R6,AMENUTAB                                                      
         MVC   STAHLD,0(R6)        STATION                                      
         MVC   CSTAT,STAHLD                                                     
         MVC   CCOSCRST,8(R2)                                                   
*                                                                               
         XC    INVHLD,INVHLD                                                    
         MVI   ERROR,MISSING                                                    
         LA    R2,GL1INVH          INVENTORY NUMBER                             
         CLI   5(R2),0             ANY INVENTORY #?                             
         BE    ERREND                                                           
*                                                                               
VK50     CLI   5(R2),4             MAX LENGTH IS 4                              
         BH    ERREND                                                           
         MVC   INVHLD(4),8(R2)                                                  
         OC    INVHLD(4),=4X'40'                                                
         MVC   MTINV,8(R2)         ORIGINAL INV#                                
         OC    MTINV,=4X'40'                                                    
*                                                                               
VK60     LA    R2,GL1EFFH          EFFECTIVE DATE                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
*                                                                               
         LA    RE,GL1EFF           A(INPUT DATE FIELD)                          
         ST    RE,DMCB                                                          
         MVC   DMCB(1),GL1EFFH+5   L'INPUT FIELD                                
         LA    RE,WORK2            A(PERVAL OUTPUT BLOCK)                       
         ST    RE,DMCB+4                                                        
         OI    DMCB+4,X'40'        SINGLE DATE IS ONLY VALID                    
*                                                                               
         GOTO1 PERVAL,DMCB                                                      
         CLI   DMCB+4,X'04'        VALID SINGLE DATE INPUT?                     
         BNE   ERREND                                                           
*                                                                               
         LA    R3,WORK2                                                         
         USING PERVALD,R3                                                       
         XC    DTEHLD,DTEHLD                                                    
         XC    DTEHLD2(4),DTEHLD2                                               
*                                                                               
         MVC   DTEHLD,PVALBSTA        BIN YYMMDD START OF PERIOD                
         MVC   DTEHLD2(2),PVALCSTA    BIN COMPRESSED START OF PERIOD            
         MVC   MTEFF,PVALBSTA         ORIGINAL EFFECTIVE DATE                   
*                                                                               
VK100    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   CCONKSTA,STAHLD                                                  
         MVC   CCONINV,INVHLD                                                   
         MVC   CCONEFF,GL1EFF                                                   
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,DTEHLD                                                  
         MVC   SAVEKEY,KEY                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 =A(OVFLRTN4),DMCB,(1,DUB),(RC),RR=RELO    (FRSTSTA)              
*                                                                               
         GOTO1 GETINV              GET THE INVENTORY RECORD                     
         L     R6,AIO                                                           
         MVC   TIMECHG,RINVTCHG    TIME CHANGE                                  
*                                                                               
         GOTO1 CALLOV,DMCB,(X'C5',GL1HEREH),0     LOAD MAIN SCREEN              
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVI   MTSCRN,X'C5'        DONE W/ PROCESSING SCREEN                    
         BAS   RE,XMTALL                                                        
         OI    GL1SSTAH+4,X'01'                                                 
*                                                                               
VK119    EQU   *                                                                
         XC    MTFLAG,MTFLAG                                                    
*                                                                               
         NI    GLBNINVH+1,X'FF'-X'20' TURN OFF PROTECT BIT                      
         OI    GLBNINVH+6,X'80'                                                 
         NI    GLBNIN2H+1,X'FF'-X'20' TURN OFF PROTECT BIT                      
         OI    GLBNIN2H+6,X'80'                                                 
*                                                                               
         TM    RMPPROFS,X'80'      SYSTEM ASSIGNED INV #'S                      
         BO    VKXIT               NO                                           
         OI    GLBNINVH+1,X'20'    YES - PROTECT FIELD                          
         OI    GLBNINVH+6,X'80'                                                 
         OI    GLBNIN2H+1,X'20'    PROTECT FIELD                                
         OI    GLBNIN2H+6,X'80'                                                 
*                                                                               
VKXIT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
VREC     DS    0H                                                               
         CLI   MTSCRN,X'C6'        CONTINUE PROCESSING STATIONS?                
         BNE   VR5                 NO                                           
         MVC   INVHLD,MTINV        ORIGINAL INVENTORY #                         
         MVC   DTEHLD,MTEFF        ORIGINAL EFFECTIVE DATE                      
         MVC   DTEHLD2+2,MTEND     ORIGINAL END DATE                            
         B     VR15                                                             
*&&DO                                                                           
VR5      BAS   RE,ENDDATE          GET NEW END DATE                             
*&&                                                                             
*                                                                               
VR5      DS    0H                                                               
         GOTO1 =A(OVFLRTN6),DMCB,(0,DUB),(RC),RR=RELO    (ENDDATE)              
*                                                                               
         TM    MTFLAG,JUSTSRC      ONLY CHANGE SOURCE INV?                      
         BZ    VR5A                                                             
         GOTO1 =A(OVFLRTN6),DMCB,(2,DUB),(RC),RR=RELO    (GETPGMS)              
         GOTO1 =A(OVFLRTN6),DMCB,(3,DUB),(RC),RR=RELO    (GETDAYS)              
         GOTO1 =A(OVFLRTN6),DMCB,(4,DUB),(RC),RR=RELO    (GETTIMES)             
         BAS   RE,GETNINV          GET NEW INVENTORY #'S                        
*                                                                               
VR5A     DS    0H                                                               
         TM    MTFLAG,JUSTTRAK     ADD ONLY TRACK RECORDS?                      
         BZ    VR6                                                              
         GOTO1 =A(OVFLRTN3),DMCB,(12,DUB),(RC),RR=RELO    (FILLPGM1)            
*                                                                               
VR6      CLI   GLBNEFFH+5,0        ADD ANY NEW INV HEADERS?                     
         BE    VR10                NO                                           
*&&DO                                                                           
         BAS   RE,NEWEFFD          GET NEW EFFECTIVE DATE                       
         BAS   RE,GETPGMS          GET NEW PROGRAM NAMES                        
         BAS   RE,GETDAYS          GET NEW DAYS + DAY CODES                     
         BAS   RE,GETTIMES         GET NEW TIMES                                
*&&                                                                             
*                                                                               
         GOTO1 =A(OVFLRTN6),DMCB,(1,DUB),(RC),RR=RELO    (NEWEFFD)              
         GOTO1 =A(OVFLRTN6),DMCB,(2,DUB),(RC),RR=RELO    (GETPGMS)              
         GOTO1 =A(OVFLRTN6),DMCB,(3,DUB),(RC),RR=RELO    (GETDAYS)              
         GOTO1 =A(OVFLRTN6),DMCB,(4,DUB),(RC),RR=RELO    (GETTIMES)             
*                                                                               
         TM    RMPPROFS,X'80'      SYSTEM DEFINED INV #?                        
         BZ    VR7                 YES - ALREADY GOT INV #                      
         BAS   RE,GETNINV          GET NEW INVENTORY #'S                        
*                                                                               
         CLC   MTINV1,MTINV2       SAME INVENTORY #'S?                          
         BNE   VR8                                                              
         CLC   MTNEFFB,MTNEFF2B    SAME EFFECTIVE DATES?                        
         BNE   VR8                                                              
         MVC   RERROR(2),=AL2(SAMEKEY)                                          
         LA    R2,GLBNEF2H                                                      
         B     ERREND2                                                          
*                                                                               
VR7      EDIT  (B1,MTQTR1),(2,MTINV1),FILL=0                                    
         MVC   MTINV1+2(1),MTDCODE1                                             
         MVC   MTINV1+3(1),MTLEN1                                               
*                                                                               
         MVC   GLBNINV(4),MTINV1                                                
         MVI   GLBNINVH+5,4                                                     
         OI    GLBNINVH+1,X'20'    PROTECT FIELD                                
         OI    GLBNINVH+6,X'80'                                                 
*                                                                               
         EDIT  (B1,MTQTR2),(2,MTINV2),FILL=0                                    
         MVC   MTINV2+2(1),MTDCODE2                                             
         MVC   MTINV2+3(1),MTLEN2                                               
*                                                                               
         MVC   GLBNIN2(4),MTINV2                                                
         MVI   GLBNIN2H+5,4                                                     
         OI    GLBNIN2H+6,X'80'                                                 
         OI    GLBNINVH+1,X'20'    PROTECT FIELD                                
*                                                                               
VR8      DS    0H                                                               
         GOTO1 =A(OVFLRTN4),DMCB,(0,DUB),(RC),RR=RELO    (BLDTTAB)              
*                                                                               
         CLI   MTPGM2LN,0          ANY 2ND PROGRAM?                             
         BE    VR10                NO                                           
*                                                                               
         GOTO1 SCANNER,DMCB,(0,GLBNEF2H),WORK,C',=,-'                           
         CLI   DMCB+4,0            DID IT WORK?                                 
         BE    ERREND              NO                                           
*                                                                               
         CLC   MTINV1,MTINV2       SAME INV #?                                  
         BNE   VR9                                                              
         CLI   WORK+1,0            SHOULD HAVE A CLOSE OUT #                    
         BNE   VR9A                                                             
         MVC   RERROR(2),=AL2(CLOSEINV)                                         
         LA    R2,GLBNEF2H                                                      
         B     ERREND2                                                          
*                                                                               
VR9      DS    0H                                                               
         LA    R2,GLBNEF2H                                                      
         MVI   ERROR,INVALID                                                    
         CLI   WORK+1,0            SHOULD NOT HAVE A CLOSE OUT DATE             
         BNE   ERREND                                                           
         B     VR10                                                             
*                                                                               
VR9A     BAS   RE,ENDDATE2         GET END DATE FOR 1ST PROGRAM                 
*                                                                               
VR10     DS    0H                                                               
*  MAKE SURE NO DATE IS LATER THAN 12/31/26                                     
*                                                                               
         CLC   MTEND,=XL2'FD9F'                                                 
         BH    ERR2027                                                          
         CLC   MTEND2,=XL2'FD9F'                                                
         BH    ERR2027                                                          
         CLC   MTNEFFC,=XL2'FD9F'                                               
         BH    ERR2027                                                          
         CLC   MTNEFF2C,=XL2'FD9F'                                              
         BH    ERR2027                                                          
*                                                                               
         CLI   MTSCRN,X'C6'        ALREADY IN PROC. SCREEN                      
         BE    VR15                NO                                           
*                                                                               
         GOTO1 CALLOV,DMCB,(X'C6',GL1HEREH),0    LOAD PROCESSING SCR.           
         CLI   DMCB+4,X'FF'        ERROR?                                       
         BNE   *+6                                                              
         DC    H'00'                                                            
         MVI   MTSCRN,X'C6'        SET SCREEN NUMBER                            
         BAS   RE,XMTALL           TRANSMIT PROCESSING SCREEN                   
*                                                                               
         CLI   MTPGM1LN,0          ANY NEW HEADERS TO ADD?                      
         BE    *+8                 NO                                           
         BAS   RE,FILLNEW          FILL PROCESS SCREEN W/NEW INV INFO           
         BAS   RE,FILLSTA          FILL PROCESSING SCREEN W/STATIONS            
*                                                                               
         MVC   RERROR(2),=AL2(PROSTA)                                           
         B     ERREND2                                                          
* !!!!   B     VRXIT                                                            
*                                                                               
VR15     LA    R2,MENFRSTH                                                      
         LA    R4,4                # OF STATIONS TO PROCESS AT A TIME           
*                                                                               
VR20     DS    0H                                                               
         CLI   5(R2),0             PROCESS THIS STATION?                        
         BE    VR30                YES                                          
         BAS   RE,NEXTFLD          NO - GET NEXT STATION                        
         BAS   RE,NEXTFLD                                                       
         B     VR20                                                             
*                                                                               
VR30     CLI   14(R2),0            ANY STATION HERE?                            
         BNE   VR40                                                             
         MVI   MTSCRN,X'FF'                                                     
         B     EXIT                                                             
*                                                                               
VR40     MVI   8(R2),C'*'          CURRENT STATION BEING PROCESSED              
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
         ST    R2,APROCSCR         SELECT FIELD ON PROC SCREEN                  
         BAS   RE,NEXTFLD                                                       
*                                                                               
         XC    STAHLD,STAHLD                                                    
         MVC   STAHLD(4),8(R2)     MOVE IN STATION                              
         MVI   STAHLD+4,C'T'       TV STATION                                   
         CLI   5(R2),6             STATION HAS SATELLITE?                       
         BNE   *+10                NO                                           
         MVC   STAHLD+4(1),13(R2)  MOVE IN SATELLITE                            
         OC    STAHLD,SPACES                                                    
*                                                                               
         BAS   RE,INVHEADR         GET INVENTORY HEADER                         
         BNE   VR100                                                            
*                                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         TM    MTFLAG,JUSTSRC     CHANGE ONLY SOURCE INV?                       
         BZ    VR45                                                             
         MVC   MTNEFFB,RINVKSTD     START DATE (BINARY)                         
         MVC   MTNEFFC,RINVPEFF    START DATE (COMPRESSED)                      
         MVC   PGMNEND,RINVPEFF+2   END DATE (COMPRESSED)                       
*                                                                               
         OC    MTEND,MTEND         CHANGE END DATE?                             
         BZ    *+10                                                             
         MVC   PGMNEND,MTEND                                                    
         B     VR75                                                             
*                                                                               
VR45     DS    0H                                                               
         TM    MTFLAG,SRCEFF       CHANGE SOURCE EFF?                           
         BO    VR60                                                             
*                                                                               
         TM    MTFLAG,JUSTTRAK    ADD ONLY TRACKS?                              
         BO    VR75                                                             
*                                                                               
         MVC   RINVPEFF+2(2),DTEHLD2+2   NEW END DATE COMPRESSED                
         MVI   ERROR,OVERLAP                                                    
*                                                                               
         CLC   RINVPEFF(2),RINVPEFF+2    START DATE CAN'T BE > END DATE         
         BNH   VR70                                                             
         B     ERREND                                                           
*                                                                               
VR60     DS    0H                  CHANGE SOURCE EFF DATE                       
         GOTO1 =A(OVFLRTN7),DMCB,(0,DUB),(RC),RR=RELO    (CHEFF)                
         B     VR100                                                            
*                                                                               
VR70     DS    0H                  CLOSE OUT RECORD W/ NEW END DATE             
         TM    RINVGPRO,X'10'      GLOBALLY PROTECTED?                          
         BO    VR74                YES - DON'T CHANGE ORIG HEADER               
         GOTO1 =A(OVFLRTN3),DMCB,(3,DUB),(RC),RR=RELO    (MYFILWRT)             
         B     VR75                                                             
*                                                                               
VR74     OI    MYFLAG3,GLBLPROT                                                 
*                                                                               
VR75     CLI   MTPGM1LN,0          ADD ANY NEW HEADERS?                         
         BE    VR100               NO                                           
*                                                                               
         MVC   PGMNINV,MTINV1      PROGRAM INVENTORY #                          
         MVC   PGMNOINV,MTOINV1    OLD FORMAT INV #OINV,MTINV1                  
         TM    RMPPROFS,X'80'      SYSTEM ASSIGNED INV #?                       
         BO    VR80                NO - USE REGULAR INV #                       
         EDIT  (B1,MTQTR1),(2,PGMNINV),FILL=0                                   
         MVC   PGMNINV+2(1),MTDCODE1                                            
         MVC   PGMNINV+3(1),MTLEN1                                              
*                                                                               
VR80     MVC   PGMNAME,MTPGM1      NAME OF 1ST PROGRAM                          
         MVC   PGMNAMLN,MTPGM1LN   L'NAME OF 1ST PROGRAM                        
         MVC   PGMNDAY,MTDAY1      PROGRAM DAY                                  
         MVC   PGMNTIME,MTTIME1    PROGRAM TIME                                 
         MVC   PGMNEFFB,MTNEFFB       NEW EFF START DATE (BINARY)               
         MVC   PGMNEFFC,MTNEFFC       NEW EFF START DATE (COMPRESSED)           
*                                                                               
         OC    MTEND2,MTEND2       END 1ST PROGRAM HEADER?                      
         BZ    *+10                                                             
         MVC   PGMNEND,MTEND2                                                   
*                                                                               
         TM    MTFLAG,JUSTTRAK    ADD ONLY TRACKS?                              
         BZ    VR81                                                             
*                                                                               
VR80A    DS    0H                                                               
         L     RE,AIO2                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     RF,AIO2             SAVE AWAY CURRENT REC IN AIO2                
         LA    R1,2000                                                          
         L     RE,AIO1                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         B     VR82                                                             
*                                                                               
VR81     BAS   RE,NEWINVH          GO ADD NEW INV HEADER                        
         MVC   MTDURAT1,TEMPDURA                                                
         XC    PGMNEND,PGMNEND                                                  
*                                                                               
         TM    MYFLAG,NOHEAD       DID WE ADD THE NEW HEADER?                   
         BZ    VR82                    YES                                      
         NI    MYFLAG,X'FF'-NOHEAD     NO                                       
*                                                                               
         TM    MYFLAG3,GLBLPROT    GLOBALLY PROTECTED?                          
         BO    VR85                                                             
         OI    MYFLAG2,NONEWHED        A NEW HEADER WASN'T ADDED                
         B     VR85                                                             
*                                                                               
VR82     OC    TRACKTAB(L'TRACKTAB),TRACKTAB                                    
         BZ    VR85                NEED TO PROCESS BACKTRACKS?                  
         OI    MYFLAG,PGM1         YES - ADDING PROGRAM 1 RECORDS               
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),RR=RELO      (BACKTRAK)            
*                                                                               
VR85     CLI   MTPGM2LN,0          ANY 2ND PROGRAM?                             
         BE    VR100               NO - PROCESS NEXT STATION                    
*                                                                               
         MVC   PGMNINV,MTINV2      PROGRAM INVENTORY #                          
         MVC   PGMNOINV,MTOINV2    OLD FORMAT INV #                             
         TM    RMPPROFS,X'80'      SYSTEM ASSIGNED INV #?                       
         BO    VR90                NO - USE REGULAR INV #                       
         EDIT  (B1,MTQTR2),(2,PGMNINV),FILL=0                                   
         MVC   PGMNINV+2(1),MTDCODE2                                            
         MVC   PGMNINV+3(1),MTLEN2                                              
*                                                                               
VR90     MVC   PGMNAME,MTPGM2      NAME OF 1ST PROGRAM                          
         MVC   PGMNAMLN,MTPGM2LN   L'NAME OF 1ST PROGRAM                        
         MVC   PGMNDAY,MTDAY2      PROGRAM DAY                                  
         MVC   PGMNTIME,MTTIME2    PROGRAM TIME                                 
         MVC   PGMNEFFB,MTNEFF2B   NEW EFF START DATE (BINARY)                  
         MVC   PGMNEFFC,MTNEFF2C   NEW EFF START DATE (COMPRESSED)              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),ORIGKEY     RESTORE ORIGINAL KEY                         
*                                                                               
         OI    MYFLAG,ADDPGM2      ADDING PGM 2 HEADER                          
         BAS   RE,NEWINVH          GO ADD NEW INV HEADER                        
         MVC   MTDURAT2,TEMPDURA                                                
         NI    MYFLAG,X'FF'-ADDPGM2                                             
*                                                                               
         TM    MYFLAG,NOHEAD       DID WE ADD THE NEW HEADER?                   
         BZ    VR95                    YES                                      
         NI    MYFLAG,X'FF'-NOHEAD     NO                                       
*                                                                               
         TM    MYFLAG3,GLBLPROT    GLOBALLY PROTECTED?                          
         BO    VR100                                                            
         OI    MYFLAG2,NONEWHED        A NEW HEADER WASN'T ADDED                
         B     VR100                                                            
*                                                                               
VR95     OC    TRACKTAB(L'TRACKTAB),TRACKTAB                                    
         BZ    VR100               NEED TO PROCESS BACKTRACKS?                  
         OI    MYFLAG,PGM2         YES - ADDING PROGRAM 2 RECORDS               
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),RR=RELO      (BACKTRAK)            
*                                                                               
VR100    DS    0H                                                               
         L     RF,APROCSCR         SELECT FIELD ON PROC SCREEN                  
         MVI   8(RF),C'/'          PROCESSED STATION SUCCESSFULLY               
*                                                                               
         TM    MYFLAG2,NONEWHED    ALL NEW HEADERS ADDED SUCESSFULLY?           
         BZ    *+12                                                             
         MVI   8(RF),C'H'          NO, A NEW HEADER WASN'T ADDED                
         NI    MYFLAG2,X'FF'-NONEWHED                                           
*                                                                               
         TM    MYFLAG3,NOTCLOSE    PREVIOUS HEADER NOT CLOSED OUT?              
         BZ    *+12                                                             
         MVI   8(RF),C'H'                                                       
         NI    MYFLAG3,X'FF'-NOTCLOSE                                           
*                                                                               
         TM    MYFLAG3,GLBLPROT    GLOBALLY PROTECTED?                          
         BZ    *+12                                                             
         MVI   8(RF),C'G'          GLOBALLY PROTECTED                           
         NI    MYFLAG3,X'FF'-GLBLPROT                                           
*                                                                               
         TM    MYFLAG2,BADTRACK    SKIPPED A BAD TRACK?                         
         BZ    *+8                 NO                                           
         MVI   8(RF),C'T'          SKIPPED A BAD TRACK                          
*                                                                               
         OI    6(RF),X'80'                                                      
         BAS   RE,NEXTFLD                                                       
         BCT   R4,VR20             GO PROCESS NEXT STATION                      
         TM    MTFLAG,SRCEFF                                                    
         BZ    *+10                                                             
         MVC   TWAKEYSV,RELOKEY                                                 
*                                                                               
         MVC   RERROR(2),=AL2(ENTCONT)                                          
         B     ERREND2                                                          
*                                                                               
VRXIT    DS    0H                                                               
         CLI   MTSCRN,X'FF'        FINISHED PROCESSING ALL STATIONS?            
         BNE   *+8                                                              
         BAS   RE,GOLTRANS         YES - ISSUE LTRANS                           
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*                CLEAR KEY FIELDS                              *                
****************************************************************                
CLRKEY   NTR1                                                                   
         XC    GL1SSTA,GL1SSTA                                                  
         OI    GL1SSTAH+6,X'80'                                                 
         XC    GL1INV,GL1INV                                                    
         OI    GL1INVH+6,X'80'                                                  
         XC    GL1EFF,GL1EFF                                                    
         OI    GL1EFFH+6,X'80'                                                  
CLRKEYX  DS    0H                                                               
         B     EXIT                                                             
****************************************************************                
*        GET END DATE FOR 1ST PROGRAM                          *                
****************************************************************                
ENDDATE2 NTR1                                                                   
         LA    R2,GLBNEF2H                                                      
         MVC   RERROR,=AL2(CLOSE7)                                              
         CLI   WORK+1,1            SHOULD ONLY BE ONE DIGIT                     
         BNE   ERREND2                                                          
         CLI   WORK+22,C'1'        MUST BE BETWEEN 1 AND 7                      
         BL    ERREND2                                                          
         CLI   WORK+22,C'7'                                                     
         BH    ERREND2                                                          
*                                                                               
         XC    NUMDAYS,NUMDAYS                                                  
         MVC   NUMDAYS,WORK+22   # DAYS TO CLOSE PREVIOUS RECORD                
*                                                                               
         LA    RE,GLBNEF2          DATE TO SUBTRACT FROM FOR END DATE           
         ST    RE,DMCB                                                          
         MVC   DMCB(1),WORK        L'INPUT DATE                                 
         LA    RE,WORK2                                                         
         ST    RE,DMCB+4                                                        
         OI    DMCB+4,X'40'                                                     
*                                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 PERVAL,DMCB                                                      
         CLI   DMCB+4,X'04'        VALID SINGLE DATE INPUT?                     
         BNE   ERREND                                                           
*                                                                               
         LA    R3,WORK2                                                         
         USING PERVALD,R3                                                       
*                                                                               
         MVC   NEWEFF(8),PVALCPER      NEW EFFECTIVE START DATE                 
         MVC   NEWEFF+8(2),=C'(-'                                               
         MVC   NEWEFF+10(1),NUMDAYS                                             
         MVI   NEWEFF+11,C')'                                                   
*                                                                               
         LA    RE,NEWEFF           DATE TO SUBTRACT FROM FOR END DATE           
         ST    RE,DMCB                                                          
         MVI   DMCB,L'NEWEFF       L'INPUT DATE MMMDD/YY(-N)                    
         LA    RE,WORK2                                                         
         ST    RE,DMCB+4                                                        
         OI    DMCB+4,X'40'                                                     
*                                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 PERVAL,DMCB                                                      
         CLI   DMCB+4,X'04'        VALID SINGLE DATE INPUT?                     
         BNE   ERREND                                                           
*        MVC   DTEHLD2+2(2),PVALCSTA     END DATE COMPRESSED                    
         MVC   MTEND2,PVALCSTA           NEW END DATE COMPRESSED                
*                                                                               
         MVI   ERROR,OVERLAP                                                    
         CLC   MTEND2,MTNEFFC       1ST PGM END < 1ST PGM START?                
         BL    ERREND                                                           
*                                                                               
ENDD2X   DS    0H                                                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
****************************************************************                
*        FILL PROCESSING SCREEN W/ NEW INV INFO                *                
****************************************************************                
FILLNEW  NTR1                                                                   
         XC    MENNPG1,MENNPG1                                                  
         ZIC   RF,MTPGM1LN         L'1ST PROGRAM                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MENNPG1(0),MTPGM1   PRINT 1ST PROGRAM                            
         OI    MENNPG1H+6,X'80'                                                 
         MVC   MENNIN1,MTINV1      MOVE IN 1ST INV #                            
         OI    MENNIN1H+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(3,MTNEFFB),(5,MENNEF1)                              
         OI    MENNEF1H+6,X'80'                                                 
*                                                                               
         CLI   MTPGM2LN,0          ANY 2ND PROGRAM                              
         BE    FILLNEWX            NO                                           
         XC    MENNPG2,MENNPG2                                                  
         ZIC   RF,MTPGM2LN         L'2ND PROGRAM                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MENNPG2(0),MTPGM2   PRINT 2ND PROGRAM                            
         OI    MENNPG2H+6,X'80'                                                 
         MVC   MENNIN2,MTINV2      MOVE IN 2ND INV #                            
         OI    MENNIN2H+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(3,MTNEFF2B),(5,MENNEF2)                             
         OI    MENNEF2H+6,X'80'                                                 
*                                                                               
FILLNEWX B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*        FILL PROCESSING SCREEN W/ STATIONS                    *                
****************************************************************                
FILLSTA  NTR1                                                                   
         LA    R2,MENFRSTH         FIRST STATION SELECT FIELD                   
         LA    R3,MENUTAB          TABLE OF SET IDENTIFIERS                     
         NI    MYFLAG2,X'FF'-GOTFIRST                                           
*                                                                               
FILL5    DS    0H                                                               
         CLI   0(R3),0             ANY STATION HERE?                            
         BE    FILLX               NO - FINISHED ALL STATIONS - EXIT            
*                                                                               
         TM    MYFLAG2,GOTFIRST    ALREADY GOT FIRST STATION                    
         BO    FILL7                                                            
*                                                                               
         CLC   SV1STA,0(R3)        IS THIS 1ST VALID HEADER STATION?            
         BE    FILL6                                                            
         MVI   8(R2),C'*'          DIDN'T ADD ANYTHING FOR THIS STA             
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'                                                      
         B     FILL7                                                            
*                                                                               
FILL6    OI    MYFLAG2,GOTFIRST    GOT FIRST VALID HEADER                       
*                                                                               
FILL7    DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               MOVE IN STATION                              
*                                                                               
         MVI   5(R2),4             L'STATION FIELD                              
         CLI   4(R3),C'T'          TV STATION?                                  
         BE    FILL10              YES - NO SATELLITE                           
         MVI   5(R2),6             L'STATION FIELD                              
         MVI   12(R2),C'-'                                                      
         MVC   13(1,R2),4(R3)      MOVE IN SATELLITE                            
*                                                                               
FILL10   MVC   8(4,R2),0(R3)       MOVE IN STATION                              
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         LA    R3,5(R3)            NEXT ENTRY IN MENUTAB                        
         B     FILL5                                                            
*                                                                               
FILLX    B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*                GET NEW INVENTORY #'S                         *                
****************************************************************                
GETNINV  NTR1                                                                   
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
         MVI   ERROR,INVALID                                                    
*                                                                               
         LA    R2,GLBNINVH         NEW INVENTORY # FOR PGM 1                    
         CLI   5(R2),0                                                          
         BNE   GINV10                                                           
         MVC   MTINV1,RINVKINV     SAME AS PREVIOUS                             
         B     GINV20                                                           
*                                                                               
GINV10   DS    0H                                                               
         CLI   5(R2),4             MUST BE 4 DIGITS                             
         BNE   ERREND                                                           
         MVC   MTINV1,8(R2)                                                     
*                                                                               
GINV20   DS    0H                                                               
         CLI   MTPGM2LN,0          ANY 2ND PROGRAM?                             
         BE    GETINVX             NO - EXIT                                    
         LA    R2,GLBNIN2H         NEW INVENTORY # FOR PGM 2                    
         CLI   5(R2),0                                                          
         BNE   GINV30                                                           
         MVC   MTINV2,RINVKINV     SAME AS PREVIOUS                             
         B     GETINVX                                                          
*                                                                               
GINV30   DS    0H                                                               
         CLI   8(R2),C'='          SAME AS 1ST PROGRAM?                         
         BNE   GINV35                                                           
         MVC   MTINV2,MTINV1                                                    
         B     GETINVX                                                          
*                                                                               
GINV35   CLI   5(R2),4             MUST BE 4 DIGITS                             
         BNE   ERREND                                                           
         MVC   MTINV2,8(R2)                                                     
*                                                                               
GETINVX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                ISSUE LTRANS REQUEST                                           
***********************************************************************         
GOLTRANS NTR1                                                                   
         LA    R6,MENUTAB          TABLE OF IDENTIFIERS                         
*                                                                               
GT10     DS    0H                                                               
         OC    0(5,R6),0(R6)       ANY IDENTIFIER HERE?                         
         BZ    GOLTRANX            NO - EXIT                                    
         MVC   CSTAT,0(R6)                                                      
*                                                                               
         CLI   CSTAT+4,C'T'        TELEVISION?                                  
         BNE   *+8                                                              
         MVI   CSTAT+4,C' '        MOVE IN SPACE FOR LTRANS                     
*                                                                               
         NI    DMINBTS,X'FF'-X'80' TURN OFF DELETED REC READ                    
         GOTO1 VLTRANS             YES- ISSUE LTRANS REQUEST                    
*                                                                               
         LA    R6,5(R6)            GET NEXT IDENTIFIER                          
         B     GT10                                                             
*                                                                               
GOLTRANX B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*       GET NEXT INV HEADER FROM SET IDENTIFIERS               *                
****************************************************************                
INVHEADR NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   CCONKSTA,STAHLD                                                  
         MVC   CCONINV,INVHLD                                                   
         MVC   CCONEFF,GL1EFF                                                   
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,DTEHLD                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         MVC   AIO,AIO1                                                         
         LA    R2,GL1SSTAH                                                      
*!!!!    GOTO1 GETINV              GET NEXT INVENTORY HEADER                    
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVC   KEYSAVE(27),SAVEKEY     FOR OVKEYCHK                             
         CR    RB,RB                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE     FOUND IT?                                    
         BNE   INVH20              YES                                          
         GOTO1 GETREC                                                           
         B     INVHEADX                                                         
*                                                                               
INVH20   DS    0H                                                               
         OI    MYFLAG2,NONEWHED                                                 
         LTR   RB,RB                                                            
*                                                                               
INVHEADX B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*              CREATE NEW PASSIVE POINTER                                       
****************************************************************                
*              PARAM 1   BYTES 1-3 A(INVENTORY RECORD)                          
*              PARAM 2   BYTES 1-3 A(200 BYTE OUTPUT AREA)                      
*                                                                               
INVPTR   NTR1                                                                   
         USING RINVREC,R2                                                       
         USING RIDPKEY,R4                                                       
*                                                                               
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
         XC    0(200,R4),0(R4)                                                  
         LA    R6,6                                                             
         LA    R3,RINVDP                                                        
*                                                                               
INVPTR1  DS    0H                                                               
         MVI   RIDPKTYP,X'92'                                                   
         MVC   RIDPKREP,RINVKREP                                                
         MVC   RIDPKSTA,RINVKSTA                                                
         MVC   RIDPKDPT,0(R3)                                                   
         MVC   RIDPKINV,RINVKINV                                                
         MVC   RIDPKSTD,RINVKSTD                                                
*                                                                               
*                                                                               
*  IF SELF ASSIGNED GET NEXT DAYPART                                            
*  ONLY COMPUTER GENERATED NUMBERS GET THE DAY,QTR HOUR                         
*  AND THE LENGTH FILLED IN.                                                    
*                                                                               
         TM    RINVSTAT,X'80'                                                   
         BO    INVPTR20            BIT ON SELF ASSIGNED                         
*                                                                               
         MVC   RIDPKDAY,RINVOINV+1   MOVE DAY CODE,                             
         MVC   RIDPKQTR,RINVOINV     QUARTER HOUR,                              
         MVC   RIDPKLEN,RINVOINV+2   AND PROGRAM LENGTH TO KEY                  
*                                                                               
         LA    RE,EFFDAT           SPECIAL DAYPARTS                             
INVPTR10 DS    0H                                                               
         CLI   0(RE),X'FF'                                                      
         BE    INVPTR20                                                         
         CLC   0(1,R3),0(RE)                                                    
         BE    INVPTR15                                                         
         LA    RE,1(RE)                                                         
         B     INVPTR10                                                         
*                                                                               
INVPTR15 XC    RIDPKDAY,RIDPKDAY                                                
         MVC   RIDPKDTE,RINVPEFF                                                
*                                                                               
INVPTR20 LA    R3,1(R3)            NEXT DAYPART CODE                            
         CLI   0(R3),X'40'                                                      
         BNH   INVPTX                                                           
         LA    R4,32(R4)                                                        
         BCT   R6,INVPTR1          DO NEXT POINTER                              
*                                                                               
INVPTX   B     EXIT                                                             
         DROP  R2,R4                                                            
*                                                                               
*  THESE DAYPARTS GET A DAY CODE, QUARTER HOUR, AND PROGRAM LENGTH              
DAYCOD   DC    C'MDKNPOUXYWZ',X'FF'                                             
*                                                                               
*  THESE DAYPARTS GET EFFECTIVE DATE, QUARTER HOUR, AND PROGRAM LENGTH          
EFFDAT   DC    C'VSJ',X'FF'                                                     
         EJECT                                                                  
****************************************************************                
*                ADD NEW PASSIVE POINTER                       *                
****************************************************************                
*              PARAM 1   BYTES 1-3 A(LIST OF POINTERS)                          
*                                                                               
NWPT     NTR1                                                                   
         L     R2,0(R1)                                                         
NWPT1    CLI   0(R2),0                                                          
         BE    NWPTX               END OF LIST                                  
         MVC   KEY(27),0(R2)                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(27),KEY                                                  
         BE    NWPT3                                                            
         MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
*        BAS   RE,MYDIRADD                                                      
         GOTO1 =A(OVFLRTN3),DMCB,(5,DUB),(RC),RR=RELO    (MYDIRADD)             
         B     NWPT4                                                            
*                                                                               
NWPT3    MVC   KEY(28),0(R2)                                                    
         MVC   KEY+28(4),BSVDA                                                  
*        BAS   RE,MYDIRWRT                                                      
         GOTO1 =A(OVFLRTN3),DMCB,(4,DUB),(RC),RR=RELO    (MYDIRWRT)             
*                                                                               
NWPT4    LA    R2,32(R2)                                                        
         B     NWPT1                                                            
*                                                                               
NWPTX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
****************************************************************                
*                GO ADD NEW INV HEADER                         *                
****************************************************************                
NEWINVH  NTR1                                                                   
         NI    MYFLAG,X'FF'-NOHEAD                                              
*                                                                               
         XC    TEMPDURA,TEMPDURA                                                
         XC    ORIGKEY,ORIGKEY                                                  
         MVC   ORIGKEY,KEY                                                      
         MVC   AIO,AIO2                                                         
         GOTO1 GETINV                                                           
*                                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         TM    RINVGPRO,X'10'      GLOBALLY PROTECTED?                          
         BZ    *+16                NO                                           
         OI    MYFLAG,NOHEAD       YES - DON'T ADD                              
         OI    MYFLAG3,GLBLPROT                                                 
         B     NEWINVX                                                          
*                                                                               
         XC    RINVKSTD,RINVKSTD                                                
         XC    RINVPEFF,RINVPEFF                                                
         MVC   RINVKSTD,PGMNEFFB      NEW EFF START DATE (BINARY)               
         MVC   RINVPEFF(2),PGMNEFFC   NEW EFF START DATE (COMPRESSED)           
         MVC   RINVKINV,PGMNINV       PROGRAM INVENTORY #                       
         MVC   RINVOINV,PGMNOINV      OLD FORMAT INV #                          
*                                                                               
         OC    PGMNEND,PGMNEND     END 1ST PROGRAM HEADER?                      
         BZ    *+10                                                             
         MVC   RINVPEFF+2(2),PGMNEND                                            
*                                                                               
         MVI   ELCODE,X'03'        PROGRAM NAME ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               SHOULD BE THERE                              
         DROP  R6                                                               
*                                                                               
* REMOVE X'03' PROGRAM ELEMENT                                                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'03',AIO),0                          
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R3,WORK             BUILD NEW X'03' ELEMENT                      
         USING RIPGELEM,R3                                                      
*                                                                               
         MVI   RIPGCODE,X'03'      ELEMENT CODE                                 
         ZIC   RF,PGMNAMLN         L'PROGRAM NAME                               
         LA    RF,2(RF)            ADD 2 FOR OVERHEAD BYTES                     
         STC   RF,RIPGLEN                                                       
         SH    RF,=H'3'            SUBTRACT OVERHEAD + 1 FOR EX                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RIPGNAME(0),PGMNAME     PROGRAM NAME                             
*                                                                               
* ADD NEW X'03' PROGRAM ELEMENT                                                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R3),=C'ADD=CODE'              
*                                                                               
NI50     DS    0H                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   ELCODE,X'02'        DAY TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               SHOULD BE THERE                              
         DROP  R6                                                               
*                                                                               
* REMOVE X'02' PROGRAM ELEMENT                                                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',REPFILE),(X'02',AIO),0                          
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R3,WORK             BUILD NEW X'02' ELEMENT                      
         USING RIDTELEM,R3                                                      
*                                                                               
         MVI   RIDTCODE,X'02'      ELEMENT ID                                   
         MVI   RIDTLEN,X'07'       ELEMENT LENGTH                               
         MVC   RIDTDAY,PGMNDAY     PROGRAM DAY                                  
         MVC   RIDTTIME,PGMNTIME   PROGRAM TIME                                 
*                                                                               
* ADD NEW X'02' PROGRAM ELEMENT                                                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),(R3),=C'ADD=CODE'              
*                                                                               
NI55     DS    0H                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   ELCODE,X'EF'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               SHOULD BE THERE                              
         DROP  R6                                                               
*                                                                               
         USING RINVAEL,R6                                                       
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         OI    RINVAFLG,AEGLOBAL   CAME FROM GLOBAL/CHANGE                      
         DROP  R6                                                               
*                                                                               
NI58     L     R6,AIO                                                           
         USING REINVREC,R6                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(27),0(R6)                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     RECORD ALREADY EXISTS?                       
         BNE   NI59                NO                                           
*                                                                               
         TM    MTFLAG,JUSTSRC      CHANGE SOURCE INV?                           
         BO    NI70                                                             
*                                                                               
         OI    MYFLAG,NOHEAD       YES - DON'T ADD                              
         B     NEWINVX                                                          
*                                                                               
NI59     DS    0H                                                               
         MVC   SAVEKEY,KEYSAVE                                                  
*                                                                               
         CLC   PGMNINV,GL1INV      DIFFERENT INV #'S                            
         BE    NI60                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(21),0(R6)       ARE THERE ANY OTHER HEADER'S THERE?          
         GOTO1 HIGH                                                             
         CLC   KEY(21),KEYSAVE                                                  
         BNE   NI70                NO - JUST GO ADD                             
*                                                                               
NI60     XC    KEY,KEY                                                          
         MVC   KEY(27),0(R6)                                                    
         MVC   AIO,AIO3                                                         
         GOTO1 GETINV                                                           
         L     R6,AIO                                                           
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2     LAST RECORD CLOSED OUT?             
         BNZ   *+12                                                             
         OI    MYFLAG3,NOTCLOSE             YES - NOT CLOSED OUT                
         B     NEWINVX                                                          
*                                                                               
         MVI   ERROR,OVERLAP                                                    
         TM    MYFLAG,ADDPGM2          ADDING 2ND PGM HEADER?                   
         BZ    NI65                                                             
         CLC   MTNEFF2C,RINVPEFF+2     OVERLAPPING EFF. DATES? (PGM2)           
         BL    ERREND                                                           
         B     NI70                                                             
*                                                                               
NI65     CLC   MTNEFFC,RINVPEFF+2      OVERLAPPING EFF. DATES? (PGM1)           
         BL    ERREND                                                           
*                                                                               
NI70     MVC   AIO,AIO2                                                         
         MVC   KEYSAVE(27),SAVEKEY                                              
*                                                                               
         TM    MTFLAG,JUSTSRC      CHANGE SOURCE INV?                           
         BZ    NI75                                                             
         GOTO1 =A(OVFLRTN3),DMCB,(3,DUB),(RC),RR=RELO    (OVFILWRT)             
         B     NEWINVX                                                          
*                                                                               
NI75     DS    0H                                                               
         GOTO1 =A(OVFLRTN3),DMCB,(2,DUB),(RC),RR=RELO    (OVFILADD)             
*                                                                               
         GOTO1 INVPTR,DMCB,AIO,WORK2      ADD PASSIVE POINTERS                  
         GOTO1 NWPT,DMCB,WORK2                                                  
*                                                                               
NEWINVX  DS    0H                                                               
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(27),ORIGKEY                                                  
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
****************************************************************                
*           TRANSMIT NEW OVERLAY SCREEN                        *                
****************************************************************                
XMTALL   NTR1                                                                   
         LA    RE,64(RA)                                                        
XMTALL2  OI    6(RE),X'80'                                                      
         ZIC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   XMTALL2                                                          
         B     EXIT                                                             
****************************************************************                
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
XIT      XIT1  REGS=(R0,R1)                                                     
*                                                                               
ERREND   DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 ERREX                                                            
ERREND2  DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 MYERROR                                                          
*                                                                               
ERR2027  DS    0H                                                               
         MVC   RERROR(2),=AL2(AFT2027)                                          
         LA    R2,GLBNEFFH                                                      
         GOTO1 MYERROR                                                          
*                                                                               
RELO     DS    A                                                                
REPFILE  DC    CL8'REPFILE'                                                     
*                                                                               
*        EQUATES                                                                
*                                                                               
MISINP   EQU   1                                                                
PRO      EQU   X'01'                                                            
INV      EQU   X'02'                                                            
TP       EQU   X'04'               READ TIME PERIOD FILE                        
MIX      EQU   X'08'               READ FROM PAV AND TIME PERIOD                
COMMA    EQU   C','                                                             
*                                                                               
*  BUMP TO NEXT SCREEN FIELD                                                    
NEXTFLD  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
*****************************************************************               
*                 OVERFLOW ROUTINES                             *               
*****************************************************************               
         DS    0F                                                               
         DROP  R9,RB                                                            
OVFLRTN  NMOD1 0,*RM10OV*,RR=R5                                                 
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING OVFLRTN+4096,RA                                                  
         L     RC,4(R1)                                                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R5,RELO2                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRANCH(RF)                                                     
*                                                                               
OVBRANCH B     BACKTRAK                                                         
****************************************************************                
*              PROCESS BACK TRACKS                             *                
*          AIO1 = ORIGINAL HEADER RECORD                       *                
*          AIO2 = NEW HEADER ADDED                             *                
****************************************************************                
BACKTRAK DS    0H                                                               
         GOTO1 =A(OVFLRTN2),DMCB,(0,DUB),(RC),RR=RELO2     (DYTIMSET)           
*                                                                               
         MVI   ERROR,INVALID                                                    
         MVC   DEMSTA,STAHLD       SAVE AWAY STATION                            
*                                                                               
         NI    MYFLAG2,X'FF'-BADTRACK                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R4,AIO2             NEW HEADER THAT WAS ADDED                    
         USING REINVREC,R4                                                      
         MVC   TRSVKEY,0(R4)       SAVE AWAY HEADER'S KEY                       
*                                                                               
         LA    R5,TEMPWORK         WORKING STORAGE FOR TRANSFER DATA            
         ST    R5,ATRANS                                                        
         LA    RF,TRBKLIST                                                      
         ST    RF,ATRBKLST                                                      
*                                                                               
         LA    R3,TRACKTAB                                                      
         USING TTABD,R3                                                         
*                                                                               
BK10     DS    0H                                                               
         OC    0(L'TRACKTAB,R3),0(R3)    ANY TRACKS TO PROCESS HERE?            
         BZ    BACKTRX                   NO - EXIT                              
*                                                                               
         CLC   =C'TEXT',TTBOOKS    JUST COPY TEXT RECORDS?                      
         BE    BK30                                                             
*                                                                               
         XC    TEMPWORK,TEMPWORK   BUILD DUMMY HEADER FOR BOOKS FIELD           
         LA    R2,TEMPWORK                                                      
         MVI   0(R2),X'19'         8 FOR HEADER, 17 FOR BOOKS FIELD             
         MVC   5(1,R2),TTBOOKLN    L'INPUT                                      
         MVC   8(L'TTBOOKS,R2),TTBOOKS                                          
*                                                                               
         XC    TRBKLIST,TRBKLIST                                                
         GOTO1 VREBKLST,DMCB,(R2),(C'B',TRBKLIST),BOOKVAL,SCANNER               
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BE    OVERRND                                                          
         MVC   TRBKCNT,DMCB        NUMBER OF BOOK ENTRIES                       
*                                                                               
BK30     DS    0H                                                               
         TM    MYFLAG,PGM2         ADDING PROGRAM 2 RECORDS?                    
         BO    BK40                YES                                          
*                                                                               
         CLC   TTPCODE1,=C'NA'     DON'T ADD THIS TRACK?                        
         BE    BK500               YES - DON'T ADD THIS ONE                     
         CLC   TTPCODE1,=C'CP'     COPY?                                        
         BNE   BK35                                                             
*                                                                               
         CLC   =C'TEXT',TTBOOKS    COPY JUST TEXT RECORDS?                      
         BE    BK34A                                                            
*                                                                               
         ZIC   R2,TRBKCNT          COUNT OF ENTRIES INBOOK LIST                 
         LA    R5,TRBKLIST         R5 POINTS TO ENTRY                           
*                                                                               
BK32     DS    0H                                                               
         ST    R5,ATRBKLST                                                      
         GOTO1 =A(OVFLRTN4),DMCB,(2,DUB),(RC),RR=RELO2     (COPYTRAK)           
         L     R5,ATRBKLST                                                      
*                                                                               
BK34     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),TEMPKEY     RESTORE SOURCE HEADER KEY                    
         LA    R5,OUTLN(R5)        GET NEXT TRACK                               
         BCT   R2,BK32                                                          
         B     BK500                                                            
*                                                                               
BK34A    GOTO1 =A(OVFLRTN4),DMCB,(3,DUB),(RC),RR=RELO2     (ALLTEXT)            
         B     BK500                                                            
*                                                                               
BK35     MVC   INVCODE,TTPCODE1    USE PROGRAM CODE 1                           
         MVC   INVCDCTL,TTPCDE1B                                                
         B     BK50                                                             
*                                                                               
BK40     DS    0H                  MUST BE ADDING PROGRAM 2 RECORDS             
         CLC   TTPCODE2,=C'NA'     DON'T ADD THIS TRACK?                        
         BE    BK500               YES - DON'T ADD THIS ONE                     
         CLC   TTPCODE2,=C'CP'     COPY?                                        
         BNE   BK45                                                             
*                                                                               
         CLC   =C'TEXT',TTBOOKS    COPY JUST TEXT RECORDS?                      
         BE    BK43                                                             
*                                                                               
         ZIC   R2,TRBKCNT          COUNT OF ENTRIES INBOOK LIST                 
         LA    R5,TRBKLIST         R5 POINTS TO ENTRY                           
*                                                                               
BK41     DS    0H                                                               
         ST    R5,ATRBKLST                                                      
         GOTO1 =A(OVFLRTN4),DMCB,(2,DUB),(RC),RR=RELO2     (COPYTRAK)           
         L     R5,ATRBKLST                                                      
*                                                                               
BK42     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),TEMPKEY     RESTORE SOURCE HEADER KEY                    
         LA    R5,OUTLN(R5)        GET NEXT TRACK                               
         BCT   R2,BK41                                                          
         B     BK500                                                            
*                                                                               
BK43     GOTO1 =A(OVFLRTN4),DMCB,(3,DUB),(RC),RR=RELO2     (ALLTEXT)            
         B     BK500                                                            
*                                                                               
BK45     MVC   INVCODE,TTPCODE2    USE PROGRAM CODE 2                           
         MVC   INVCDCTL,TTPCDE2B                                                
*                                                                               
BK50     DS    0H                                                               
         MVI   INVTYP,C'P'         DEFAULT - FROM DEMO FILES                    
         MVI   INVNO,1             BUILD A DUMMY ENTRY                          
         MVC   INVPRG#,TTSRCEPG    PUT PROGRAM# IN SHARED 'DEM' BLOCK           
         OC    TTSRCEPG,TTSRCEPG   ANY BK/PGM ENTERED?                          
         BNZ   BK80                YES                                          
*                                                                               
BK60     DS    0H                                                               
         L     R5,AIO3                                                          
         ST    R5,INVLIST                                                       
         L     R2,ATRANS           TRANSFER DATA STORAGE                        
         XC    0(250,R2),0(R2)     ALSO BUILD A DUMMY HEADER AND                
         MVI   0(R2),250           DATA W/ DEFAULT DETAILS                      
         LA    R6,8(R2)            POINT R6 AT DATA START                       
*                                                                               
         USING INVLD,R5                                                         
         XC    INVLREC,INVLREC     HAS NOT BEEN INPUT BY USER                   
         MVI   INVLWT,1                                                         
         MVC   INVLFLE,INVTYP      TAKE FILE FROM TYPE                          
         CLI   INVLFLE,C'I'        TEST FOR INVENTORY                           
         BE    BK70                                                             
*                                                                               
         OI    INVLTYP,X'40'       THIS ENTRY IS THE 1ST DAY/TIME               
         LA    RF,DYTIMTAB         DAY/TIMES ARE STORED IN THIS TABLE           
*                                                                               
BK64     DS    0H                                                               
         MVC   INVLDAY,0(RF)       DAY                                          
         MVC   INVLSTIM(4),1(RF)   START & END TIMES                            
         MVC   SVSETIM,INVLSTIM    SAVE AWAY START & END TIMES                  
         CLI   5(RF),0             ANY MORE DAY/TIME IN TABLE?                  
         BE    BK68                NOPE                                         
*                                                                               
         MVC   (INVLREC+L'INVLREC)(L'INVLREC),INVLREC                           
         NI    INVLTYP+L'INVLREC,X'FF'-X'60'                                    
         LA    R5,L'INVLREC(R5)                                                 
         LA    RF,5(RF)                                                         
         B     BK64                                                             
*                                                                               
BK68     DS    0H                                                               
         OI    INVLTYP,X'20'       THIS ENTRY IS THE LAST DAY/TIME              
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',OVREP),(X'02',AIO2),0                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     RE,12(R1)                                                        
*                                                                               
         MVC   INVDAYS(5),2(RE)    DAY TIME                                     
*                                                                               
         GOTO1 UNDAY,DMCB,INVDAYS,(R6)                                          
         CLI   0(R6),C' '          TEST FOR END OF DAY EXPRESSION               
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         MVI   0(R6),COMMA         INSERT COMMA AFTER IT                        
         LA    R6,1(R6)                                                         
*                                                                               
         GOTO1 UNTIME,DMCB,INVTIM,(R6)                                          
         CLI   0(R6),C' '          FIND END OF TIME EXPRESSION                  
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         OC    INVTIM+2(2),INVTIM+2      TEST FOR BREAK CODE                    
         BNZ   *+14                                                             
         MVC   0(2,R6),=C',B'                                                   
         LA    R6,2(R6)            BUMP OUTPUT POINTER                          
         LA    R1,8(R2)                                                         
         SR    R6,R1               FIND LENGTH OF DATA                          
         STC   R6,5(R2)                                                         
         B     BK150                                                            
*                                                                               
BK70     DS    0H                                                               
         MVI   INVLTYP,X'80'                                                    
         LA    RE,TRSVKEY                                                       
         MVC   INVLNUMB,RINVKINV-RINVKEY(RE)                                    
         MVC   INVLDATE,RINVKSTD-RINVKEY(RE)                                    
         ZIC   R1,INVLNUMB         QUARTER HOUR                                 
         CVD   R1,DUB                                                           
         UNPK  0(2,R6),DUB+6(2)                                                 
         OI    1(R6),X'F0'                                                      
         MVC   2(1,R6),INVLNUMB+1  DAY CODE                                     
         LA    R6,3(R6)                                                         
         CLI   INVLNUMB+2,C'0'     TEST FOR LENGTH OR SPECIAL CODE              
         BE    *+14                                                             
         MVC   0(1,R6),INVLNUMB+2                                               
         LA    R6,1(R6)                                                         
*                                                                               
         MVI   0(R6),COMMA         COMMA AFTER INVENTORY NUMBER                 
         LA    R6,1(R6)                                                         
         GOTO1 DATCON,DMCB,(3,INVLDATE),(5,(R6))                                
         LA    R6,8(R6)                                                         
         LA    R1,8(R2)                                                         
         SR    R6,R1               FIND DATA LENGTH                             
         STC   R6,5(R2)                                                         
         B     BK150                                                            
         DROP  R5                                                               
*                                                                               
BK80     DS    0H                                                               
         OC    TTSRCEBK,TTSRCEBK   ANY SOURCE BOOK ENTERED?                     
         BNZ   BK85                                                             
         L     RF,ATRBKLST         NO SOURCE BOOK - USE CURR BOOK               
         MVC   SRCEBK,1(RF)        SOURCE BOOK = CURRENT BOOK                   
         LA    RF,8(RF)            POINT TO NEXT BOOK                           
         ST    RF,ATRBKLST                                                      
         B     *+10                                                             
*                                                                               
BK85     MVC   SRCEBK,TTSRCEBK+1   SOURCE BOOK                                  
         MVC   SRCEPGM,TTSRCEPG    SOURCE PROGRAM NUMBER                        
         GOTO1 =A(OVFLRTN2),DMCB,(2,DUB),(RC),RR=RELO2     (INITDBLK)           
*                                                                               
         MVC   PGMNDURA,MTDURAT1                                                
         TM    MYFLAG,PGM2         ADDING PGM2 RECORDS?                         
         BZ    *+10                                                             
         MVC   PGMNDURA,MTDURAT2                                                
*                                                                               
         GOTO1 =A(OVFLRTN2),DMCB,(3,DUB),(RC),RR=RELO2     (GODEMAND)           
*                                                                               
         TM    MYFLAG,GOTDEM       GOT A MATCH FROM DEMAND?                     
         BNZ   BK90                YES                                          
         OI    MYFLAG2,BADTRACK                                                 
         B     BK500               NO - TRY NEXT LINE                           
*                                                                               
BK90     L     R5,AIO3                                                          
         USING INVLD,R5                                                         
         ST    R5,INVLIST                                                       
*                                                                               
         XC    INVLREC,INVLREC     HAS NOT BEEN INPUT BY USER                   
         MVI   INVLWT,1                                                         
         MVC   INVLFLE,INVTYP      TAKE FILE FROM TYPE                          
         CLI   INVLFLE,C'I'        TEST FOR INVENTORY                           
         BE    *+8                                                              
         OI    INVLTYP,X'40'       THIS ENTRY IS THE 1ST DAY/TIME               
         MVC   INVLSTIM,TEMPSTIM   START TIME                                   
         MVC   INVLETIM,TEMPETIM   END TIME                                     
         MVC   INVLDAY,TEMPDAY     DAY                                          
         MVC   SVSETIM,INVLSTIM    SAVE START AND END TIME                      
         DROP  R5                                                               
*                                                                               
* GENERATE DATA RECORDS IN A LOOP USING BOOK LIST                               
* R2 WILL BE USED AS A COUNTER OF BOOK LIST ENTRIES.  ERROR                     
* MESSAGE NUMBERS MUST BE LOADED ONLY UPON ERROR EXIT                           
*                                                                               
BK150    DS    0H                                                               
         MVI   TRMODE,C'I'         INITIALIZE FOR BUFFERING AND                 
         GOTO1 =A(OVFLRTN5),DMCB,(0,DUB),(RC),RR=RELO2   (BUFFER)               
         ZIC   R2,TRBKCNT          COUNT OF ENTRIES INBOOK LIST                 
         LA    R5,TRBKLIST         R5 POINTS TO ENTRY                           
         USING BKLSTD,R5                                                        
*                                                                               
BK160    EQU   *                                                                
         MVC   INVSRC,0(R5)        BOOKVAL BIT OF FROM BOOK                     
         MVC   INVFBK,1(R5)        FROM BOOK                                    
         MVC   INVBTYPE,3(R5)      FROM BOOK TYPE                               
         MVC   INVTOBK(4),4(R5)    TO BOOK                                      
*                                                                               
         MVC   TEMPBOOK,INVFBK     CHECK VALID BOOK/STA MATCH                   
         GOTO1 =A(OVFLRTN4),DMCB,(4,DUB),(RC),RR=RELO2     (VALBKSTA)           
*                                                                               
         CLC   INVTOBK+1(2),=C'AM'  TEST FOR SAME                               
         BNE   *+10                                                             
         MVC   INVTOBK(4),INVSRC   SET TO BOOK EQUAL TO FROM BOOK               
         CLI   OTOBTYP,0           NO OUTPUT BKTYPE?                            
         BE    BK170                                                            
         CLC   OTOBTYP,OFRBTYP     SAME BKTYPE AS FROM?                         
         BE    BK170                                                            
         TM    OTOBIT,X'20'        IF ESTIM BK OR PROJ BK - > ERROR             
         BO    *+12                                                             
         TM    OTOBIT,X'04'                                                     
         BNO   BK170                                                            
         CLI   OFRBTYP,0                                                        
         BNE   *+14                                                             
         MVC   INVBTYPE,OTOBTYP    SET SOURCE BKTYPE=DESTIN BKTYPE              
         B     BK170                                                            
         DC    H'00'                                                            
*                                                                               
BK170    DS    0H                                                               
         LA    RE,BUFF             CLEAR AND BUILD ONE DATA                     
         A     RE,=F'4000'                                                      
         LR    R4,RE               RECORD FOR EACH BOOK ENTRY IN                
         LA    RF,2000             REC.                                         
         SR    R1,R1                                                            
         MVCL  RE,R0               ZERO REC.                                    
         ST    R4,AIO                                                           
*                                                                               
         MVC   RINVKEY,TRSVKEY     MOVE IN HEADER'S KEY                         
         MVC   RINVKSRC(3),INVTOBK                                              
*                                                                               
         XC    IBLK,IBLK           INPUT BLOCK:                                 
         MVC   IBLK+3(1),INVTOBK     BOOKVAL BITS                               
         MVC   IBLK+4(1),INVTOBK+3   BOOKTYPE                                   
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',IBLK),OBLK                                   
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BE    *+6                 INVALID BOOK                                 
         DC    H'00'                                                            
         MVC   RINVKSRC,OBLK+2     SET RINVKSRC                                 
*                                                                               
BK175    DS    0H                                                               
         MVC   RINVLEN,=H'35'      SET LENGTH FOR NEW RECORD                    
*                                                                               
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     RF,AIO1             SAVE AWAY CURRENT REC IN AIO1                
         LA    R1,2000                                                          
         L     RE,AIO2                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         CLI   TTUPTLN,0           ANY OPTIONS?                                 
         BE    BK175B                                                           
         OC    TTOPTBK,TTOPTBK     BASE BOOK INPUT?                             
         BNZ   BK175A              YES                                          
         GOTO1 =A(OVFLRTN3),DMCB,(9,DUB),(RC),RR=RELO2    (GETBASE)             
         GOTO1 =A(OVFLRTN3),DMCB,(10,DUB),(RC),RR=RELO2    (USEBASE)            
         B     BK175C                                                           
*                                                                               
BK175A   MVC   INVSRC,TTOPTBK      BOOKVAL BIT OF BASE BOOK                     
         MVC   INVFBK,TTOPTBK+1    BASE BOOK (OPTIONS)                          
         MVC   INVBTYPE,TTOPTBK+3  BASE BOOK TYPE                               
         MVC   INVTOBK(4),TTOPTBK  TO BOOK                                      
         B     BK175C                                                           
*                                                                               
BK175B   TM    MYFLAG,GOTDEM       WENT TO DEMAND WITH PURE #?                  
         BZ    *+10                                                             
         MVC   INVFBK,SRCEBK       LOOK UP THE SOURCE BOOK                      
*                                                                               
BK175C   DS    0H                                                               
         XC    ACMMNCTE,ACMMNCTE                                                
*                                                                               
         MVI   WGTWEEK,0           SET WEIGHT-WITH-WEEKS FLAG                   
         TM    RMPPROFS+RMP_WKWB,RMP_WKWA                                       
         BZ    *+8                                                              
         MVI   WGTWEEK,C'Y'        FACTOR WKS INTO WGT, AS PER PROFILE          
*                                                                               
         GOTO1 VT81030,DMCB,(RC)                                                
*                                                                               
         CLI   INVBAD,0                                                         
         BE    BK179               NO ERROR                                     
*                                                                               
         CLC   INVCODE,=C'  '      JUST LOOKED FOR A PAV?                       
         BE    *+14                YES                                          
         CLC   INVCODE,=C'TT'      LOOKED FOR TT TRACK?                         
         BNE   BK176                                                            
*                                                                               
         CLC   INVCODE,=C'TT'      NO PAV EXISTS - TRY TT                       
         BE    BK176                                                            
         MVC   INVCODE,=C'TT'                                                   
         MVI   INVCDCTL,TP                                                      
         B     BK177                                                            
*                                                                               
BK176    DS    0H                                                               
         TM    MYFLAG,NOTVALTK     NOT VALID TRACK FOR THIS STA?                
         BO    *+8                                                              
         OI    MYFLAG2,BADTRACK    THIS IS A BAD TRACK                          
         BAS   RE,RESTAIO          RESTORE CURRENT REC IN AIO2                  
         XC    INVBAD,INVBAD                                                    
         B     BK300               NO DEMOS FOUND/DON'T ADD TRACK               
*                                                                               
BK177    DS    0H                                                               
         BAS   RE,RESTAIO          RESTORE CURRENT REC IN AIO2                  
         XC    INVBAD,INVBAD                                                    
         B     BK160                                                            
*                                                                               
BK179    DS    0H                                                               
         CLC   INVCODE,=C'  '      THIS A PAV TRACK?                            
         BNE   BK179A                                                           
         L     RF,INVLIST                                                       
         USING INVLD,RF                                                         
         MVC   SVPURE,INVLNUMB                                                  
         DROP  RF                                                               
*                                                                               
BK179A   BAS   RE,RESTAIO          RESTORE CURRENT REC IN AIO2                  
*                                                                               
BK180    DS    0H                                                               
         L     R4,AIO                                                           
         MVC   HALF,27(R4)         REPAIR RECORD LENGTH AFTER                   
         LH    RE,HALF             DEMO MODULES                                 
         BCTR  RE,0                                                             
         STCM  RE,3,27(R4)                                                      
*                                                                               
         GOTO1 =A(OVFLRTN2),DMCB,(1,DUB),(RC),RR=RELO2     (BLDCE)              
         GOTO1 =A(OVFLRTN3),DMCB,(6,DUB),(RC),RR=RELO2     (BLDCF)              
*                                                                               
         XC    WORK2(200),WORK2    BUILD TRANSFER FROM ELEMENT                  
         LA    RE,WORK2            SO DEMUP WILL HAVE FROM SRC/BOOK             
         USING RINVFREL,RE                                                      
         MVI   RINVFRCD,X'03'                                                   
         MVC   RINVFRST,DEMSTA                                                  
         MVC   RINVFRBK,INVSRC                                                  
         MVC   RINVFRTY,INVTYP                                                  
         MVI   RINVFRPR,C'G'       GLOBAL CHANGES                               
         MVC   RINVFRBT,INVBTYPE                                                
         CLI   INVTYP,C'I'         TEST FOR INVENTORY TRANSFER                  
         BNE   *+10                                                             
         MVC   RINVFRBT,INVFRBT    USE BK TYPE PASSED BY T81030                 
         MVI   RINVFRLN,16         ELEMENT LENGTH                               
*                                                                               
         OC    TTSRCEPG,TTSRCEPG   ANY BOOK/PROGRAM ENTERED?                    
         BZ    BK185               NO                                           
         GOTO1 =A(OVFLRTN3),DMCB,(11,DUB),(RC),RR=RELO2   (TRSLTPUR)            
*                                                                               
BK185    GOTO1 HELLO,DMCB,(C'P',OVREP),(0,(R4)),WORK2,0                         
         DROP  RE                                                               
*                                                                               
         LA    RE,WORK             BUILD CODE ELEMENT                           
         USING RINVCEL,RE                                                       
         XC    WORK,WORK                                                        
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,10                                                      
         MVC   RINVCODE,INVCODE                                                 
*                                                                               
         TM    INVCDCTL,TP         FOR TIME PERIOD TRANSFERS WHERE              
         BZ    *+18                AUTOTMATIC FOOTNOTING IS SUPPRESSED,         
         CLI   TRFNOVER,C'Y'       CLEAR THE CODE ON RECORD                     
         BNE   *+10                                                             
         MVC   RINVCODE,SPACES                                                  
*                                                                               
         TM    INVTOBK,X'20'       ESTIMATED TO BOOK TEST                       
         BZ    *+8                                                              
         MVI   RINVCSET,C'E'                                                    
*                                                                               
         TM    INVTOBK,X'04'       PROJECTED TO BOOK TEST                       
         BZ    *+8                                                              
         MVI   RINVCSET,C'P'                                                    
*                                                                               
         TM    INVTOBK,X'02'       SPECIAL SURVEY BOOK TEST                     
         BZ    *+8                                                              
         MVI   RINVCSET,C'S'                                                    
*                                                                               
         OC    RINVCTYP,INVIND     CUMULATIVE INDICATORS                        
         GOTO1 HELLO,DMCB,(C'P',OVREP),(0,(R4)),WORK,0                          
         DROP  RE                                                               
*                                                                               
BK200    DS    0H                                                               
         CLI   TTUPTLN,0           ANY OPTIONS?                                 
         BE    BK240               NO                                           
         GOTO1 =A(OVFLRTN3),DMCB,(1,DUB),(RC),RR=RELO2    (OPTIONS)             
         B     BK247                                                            
*                                                                               
BK240    DS    0H                                                               
         CLC   INVCODE,=C'PJ'      CODE PJ REQUIRES UPGRADE                     
         BNE   BK245                                                            
*                                                                               
         CLI   INVTYP,C'I'         TEST FOR INVENTORY TRANSFER                  
         BE    BK250                                                            
         DC    H'00'                                                            
*                                                                               
BK245    DS    0H                                                               
         XC    WORK,WORK           BUILD FORCED UPGRADE ELEMENT                 
         XC    WORK2,WORK2                                                      
         MVC   WORK2+8(6),=C'IX,100'                                            
         MVI   WORK2+0,14                                                       
         MVI   WORK2+5,6                                                        
         GOTO1 UPVAL,DMCB,(1,WORK2),(C'Y',WORK),ACOMFACS                        
         CLI   DMCB,1                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BK247    GOTO1 HELLO,DMCB,(C'P',OVREP),(0,(R4)),WORK,0                          
         MVI   BYTE2,0                                                          
         CLI   INVTYP,C'I'         TEST FOR INV TO INV TRANSFER                 
         BNE   *+8                                                              
         MVI   BYTE2,C'I'                                                       
*                                                                               
         MVI   BYTE,0              SET PRECISION TYPE FOR DEMO CALCS            
         CLI   TAPEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BYTE,C'I'                                                        
*                                  BECAUSE OF UT/TP PROBLEM                     
         NI    11(R4),X'FF'-X'40'                                               
         L     R7,ACOMFACS                                                      
         XC    MYWORK,MYWORK                                                    
*                                                                               
         MVC   RIDBLK(4),=CL4'RI2=' 4TH DEMUP PARAMETER SETUP                   
         MVC   RIDBLK+4(2),AGENCY                                               
         MVC   RIDBLK+6(1),TIMECHG TIME CHANGE                                  
*                                                                               
         GOTO1 DEMUP,(R1),(BYTE2,34(R4)),(BYTE,WORK),(R7),RIDBLK                
         OI    11(R4),X'40'                                                     
*                                                                               
         CLI   TTUPTLN,0           USED OPTIONS?                                
         BE    BK250               NO                                           
         MVC   INVSRC,0(R5)        RESTORE BOOKVAL BIT OF FROM BOOK             
         MVC   INVFBK,1(R5)        RESTORE FROM BOOK                            
         MVC   INVBTYPE,3(R5)      RESTORE FROM BOOK TYPE                       
         MVC   INVTOBK(4),4(R5)    RESTORE TO BOOK                              
         MVC   RINVKBK,INVFBK      RESTORE NEW BOOK INTO RECORD                 
*                                                                               
BK250    DS    0H                                                               
         MVI   TRMODE,C'P'                                                      
         GOTO1 =A(OVFLRTN5),DMCB,(0,DUB),(RC),RR=RELO2   (BUFFER)               
*                                                                               
BK275    CLC   INVCODE,=C'  '      JUST LOOKED FOR A PAV?                       
         BNE   BK300                                                            
         L     RF,INVLIST                                                       
         USING INVLD,RF                                                         
         MVC   INVLSTIM(4),SVSETIM   START AND END TIME                         
*                                                                               
BK300    LA    R5,OUTLN(R5)                                                     
*                                                                               
         MVC   INVCODE,TTPCODE1    USE PROGRAM CODE 1                           
         MVC   INVCDCTL,TTPCDE1B                                                
         TM    MYFLAG,PGM2         CURRENTLY ADDING PROGRAM 2?                  
         BZ    *+16                                                             
         MVC   INVCODE,TTPCODE2     YES, USE PROGRAM CODE 2                     
         MVC   INVCDCTL,TTPCDE2B                                                
*                                                                               
         BCT   R2,BK160                                                         
         DROP  R5,RF                                                            
*                                                                               
         L     R4,AIO3                                                          
         ST    R4,AIO                                                           
         MVI   TRMODE,C'F'         FINAL BUFFERING                              
         GOTO1 =A(OVFLRTN5),DMCB,(0,DUB),(RC),RR=RELO2   (BUFFER)               
*                                                                               
         MVI   TRMODE,C'W'         HANDLE I/O TO REPFILE                        
         GOTO1 =A(OVFLRTN5),DMCB,(0,DUB),(RC),RR=RELO2   (BUFFER)               
*                                                                               
BK500    DS    0H                                                               
         NI    MYFLAG,X'FF'-GOTDEM                                              
         LA    R3,L'TRACKTAB(R3)                                                
*                                                                               
         TM    MTFLAG,JUSTTRAK    ADD ONLY TRACKS?                              
         BZ    *+8                                                              
         BAS   RE,RESTAIO          RESTORE HEADER IN AIO2                       
         B     BK10                                                             
*                                                                               
BACKTRX  DS    0H                                                               
         NI    MYFLAG,X'FF'-PGM1                                                
         NI    MYFLAG,X'FF'-PGM2                                                
         B     OVEXIT                                                           
*                                                                               
****************************************************************                
*        RESTORE CURRENT RECORD IN AIO2                                         
****************************************************************                
RESTAIO  NTR1                                                                   
         L     RE,AIO2                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     RF,AIO2                                                          
         LA    R1,2000                                                          
         L     RE,AIO1                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
RESTAIOX B     OVEXIT                                                           
         EJECT                                                                  
***********************************************************************         
OVEXIT   XMOD1 1                                                                
RELO2    DS    A                                                                
         SPACE 3                                                                
OVERRND  DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 ERREX                                                            
OVERRND2 DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 MYERROR                                                          
OVREP    DC    CL8'REPFILE'                                                     
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*                 OVERFLOW ROUTINES                             *               
*****************************************************************               
OVFLRTN2 NMOD1 0,*RM10OV2*,RR=R5                                                
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING OVFLRTN2+4096,RA                                                 
*                                                                               
         L     RC,4(R1)                                                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R5,RELO3                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRNCH2(RF)                                                     
*                                                                               
OVBRNCH2 B     DYTIMSET                                                         
         B     BLDCE                                                            
         B     INITDBLK                                                         
         B     GODEMAND                                                         
         B     GETQTR                                                           
         EJECT                                                                  
****************************************************************                
*               SET DAY AND TIME                                                
****************************************************************                
DYTIMSET DS    0H                                                               
         LA    R2,INVDYTIM                                                      
         USING DBXTLD,R2                                                        
         LA    R6,DYTIMTAB                                                      
*                                                                               
         XC    INVDYTIM,INVDYTIM                                                
         XC    DYTIMTAB,DYTIMTAB                                                
         MVC   DBXTLID,=CL4'DYTM'                                               
         LA    R3,DBXTLIST                                                      
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HELLO,DMCB,(C'G',OV2REP),(X'02',AIO),0                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         B     DYTMS100                                                         
*                                                                               
DYTMS060 ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         CLI   0(RE),X'02'         DAY TIME ELEMENT                             
         BNE   DYTMSEX                                                          
         LA    R3,5(R3)                                                         
         LA    R6,5(R6)                                                         
*                                                                               
DYTMS100 MVC   0(1,R3),2(RE)       USE HEADER'S DAY                             
         MVC   1(4,R3),3(RE)       USE HEADER'S TIME                            
         MVC   0(5,R6),2(RE)       MOVE DAY/TIME INTO TABLE                     
         CLC   5(2,RE),=C'CC'      TEST FOR TO CONCLUSION                       
         BNE   DYTMS060            GET NEXT ELEMENT                             
         SR    R1,R1               ADD 2 HOURS TO START                         
         ICM   R1,3,1(R3)                                                       
         AH    R1,=H'200'                                                       
         CH    R1,=H'2400'         TEST FOR RUN PAST MIDNIGHT                   
         BNH   *+8                                                              
         SH    R1,=H'2400'                                                      
         STCM  R1,3,1(R3)          SET END TIME                                 
         B     DYTMS060                                                         
*                                                                               
DYTMSEX  B     OV2EXIT                                                          
         DROP  R2                                                               
         EJECT                                                                  
****************************************************************                
*            BUILD CE ELEMENTS FROM TABLE                                       
****************************************************************                
BLDCE    DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'D',OV2REP),(X'CE',AIO),0                           
         GOTO1 HELLO,DMCB,(C'D',OV2REP),(X'CF',AIO),0                           
*                                                                               
         LA    R6,DYTIMTAB                                                      
*                                                                               
BLDCE050 CLI   0(R6),0                                                          
         BE    BLDCEEX                                                          
*                                                                               
         XC    WORK,WORK           PUT IN 'CE' EL BEFORE POSSIBLE               
         MVC   WORK(2),=X'CE0A'    DEMUP CALL.                                  
         MVC   WORK+2(5),0(R6)     HEADER DAY TIME                              
         MVC   WORK+7(3),INVSRC    FROM BOOK                                    
         GOTO1 HELLO,DMCB,(C'P',OV2REP),(0,AIO),WORK,0                          
         LA    R6,5(R6)                                                         
         B     BLDCE050                                                         
*                                                                               
BLDCEEX  B     OV2EXIT                                                          
         EJECT                                                                  
****************************************************************                
*               INITIALIZE DBLOCK                              *                
****************************************************************                
INITDBLK DS    0H                                                               
         LA    R2,DBLOCK1                                                       
         USING DBLOCKD,R2                                                       
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         LA    R0,BACKIO                                                        
         ST    R0,DBAREC           I/O AREA                                     
         MVC   DBCOMFCS,ACOMFACS   COMFACS                                      
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,STAHLD+4   MEDIA                                        
         MVI   DBSELSRC,C'N'       NSI SOURCE                                   
         MVC   DBSELAGY,AGENCY     AGENCY ALPHA                                 
         MVC   DBSELBK,SRCEBK      BOOK                                         
*                                                                               
IDBLKX   DS    0H                                                               
         B     OV2EXIT                                                          
         DROP  R2                                                               
         EJECT                                                                  
****************************************************************                
*               CALL DEMAND FOR STATION                        *                
****************************************************************                
GODEMAND DS    0H                                                               
         LA    R2,DBLOCK1                                                       
         USING DBLOCKD,R2                                                       
*                                                                               
         MVI   DBFUNCT,DBGETISI    SET DEMAND FUNCTION                          
         MVC   DBSELPR4+1(3),SRCEPGM    PROGRAM NUMBER                          
         XC    SDQCNT,SDQCNT       CLEAR # OF ENTRIES IN SDQTABD TABLE          
*                                                                               
         NI    MYFLAG,X'FF'-GOTDEM                                              
         GOTO1 DEMAND,DMCB,DBLOCKD,DEMHOOK1,0,0,0,0                             
*                                                                               
         OC    SDQCNT,SDQCNT                                                    
         BZ    GODEMANX                                                         
*                                                                               
         LA    RE,WORK2                                                         
         USING SDQTABD,RE                                                       
         MVC   TEMPDAY,SDQDAY                                                   
         GOTO1 =A(OVFLRTN3),DMCB,(0,DUB),(RC),RR=RELO3    (GETDAYCD)            
*                                                                               
         LA    RE,WORK2                                                         
         MVC   TEMPQHR,SDQSQH                                                   
         GOTO1 =A(OVFLRTN3),DMCB,(8,DUB),(RC),RR=RELO3    (QHRTOMIL)            
         MVC   TEMPSTIM,TEMPMTIM   SET START TIME                               
*                                                                               
         LA    RE,WORK2                                                         
         ZIC   R1,SDQEQH                                                        
         LA    R1,1(R1)                                                         
         STC   R1,TEMPQHR                                                       
         GOTO1 =A(OVFLRTN3),DMCB,(8,DUB),(RC),RR=RELO3    (QHRTOMIL)            
         MVC   TEMPETIM,TEMPMTIM   SET END TIME                                 
*                                                                               
GODEMANX DS    0H                                                               
         B     OV2EXIT                                                          
         DROP  R2,RE                                                            
         EJECT                                                                  
DEMHOOK1 DS    0H                                                               
HK1      NTR1                                                                   
         USING DBLOCKD,R2                                                       
*                                                                               
         CLI   DBRECTYP,DBRECPRG   MAKE SURE CORRECT RECD TYPE                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    RE,SDQCNT                                                        
         LA    R1,1(RE)                                                         
         STH   R1,SDQCNT                 YES, UPDATE COUNT                      
*                                                                               
         LA    R5,WORK2                                                         
         USING SDQTABD,R5                                                       
         LA    R4,DBKEY                                                         
         USING PIKEY,R4                                                         
*                                                                               
         CLC   PISTA,STAHLD        MATCH ON STATION?                            
         BNE   DMHK1X                                                           
*                                                                               
         TM    MYFLAG,GOTDEM       ALREADY GOT MATCH ON STATION ?               
         BZ    HK50                NO - GET FIRST MATCH                         
*                                                                               
         MVC   CURRDAY,PIDAY       CURRENT DAY                                  
         ZIC   RF,PISQH            START QTR HOUR                               
         ZIC   RE,PIEQH            END QTR HOUR                                 
         SR    RE,RF                                                            
         STC   RE,CURRDURA                                                      
*                                                                               
         MVC   TEMPDAY,PIDAY       CURRENT DAY                                  
         GOTO1 =A(OVFLRTN3),DMCB,(0,DUB),(RC),RR=RELO3    (GETDAYCD)            
         NC    TEMPDAY,PGMNDAY     # OF DAYS                                    
         GOTO1 =A(OVFLRTN3),DMCB,(7,DUB),(RC),RR=RELO3    (COUNTDAY)            
*                                                                               
         CLC   PGMNDURA,CURRDURA   SAME DURATION?                               
         BE    HK50                                                             
         BL    HK40                PGM DURATION < DEMAND DURATION               
*                                                                               
         ZIC   RF,CURRDURA                                                      
         ZIC   RE,PGMNDURA                                                      
         SR    RE,RF                                                            
         STC   RE,CURRDIFF         DIFFERENCE IN DURATION                       
         B     HK45                                                             
*                                                                               
HK40     DS    0H                                                               
         ZIC   RF,PGMNDURA                                                      
         ZIC   RE,CURRDURA                                                      
         SR    RE,RF                                                            
         STC   RE,CURRDIFF         DIFFERENCE IN DURATION                       
*                                                                               
HK45     DS    0H                                                               
         CLC   SVDIFF,CURRDIFF     IS THIS A BETTER MATCH?                      
         BL    DMHK1X                                                           
         BH    HK50                                                             
*                                                                               
         CLC   SVNDAY,TMPNDAY      IS PREV DAY BETTER THAN CURR DAY?            
         BL    HK50                NO - CURR DAY IS BETTER                      
         B     DMHK1X                                                           
*                                                                               
HK50     DS    0H                                                               
         OI    MYFLAG,GOTDEM                                                    
         MVC   SDQSTA,PISTA                                                     
         MVC   SDQDAY,PIDAY                                                     
         MVC   SDQSQH,PISQH                                                     
         MVC   SDQEQH,PIEQH                                                     
*                                                                               
         MVC   SVDAY,PIDAY       SAVE AWAY DAY                                  
*                                                                               
         MVC   TEMPDAY,PIDAY                                                    
         GOTO1 =A(OVFLRTN3),DMCB,(0,DUB),(RC),RR=RELO3    (GETDAYCD)            
         NC    TEMPDAY,PGMNDAY     # OF DAYS                                    
         GOTO1 =A(OVFLRTN3),DMCB,(7,DUB),(RC),RR=RELO3    (COUNTDAY)            
         MVC   SVNDAY,TMPNDAY                                                   
*                                                                               
         ZIC   RF,PISQH            START QTR HOUR                               
         ZIC   RE,PIEQH            END QTR HOUR                                 
         SR    RE,RF                                                            
         STC   RE,TEMPDURA                                                      
*                                                                               
         CLC   PGMNDURA,TEMPDURA   SAME DURATION?                               
         BE    *+8                                                              
         BL    HK60                PGM DURATION < DEMAND DURATION               
*                                                                               
         ZIC   RF,TEMPDURA                                                      
         ZIC   RE,PGMNDURA                                                      
         SR    RE,RF                                                            
         STC   RE,SVDIFF           DIFFERENCE IN DURATION                       
         B     DMHK1X                                                           
*                                                                               
HK60     DS    0H                                                               
         ZIC   RF,PGMNDURA                                                      
         ZIC   RE,TEMPDURA                                                      
         SR    RE,RF                                                            
         STC   RE,SVDIFF           DIFFERENCE IN DURATION                       
*                                                                               
DMHK1X   DS    0H                  RETURN TO DEMAND                             
         B     OV2EXIT                                                          
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
****************************************************************                
* SUB-ROUTINE TO CONVERT MILITARY TIME TO START QUARTER HOUR                    
****************************************************************                
GETQTR   DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,3,PGMNTIME       START TIME                                   
         SR    R0,R0                                                            
         D     R0,=F'100'          R1=HOURS, R0=REMAINDER MINUTES               
         MH    R1,=H'60'           CONVERT HOURS TO MINUTES                     
         AR    R1,R0               SUM TOTAL MINUTES                            
         CH    R1,=H'360'          TEST FOR LESS THAN 6 AM                      
         BNL   *+8                                                              
         AH    R1,=Y(60*24)        ADD MINUTES OF 24 HOURS                      
         SH    R1,=H'360'          SUBTRACT 6 HOURS TO BASE OFF 6AM             
         SR    R0,R0                                                            
         D     R0,=F'15'           DIVIDE BY MINUTES IN A QUARTER HOUR          
         STC   R1,BYTE                                                          
         MVC   PGMNOINV(1),BYTE                                                 
GETQTEX  B     OV2EXIT                                                          
         EJECT                                                                  
****************************************************************                
OV2EXIT  XMOD1 1                                                                
OV2ERRND DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 ERREX                                                            
OV2ERR2  DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 MYERROR                                                          
OV2REP   DC    CL8'REPFILE'                                                     
RELO3    DS    A                                                                
DBLOCK1  DS    CL256                                                            
         DS    XL14                RESERVE BYTES FOR DBLOCK OVERFLOW            
BACKIO   DS    XL2000                                                           
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*                 OVERFLOW ROUTINES                             *               
*****************************************************************               
OVFLRTN3 NMOD1 0,*RM10OV3*                                                      
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING OVFLRTN3+4096,RA                                                 
*                                                                               
         L     RC,4(R1)                                                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRNCH3(RF)                                                     
*                                                                               
OVBRNCH3 B     GETDAYCD                                                         
         B     OPTIONS                                                          
         B     OVFILADD                                                         
         B     OVFILWRT                                                         
         B     OVDIRWRT                                                         
         B     OVDIRADD                                                         
         B     BLDCF                                                            
         B     COUNTDAY                                                         
         B     QHRTOMIL                                                         
         B     GETBASE                                                          
         B     USEBASE                                                          
         B     TRSLTPUR                                                         
         B     FILLPGM1                                                         
         EJECT                                                                  
*****************************************************************               
*                  1ST PROGRAM = ORIGINAL PROGRAM               *               
*   NOTE - FAKE OUT GLOBAL CHANGE BY MAKING THE ORIGINAL        *               
*          HEADER THE 1ST NEW PROGRAM                           *               
*****************************************************************               
FILLPGM1 DS    0H                                                               
         ZIC   RF,GL1EFFH+5        L'EFF DATE                                   
         STC   RF,GLBNEFFH+5                                                    
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   GLBNEFF(0),GL1EFF   NEW EFF DATE = ORIG EFF DATE                 
         OI    GLBNEFFH+6,X'80'                                                 
*                                                                               
         MVI   GLBNPROH+5,0        COPY SAME PROGRAM AS SOURCE                  
         XC    GLBNPRO,GLBNPRO                                                  
         OI    GLBNPROH+6,X'80'                                                 
*                                                                               
FILLPG1X DS    0H                                                               
         B     OV3EXIT                                                          
         EJECT                                                                  
*****************************************************************               
*                  TRANSLATE PURE NUMBER                        *               
*****************************************************************               
TRSLTPUR DS    0H                                                               
         LA    R5,WORK2                                                         
         USING RINVFREL,R5                                                      
         LA    R3,RINVFRDT         FROM DATA                                    
         LA    R2,TMPPURE          PURE NUMBERS                                 
*                                                                               
TRSL10   DS    0H                                                               
         CLI   0(R2),0             ANY MORE PURE #'S?                           
         BE    TRSLTPUX            NO - DONE                                    
*                                                                               
         LA    R4,DBLOCK2                                                       
         USING DBLOCKD,R4                                                       
         XC    DBLOCK2,DBLOCK2                                                  
*                                                                               
         MVC   DBFILE,=C'PAV'      SET UP DBLOCK : FILE (PAV<==>PURE)           
         LA    RF,IOAREA1                                                       
         ST    RF,DBAREC           SET UP DBLOCK : A(RECORD)                    
         USING PRKEY,RF                                                         
         XC    PRKEY(24),PRKEY                                                  
         LA    R0,PRFRSTEL                                                      
         ST    R0,DBAQUART         SET UP DBLOCK : A(1ST ELEMENT)               
         MVC   PRKMINOR,0(R2)      SET PURE NUMBER IN PAV RECORD                
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 DEFINE,DMCB,=C'PURE',DBLOCKD,WORK+4                              
*                                                                               
         LA    R1,2                LENGTH FOR EXMVC                             
         CLI   WORK+4+3+3,C' '     ONLY A 3 DIGIT NUMBER?                       
         BE    *+8                                                              
         LA    R1,3                                                             
*                                                                               
         EXMVC R1,0(R3),WORK+4+3   PUT IT IN X'03' ELEMENT                      
*                                                                               
         LA    R3,1(R1,R3)         NEXT ENTRY IN ELEMENT                        
         ZIC   RF,RINVFRLN         ELEMENT LENGTH                               
         LA    RF,1(R1,RF)                                                      
         STC   RF,RINVFRLN                                                      
*                                                                               
         CLI   L'TMPPURE(R2),0     IS THERE ANOTHER PURE #?                     
         BE    TRSLTPUX            NO                                           
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         ZIC   RF,RINVFRLN         ELEMENT LENGTH                               
         LA    RF,1(RF)                                                         
         STC   RF,RINVFRLN                                                      
*                                                                               
         LA    R2,L'TMPPURE(R2)    GET NEXT PURE NUMBER                         
         B     TRSL10                                                           
*                                                                               
TRSLTPUX DS    0H                                                               
         B     OV3EXIT                                                          
         DROP  R4,RF,R5                                                         
         EJECT                                                                  
*****************************************************************               
*                 GET DAY CODE                                  *               
*****************************************************************               
GETDAYCD DS    0H                                                               
         LA    R2,DAYTAB                                                        
GDC10    CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    GDCX                YES - EXIT                                   
*                                                                               
         CLC   1(1,R2),TEMPDAY                                                  
         BE    *+12                                                             
         LA    R2,L'DAYTAB(R2)                                                  
         B     GDC10                                                            
         MVC   TEMPDAY,2(R2)                                                    
*                                                                               
GDCX     DS    0H                                                               
         B     OV3EXIT                                                          
*                                  TABLE TO CONVERT KEY DAY VALUES              
DAYTAB   DS    0XL6                                                             
         DC    X'0',X'10',X'40',C'MON'                                          
         DC    X'0',X'20',X'20',C'TUE'                                          
         DC    X'0',X'30',X'10',C'WED'                                          
         DC    X'0',X'40',X'08',C'THU'                                          
         DC    X'0',X'50',X'04',C'FRI'                                          
         DC    X'0',X'60',X'02',C'SAT'                                          
         DC    X'0',X'70',X'01',C'SUN'                                          
         DC    C'T',X'95',X'7C',C'M-F'                                          
         DC    C'T',X'FF',X'FF',C'VAR'                                          
         DC    X'FF',X'FF',X'FF',C'???'                                         
****************************************************************                
*            COUNT NUMBER OF DAYS                                               
****************************************************************                
COUNTDAY DS    0H                                                               
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         ZICM  RF,TEMPDAY,(8)                                                   
         SLL   RF,1                                                             
*                                                                               
         OR    RF,RF                                                            
         BZ    *+16                                                             
         SLDL  RE,1                                                             
         AR    R0,RE                                                            
         SR    RE,RE                                                            
         B     *-14                                                             
*                                                                               
         STC   R0,TMPNDAY                                                       
         B     OV3EXIT                                                          
         EJECT                                                                  
****************************************************************                
*            BUILD CF ELEMENT                                                   
****************************************************************                
BLDCF    DS    0H                                                               
         USING TTABD,R3                                                         
         NI    MYFLAG2,X'FF'-GOTCF                                              
         L     R2,ACMMNCTE         GET 'CF' ELEMENTS                            
         LA    R4,TMPPURE          PURE NUMBERS FROM CF ELEMENT                 
         XC    TMPPURE,TMPPURE                                                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',OV3REP),(X'CF',AIO),0                           
*                                                                               
BLDCF10  DS    0H                                                               
         CLC   =C'HIST',0(R2)      'CF' ELEMENTS HERE?                          
         BNE   BLDCF20              NO                                          
         OI    MYFLAG2,GOTCF       GOT 'CF' ELEMENT                             
*                                                                               
         LA    R0,CFWORK                                                        
         LA    R1,CFWORKL                                                       
         LA    RE,12(R2)           12(R2)-->WHERE X'CF' ELEMENTS START          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         ZIC   R6,8(R2)            # OF 'CF' ELEMENTS TO ADD                    
         LA    R5,CFWORK           FIRST 'CF' ELEMENT                           
         USING RIDHEL,R5                                                        
*                                                                               
BLDCF15  DS    0H                                                               
         OC    TTSRCEPG,TTSRCEPG   ENTERED A BOOK/PGM?                          
         BZ    BLDCF17             NO                                           
         OI    RIDHFLG,RIDHFPUR    PURE NUMBER                                  
         MVC   0(L'TMPPURE,R4),RIDHPURE         SAVE AWAY PURE NUMB             
         LA    R4,L'TMPPURE(R4)                                                 
*                                                                               
BLDCF17  GOTO1 HELLO,DMCB,(C'P',OV3REP),(0,AIO),0(R5),0                         
*                                                                               
         ZIC   RF,1(R5)            L'CF ELEMENT                                 
         AR    R5,RF               BUMP TO NEXT CF ELEMENT                      
         BCT   R6,BLDCF15          GET NEXT ELEMENT                             
         B     BLDCFX                                                           
*                                                                               
BLDCF20  L     RF,4(R2)            A(NEXT NODE)                                 
         LTR   RF,RF               ANY ADDRESS HERE?                            
         BZ    BLDCFX              NO                                           
*                                                                               
         LR    R2,RF               GO TO NEXT NODE                              
         B     BLDCF10                                                          
*                                                                               
BLDCFX   DS    0H                                                               
         TM    MYFLAG2,GOTCF       GOT ANY 'CF' ELEMENT?                        
         BO    *+6                                                              
         DC    H'00'                                                            
         B     OV3EXIT                                                          
         DROP  R5,R3                                                            
         EJECT                                                                  
****************************************************************                
*                 GET BOOK TYPE IN EBCDIC                                       
****************************************************************                
BKTYP    NTR1                                                                   
         XC    BOOKTYPE,BOOKTYPE                                                
*                                                                               
         TM    INVSRC,X'20'         ESTIMATED BOOK?                             
         BZ    GBK10                                                            
         MVI   BOOKTYPE,C'E'                                                    
         B     GBKX                                                             
*                                                                               
GBK10    TM    INVSRC,X'04'         PROJECTED BOOK?                             
         BZ    GBK20                                                            
         MVI   BOOKTYPE,C'P'                                                    
         B     GBKX                                                             
*                                                                               
GBK20    TM    INVSRC,X'02'         SPECIAL BOOK?                               
         BZ    GBK30                                                            
         MVI   BOOKTYPE,C'S'                                                    
         B     GBKX                                                             
*                                                                               
GBK30    TM    INVSRC,X'08'         TIME PERIOD?                                
         BZ    GBKX                                                             
         MVI   BOOKTYPE,C'T'                                                    
         B     GBKX                                                             
*                                                                               
GBKX     DS    0H                                                               
         B     OV3EXIT                                                          
         EJECT                                                                  
****************************************************************                
*    ROUTINE TO CONVERT QUARTER HOUR TO MILITARY TIME                           
****************************************************************                
* At entry,                                                                     
*   TEMPQHR  = input quarter hour.                                              
* At exit,                                                                      
*   TEMPMTIM = output military time.                                            
*                                                                               
QHRTOMIL DS    0H                                                               
         SR    R0,R0                                                            
         ZIC   R1,TEMPQHR                                                       
         D     R0,=F'4'                                                         
         LA    R1,5(R1)            BASE = 5AM IF RADIO,                         
         CLI   STAHLD+4,C'R'                                                    
         BE    *+8                                                              
         LA    R1,1(R1)             ELSE, BASE = 6AM                            
         CH    R1,=H'24'           TEST AFTER MIDNIGHT                          
         BL    *+8                                                              
         SH    R1,=H'24'           YES - GO BACK ONE DAY                        
         MH    R1,=H'100'                                                       
         MH    R0,=H'15'                                                        
         AR    R1,R0               R1 CONTAINS MILITARY TIME                    
         OR    R1,R1                                                            
         BNZ   *+8                                                              
         LH    R1,=H'2400'                                                      
         STCM  R1,3,TEMPMTIM                                                    
*                                                                               
         B     OV3EXIT                                                          
         EJECT                                                                  
*****************************************************************               
*                 PROCESS OPTIONS                               *               
*****************************************************************               
OPTIONS  DS    0H                                                               
*        LA    R3,TRACKTAB                                                      
         USING TTABD,R3                                                         
*                                                                               
         XC    TEMPWORK,TEMPWORK   BUILD DUMMY HEADER                           
         MVC   TEMPWORK+5(1),TTUPTLN                                            
         ZIC   RF,TTUPTLN                                                       
         LA    RF,8(RF)            ADD 8 FOR HEADER                             
         STC   RF,TEMPWORK                                                      
*                                                                               
         ZIC   RF,TTUPTLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TEMPWORK+8(0),TTUPT     UPT OPTION                               
*                                                                               
         ZIC   RF,TEMPWORK+5                                                    
         LA    RE,TEMPWORK+8                                                    
OPT10    DS    0H                                                               
         CLI   0(RE),C'/'          SLASH HERE                                   
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,OPT10                                                         
*                                                                               
         MVI   0(RE),C','          UPVAL REQUIRES ',' - NOT A '/'               
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,TEMPWORK                                                      
         GOTO1 UPVAL,DMCB,(1,(R2)),(C'Y',WORK),ACOMFACS                         
         CLI   DMCB,1                                                           
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
OPTIONSX DS    0H                                                               
         B     OV3EXIT                                                          
         EJECT                                                                  
****************************************************************                
*            GET BASE BOOKS FROM DEMAND                                         
****************************************************************                
GETBASE  DS    0H                                                               
         LA    R2,TRBKLIST                                                      
         LA    R3,BASETAB                                                       
         XC    BASETAB,BASETAB                                                  
*                                                                               
GB10     DS    0H                                                               
         OC    0(4,R2),0(R2)       ANY BOOK HERE?                               
         BZ    GB20                NO - GET BASE BOOKS                          
*                                                                               
         MVC   0(3,R3),1(R2)       MOVE IN BOOK/TYPE INTO BASETAB               
         LA    R3,5(R3)            NEXT ENTRY IN BASETAB                        
         LA    R2,8(R2)            NEXT ENTRY IN TRBKLIST                       
         B     GB10                                                             
*                                                                               
GB20     LA    R3,BASETAB                                                       
*                                                                               
GB30     DS    0H                                                               
         CLI   0(R3),0             ANY MORE BOOKS?                              
         BE    GETBASEX            NO - DONE                                    
*                                                                               
         LA    R4,DBLOCK2          BUILD DBLOCK TO VALIDATE STA/BOOK            
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         LA    R0,IOAREA1                                                       
         ST    R0,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS   COMFACS                                      
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELSTA,STAHLD     STATION                                      
         MVC   DBSELMED,STAHLD+4   MEDIA                                        
         MVI   DBSELSRC,C'N'       NSI SOURCE                                   
         MVC   DBSELAGY,AGENCY     AGENCY ALPHA                                 
*                                                                               
         CLI   STAHLD+4,C'T'       IF MEDIA IS TELEVISION,                      
         BNE   *+16                                                             
         TM    DBSELSTA+4,X'F0'      REPLACE ANY PARENT PLUS                    
         BNO   *+8                                                              
         MVI   DBSELSTA+4,C'T'       WITH A 'T'                                 
*                                                                               
         XC    DBSELBK,DBSELBK     GET BASE BOOK                                
         MVC   DBBTYPE,2(R3)       BOOK TYPE                                    
         MVI   DBFUNCT,DBGETTLB                                                 
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCKD,0,0                                          
         CLI   DBERROR,0           TEST FOR DEMAND ERRORS                       
         BE    *+12                                                             
         OI    MYFLAG2,BADTRACK                                                 
         BE    *+10                                                             
*                                                                               
         MVC   3(2,R3),DBACTBK     BASE BOOK                                    
*                                                                               
         LA    R3,5(R3)            NEXT BASETAB ENTRY                           
         B     GB30                                                             
*                                                                               
GETBASEX DS    0H                                                               
         B     OV3EXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
*            USE BASE BOOKS FROM BASETAB                                        
****************************************************************                
USEBASE  DS    0H                                                               
         LA    R3,BASETAB                                                       
*                                                                               
UB10     DS    0H                                                               
         CLI   0(R3),0             ANY MORE BOOKS?                              
         BE    USEBASEX            NO - DONE                                    
*                                                                               
         CLC   INVFBK,0(R3)        SAME BOOK?                                   
         BNE   UB20                NO                                           
         CLC   INVBTYPE,2(R3)      SAME BOOK TYPE?                              
         BNE   UB20                NO                                           
         MVC   INVFBK,3(R3)        USE THIS BASE BOOK                           
         MVC   INVTOBK,3(R3)                                                    
         B     USEBASEX                                                         
*                                                                               
UB20     DS    0H                                                               
         LA    R3,5(R3)            GET NEXT BASETAB ENTRY                       
         B     UB10                                                             
*                                                                               
USEBASEX DS    0H                                                               
         B     OV3EXIT                                                          
         EJECT                                                                  
**********************************************************************          
*                       DATAMGR INTERFACE                                       
**********************************************************************          
OVFILADD DS    0H                                                               
         BAS   RE,OVKEYCHK                                                      
         BRAS  RE,AUDIT         MAINTAIN AUDIT TRAIL AS OF 9/00                 
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,ODMCHECK                                                      
         MVC   BSVDA,KEY+28     SAVE DISK ADDRESS                               
         B     OVYES                                                            
*                                                                               
OVFILWRT DS    0H                                                               
         BAS   RE,OVKEYCHK                                                      
         BRAS  RE,AUDIT         MAINTAIN AUDIT TRAIL AS OF 9/00                 
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'REPFILE ',KEY+28,AIO,DMWORK           
         BAS   RE,ODMCHECK                                                      
         B     OVYES                                                            
*                                                                               
OVDIRWRT DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,ODMCHECK                                                      
         B     OVYES                                                            
*                                                                               
OVDIRADD DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR  ',KEY,KEY                      
         BAS   RE,ODMCHECK                                                      
         B     OVYES                                                            
*                                                                               
OVKEYCHK DS    0H                                                               
         L     RF,AIO                                                           
         CLC   KEYSAVE(27),0(RF)                                                
         BER   RE                                                               
         DC    H'00'                                                            
*                                                                               
ODMCHECK CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'20'         DUPLICATE KEY ON ADD                         
         BZ    ODM40                                                            
         MVI   ERROR,DUPLICAT                                                   
         B     OV3ERRND                                                         
ODM40    TM    8(R1),X'90'                                                      
         BM    OVNO                                                             
         DC    H'0'                                                             
*                                                                               
OVYES    SR    R1,R1                                                            
         B     *+8                                                              
OVNO     LA    R1,1                                                             
         LTR   R1,R1                                                            
*                                                                               
OVXIT    XIT1  REGS=(R0,R1)                                                     
*****************************************************************               
OV3EXIT  XMOD1 1                                                                
OV3ERRND DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 ERREX                                                            
OV3ERR2  DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 MYERROR                                                          
OV3REP   DC    CL8'REPFILE'                                                     
BOOKTYPE DS    CL1                 BOOK TYPE                                    
         LTORG                                                                  
DBLOCK2  DS    CL256                                                            
         DS    XL14                RESERVE BYTES FOR DBLOCK OVERFLOW            
CFWORK   DS    10XL(RIDHELLN)      BLOCK OF DEMO HISTORY ELEMENTS               
CFWORKL  EQU   *-CFWORK                                                         
IOAREA1  DS    XL2000                                                           
         EJECT                                                                  
*****************************************************************               
*                 OVERFLOW ROUTINES                             *               
*****************************************************************               
OVFLRTN4 NMOD1 0,*RM10OV4*,RR=R5                                                
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING OVFLRTN4+4096,RA                                                 
*                                                                               
         L     RC,4(R1)                                                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R5,RELO5                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRNCH4(RF)                                                     
*                                                                               
OVBRNCH4 B     BLDTTAB                                                          
         B     FRSTSTA                                                          
         B     COPYTRAK                                                         
         B     ALLTEXT                                                          
         B     VALBKSTA                                                         
****************************************************************                
*              ADD ALL TEXT RECORDS                                             
****************************************************************                
ALLTEXT  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   AIO,AIO3                                                         
*                                                                               
         L     R4,AIO3                                                          
         USING REINVREC,R4                                                      
         L     R3,AIO1                                                          
*                                                                               
         MVC   KEY(24),0(R3)       MOVE IN ORIG HEADER KEY                      
         GOTO1 HIGH                                                             
         B     AT10                                                             
AT5      GOTO1 SEQ                                                              
         MVC   TEMPKEY,KEY                                                      
*                                                                               
AT10     DS    0H                                                               
         CLC   KEY(24),KEYSAVE     SAME INV REC TO EFF DATE?                    
         BNE   ALLTEXTX            NO - EXIT                                    
         CLI   KEY+24,X'FF'        TEXT RECORD?                                 
         BNE   AT5                 NO - TRY NEXT RECORD                         
*                                                                               
         GOTO1 GETREC              GET ORIG TEXT RECORD                         
         XC    KEY,KEY                                                          
         L     R3,AIO2             NEW HEADER ADDED                             
         MVC   KEY(24),0(R3)       EVERYTHING UP TO SRC AND TEXT #              
         MVC   KEY+24(3),RINVKSRC  MOVE IN SRC AND TEXT #                       
*                                                                               
         GOTO1 HIGH                CHECK IF THIS TEXT ALREADY EXISTS            
         CLC   KEY(27),KEYSAVE     DOES IT EXIST?                               
         BE    AT20                YES - DON'T ADD THIS                         
*                                                                               
         MVC   RINVKEY(24),0(R3)   COPY NEW HEAD KEY UP TO SRC                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',OV4REP),(X'EF',AIO),0                           
         L     R3,12(R1)                                                        
         CLI   12(R1),0                                                         
         BE    *+6                 NOT THERE - ERROR                            
         DC    H'00'                                                            
         USING RINVAEL,R3                                                       
*                                                                               
         OI    RINVAFLG,AEGLOBAL   CAME FROM GLOBAL/CHANGE                      
         MVI   RINVAWHY,C'A'                                                    
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(OVFLRTN3),DMCB,(2,DUB),(RC),RR=RELO5    (OVFILADD)            
*                                                                               
AT20     XC    KEY,KEY             RESTORE SEQUENTIAL READ SEQUENCE             
         MVC   KEY(27),TEMPKEY                                                  
         GOTO1 HIGH                                                             
         B     AT5                                                              
*                                                                               
ALLTEXTX DS    0H                                                               
         B     OV4EXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
*              COPY TRACKS FROM SOURCE HEADER                                   
****************************************************************                
COPYTRAK DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),ORIGKEY     SOURCE HEADER KEY                            
*                                                                               
         XC    TEMPKEY,TEMPKEY                                                  
         MVC   TEMPKEY,KEY         SAVE AWAY KEY                                
*                                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         MVC   AIO,AIO3                                                         
         L     R4,AIO                                                           
         USING REINVREC,R4                                                      
         MVC   0(27,R4),KEY                                                     
         L     R5,ATRBKLST                                                      
*                                                                               
CT10     DS    0H                                                               
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         MVC   KEY+24(3),0(R5)     RINVKSRC                                     
         XC    IBLK,IBLK           INPUT BLOCK:                                 
         MVC   IBLK+3(1),0(R5)       BOOKVAL BITS                               
         MVC   IBLK+4(1),3(R5)       BOOKTYPE                                   
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',IBLK),OBLK                                   
         CLI   DMCB+4,0            ANY ERRORS?                                  
         BE    *+6                 INVALID BOOK                                 
         DC    H'00'                                                            
         MVC   KEY+24(1),OBLK+2     SET RINVKSRC                                
*                                                                               
         MVC   TEMPBOOK,KEY+25     CURRENT BOOK BEING PROCESSED                 
         GOTO1 =A(OVFLRTN4),DMCB,(4,DUB),(RC),RR=RELO5     (VALBKSTA)           
*                                                                               
         GOTO1 HIGH                GET TRACK RECORD                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    CT20                                                             
         TM    MYFLAG,NOTVALTK     VALID TRACK/STATION MATCH?                   
         BO    *+8                 NO                                           
         OI    MYFLAG2,BADTRACK    SKIPPED THIS BAD TRACK                       
         B     CT80                                                             
*                                                                               
CT20     GOTO1 GETREC                                                           
         XC    KEY,KEY                                                          
         L     R3,AIO2             NEW HEADER ADDED                             
         MVC   KEY(24),0(R3)       EVERYTHING UP TO SRC AND BOOK                
         MVC   KEY+24(3),RINVKSRC  MOVE IN SRC AND BOOK                         
*                                                                               
         GOTO1 HIGH                CHECK IF THIS TRACK ALREADY EXISTS           
         CLC   KEY(27),KEYSAVE     DOES IT EXIST?                               
         BNE   CT30                NO - CAN ADD THIS TRACK                      
         OI    MYFLAG2,BADTRACK    SKIPPED THIS BAD TRACK                       
         B     CT80                YES - DON'T ADD THIS                         
*                                                                               
CT30     MVC   RINVKEY(24),0(R3)   COPY NEW HEAD KEY UP TO SRC                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',OV4REP),(X'03',AIO),0                           
         L     R3,12(R1)                                                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING RINVFREL,R3                                                      
         MVI   RINVFRPR,C'G'       CAME FROM GLOBAL                             
         DROP  R3                                                               
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',OV4REP),(X'EF',AIO),0                           
         L     R3,12(R1)                                                        
         CLI   12(R1),0                                                         
         BE    *+6                 NOT THERE - BUILD IT FROM SCRATCH            
         DC    H'00'                                                            
         USING RINVAEL,R3                                                       
*                                                                               
         OI    RINVAFLG,AEGLOBAL   CAME FROM GLOBAL/CHANGE                      
         MVI   RINVAWHY,C'A'                                                    
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(OVFLRTN3),DMCB,(2,DUB),(RC),RR=RELO5    (OVFILADD)            
*                                                                               
*  NOW GO ADD CORRESPONDING TEXT RECORD FOR SPECIFIC BOOK                       
*                                                                               
CT80     XC    KEY,KEY                                                          
         MVC   AIO,AIO3                                                         
*                                                                               
         L     R4,AIO3                                                          
         USING REINVREC,R4                                                      
         L     R3,AIO1                                                          
*                                                                               
         MVC   KEY(24),0(R3)       MOVE IN ORIG HEADER KEY                      
         GOTO1 HIGH                                                             
         B     CT100                                                            
CT90     GOTO1 SEQ                                                              
*                                                                               
CT100    DS    0H                                                               
         CLC   KEY(24),KEYSAVE     SAME INV REC TO EFF DATE?                    
         BNE   COPYTRKX            NO - EXIT                                    
         CLI   KEY+24,X'FF'        TEXT RECORD?                                 
         BNE   CT90                NO - TRY NEXT RECORD                         
         MVC   TEMPKEY,KEY                                                      
*                                                                               
         GOTO1 GETREC              GET ORIG TEXT RECORD                         
         XC    KEY,KEY                                                          
         L     R3,AIO2             NEW HEADER ADDED                             
         MVC   KEY(24),0(R3)       EVERYTHING UP TO SRC AND TEXT #              
         MVC   KEY+24(3),RINVKSRC  MOVE IN SRC AND TEXT #                       
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',OV4REP),(X'02',AIO),0                           
         L     R3,12(R1)                                                        
         CLI   12(R1),0                                                         
         BNE   CT110                                                            
         USING RINVFEL,R3                                                       
*                                                                               
         OC    RINVFBK,RINVFBK     ANY BOOK ASSOCIATED W/ TEXT?                 
         BZ    CT110               NO - CAN ADD THIS TEXT RECORD                
*                                                                               
         CLC   TEMPBOOK,RINVFBK    SAME CORRESPONDING BOOK?                     
         BNE   CT120                                                            
         DROP  R3                                                               
*                                                                               
CT110    GOTO1 HIGH                CHECK IF THIS TEXT ALREADY EXISTS            
         CLC   KEY(27),KEYSAVE     DOES IT EXIST?                               
         BE    CT120               YES - DON'T ADD THIS                         
*                                                                               
         L     R3,AIO2                                                          
         MVC   RINVKEY(24),0(R3)   COPY NEW HEAD KEY UP TO SRC                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',OV4REP),(X'EF',AIO),0                           
         L     R3,12(R1)                                                        
         CLI   12(R1),0                                                         
         BE    *+6                 NOT THERE - ERROR                            
         DC    H'00'                                                            
         USING RINVAEL,R3                                                       
*                                                                               
         OI    RINVAFLG,AEGLOBAL   CAME FROM GLOBAL/CHANGE                      
         MVI   RINVAWHY,C'A'                                                    
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(OVFLRTN3),DMCB,(2,DUB),(RC),RR=RELO5    (OVFILADD)            
*                                                                               
CT120    XC    KEY,KEY             RESTORE SEQUENTIAL READ SEQUENCE             
         MVC   KEY(27),TEMPKEY                                                  
         GOTO1 HIGH                                                             
         B     CT90                                                             
*                                                                               
COPYTRKX DS    0H                                                               
         B     OV4EXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
*        GET FIRST STATION WITH VALID HEADER                   *                
****************************************************************                
FRSTSTA  DS    0H                                                               
         LA    R6,MENUTAB                                                       
         ZIC   R3,MENUCNT                                                       
*                                                                               
         LA    R5,KEY                                                           
         USING REINVREC,R5                                                      
*                                                                               
FS10     DS    0H                                                               
         MVC   RINVKSTA,0(R6)      CHECK THIS STATION                           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     DUPLICATE KEY ON THIS STA?                   
         BE    FS20                YES - START W/ THIS STATION                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),KEYSAVE     RESTORE KEY                                  
*                                                                               
         LA    R6,5(R6)            TRY NEXT STATION                             
         BCT   R3,FS10                                                          
         LA    R2,GL1SSTAH                                                      
         MVC   RERROR,=AL2(HEADDNE)   HEADER DOESN'T EXIST FOR ANY STA          
         B     OV4ERR2                                                          
*                                                                               
FS20     DS    0H                  START W/ THIS STATION - REBUILD KEY          
         MVC   STAHLD,0(R6)                                                     
         MVC   SV1STA,0(R6)                                                     
*                                                                               
         MVC   CCONKSTA,STAHLD                                                  
         MVC   CCONINV,INVHLD                                                   
         MVC   CCONEFF,INVEFF                                                   
*                                                                               
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,DTEHLD                                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
FRSTSTAX DS    0H                                                               
         B     OV4EXIT                                                          
         DROP  R5                                                               
****************************************************************                
*                BUILD TRACK TABLE                             *                
****************************************************************                
BLDTTAB  DS    0H                                                               
         LA    R3,TRACKTAB         CLEAR TRACK TABLE                            
         XCEF  (R3),432            L'TRACK TABLE                                
         USING TTABD,R3                                                         
*                                                                               
         ST    R3,APREVLIN         A(PREVIOUS LINE)                             
         LA    R2,GLBBOK1H                                                      
         CLI   5(R2),0             ANY BOOKS HERE?                              
         BE    BLDTTABX            NO - EXIT                                    
*                                                                               
BT10     DS    0H                                                               
         NI    MYFLAG,X'FF'-(NOBKPGM+YESSRCE)                                   
         NI    MYFLAG2,X'FF'-ONLYPGMN                                           
         NI    MYFLAG2,X'FF'-ESPJTRK                                            
*                                                                               
         L     R4,APREVLIN                                                      
         CLI   5(R2),0             ANY BOOKS HERE?                              
         BE    BLDTTABX                                                         
*                                                                               
         CLI   8(R2),C'='          COPY FROM LAST LINE?                         
         BNE   BT12                                                             
         MVC   TTBOOKLN,0(R4)      # OF BOOK ENTRIES                            
         MVC   TTBOOKS,1(R4)       SAME BOOKS AS PREVIOUS LINE                  
         B     BT30                                                             
*                                                                               
BT12     DS    0H                                                               
         CLC   =C'TEXT',8(R2)      COPY TEXT RECORDS?                           
         BNE   BT15                                                             
         MVI   TTBOOKLN,4                                                       
         MVC   TTBOOKS,8(R2)                                                    
         B     BT30                                                             
*                                                                               
BT15     DS    0H                  CHECK FOR VALID BOOK ENTRIES                 
         XC    TRBKLIST,TRBKLIST                                                
         GOTO1 VREBKLST,DMCB,(R2),(C'B',TRBKLIST),BOOKVAL,SCANNER               
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BE    OV4ERRND                                                         
         MVC   TRBKCNT,DMCB        NUMBER OF BOOK ENTRIES                       
*                                                                               
         ZIC   R4,TRBKCNT                                                       
         MH    R4,=H'2'            NUMBER OF BOOKS TO CHECK                     
         LA    R5,TRBKLIST                                                      
*                                                                               
BT20     DS    0H                                                               
         BAS   RE,GETBKTYP         GET BOOK TYPE IN EBCDIC                      
*&&DO                                                                           
         CLI   BKTYPE,C'E'         ESTIMATED BOOK?                              
         BNE   BT25                                                             
         CLI   8+L'GLBBOK1+5(R2),0     ANY SOURCE ENTERED?                      
         BNE   BT25                    YES - CONTINUE                           
         CLI   32+L'GLBBOK1+L'GLBSRC1+L'GLBCD1A+L'GLBCD2A+5(R2),0               
         BNE   BT25                    ANY OPTIONS ENTERED?                     
         MVC   RERROR(2),=AL2(NEEDBKOP)                                         
         B     OV4ERR2                 'E' BOOKS REQUIRE A PURE NUMBER          
*&&                                                                             
BT25     GOTO1 VALBKDAT,DMCB,TODAYBIN,1(R5),BKTYPE                              
         LA    R5,4(R5)            BOOK CAN'T BE > PRESENT MONTH/YEAR           
         BCT   R4,BT20                                                          
         L     R4,APREVLIN                                                      
*                                                                               
         MVC   TTBOOKLN,5(R2)      L'BOOKS FIELD                                
         MVC   TTBOOKS,8(R2)                                                    
*                                                                               
BT30     DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         BAS   RE,OVNXTFLD         BOOK/PROGRAM FIELD                           
         CLI   5(R2),0             ANY BOOK/PROGRAM NUMBER ENTERED?             
         BNE   *+12                                                             
         OI    MYFLAG,NOBKPGM      NO BOOK/PROGRAM ENTERED                      
         B     BT50                                                             
*                                                                               
         CLC   =C'TEXT',TTBOOKS    JUST ADD TEXT RECORDS?                       
         BE    OV4ERRND                                                         
*                                                                               
         CLI   8(R2),C'='          COPY FROM LAST LINE?                         
         BNE   BT35                NO                                           
         OI    MYFLAG,YESSRCE                                                   
         MVC   TTSRCEBK,TTSRCEBK-TTBOOKLN(R4)                                   
         MVC   TTSRCEPG,TTSRCEPG-TTBOOKLN(R4)                                   
         B     BT50                                                             
*                                                                               
BT35     DS    0H                                                               
         GOTO1 SCANNER,DMCB,(0,(R2)),WORK,C',=,/'                               
         CLI   DMCB+4,0                                                         
         BE    OV4ERRND                                                         
*                                                                               
         CLI   WORK+1,0            ONLY PGM NUMBER ENTERED?                     
         BNE   BT40                                                             
         TM    WORK+2,X'80'        NUMERIC - SHOULD BE?                         
         BZ    OV4ERRND                                                         
*                                                                               
         MVC   TTSRCEPG,WORK+5     PROGRAM NUMBER (MAX 3 BYTES)                 
         OI    MYFLAG,YESSRCE                                                   
         OI    MYFLAG2,ONLYPGMN                                                 
         B     BT50                                                             
*                                                                               
BT40     DS    0H                                                               
         TM    WORK+3,X'80'        NUMERIC - SHOULD BE?                         
         BZ    OV4ERRND                                                         
         MVC   TTSRCEPG,WORK+9     PROGRAM NUMBER (MAX 3 BYTES)                 
*                                                                               
* CHECK FOR VALID BOOK ENTRIES BY BUILDING DUMMY HEADER                         
*                                                                               
         XC    TEMPWORK,TEMPWORK                                                
         ZIC   RF,WORK             L'BOOK ENTERED                               
         STC   RF,TEMPWORK+5       INPUT LENGTH                                 
         LA    RF,8(RF)            ADD 8 FOR HEADER                             
         STC   RF,TEMPWORK         FIELD LENGTH                                 
*                                                                               
         ZIC   R1,WORK                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPWORK+8(0),WORK+12    MOVE IN BOOK TO DUMMY HEADER            
*                                                                               
         GOTO1 VREBKLST,DMCB,TEMPWORK,(C'B',WORK),BOOKVAL,SCANNER               
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BE    OV4ERRND                                                         
*                                                                               
         MVC   TTSRCEBK,WORK        SOURCE BOOK                                 
         OI    MYFLAG,YESSRCE                                                   
*                                                                               
BT50     DS    0H                                                               
         BAS   RE,OVNXTFLD         PGM1 CODE                                    
         CLI   5(R2),0             ANY CODE HERE?                               
         BE    BT72                NO - PAV TRACK                               
*                                                                               
         CLI   8(R2),C'='          COPY FROM LAST LINE?                         
         BNE   BT70                                                             
         MVC   TTPCODE1,TTPCODE1-TTBOOKLN(R4)                                   
         MVC   TTPCDE1B,TTPCDE1B-TTBOOKLN(R4)                                   
         B     BT79                                                             
*                                                                               
BT70     DS    0H                                                               
         CLI   5(R2),2             2 LETTER PGM CODE?                           
         BE    BT72                                                             
         CLC   8(4,R2),=C'COPY'    COPY?                                        
         BNE   OV4ERRND                                                         
         MVC   TTPCODE1,=C'CP'     COPY                                         
         B     BT79                                                             
*                                                                               
BT72     DS    0H                                                               
         BAS   RE,GETCODE          GET PROGRAM CODE                             
         CLC   =C'TEXT',TTBOOKS    JUST ADD TEXT RECORDS?                       
         BNE   BT73                ONLY VALID WITH COPY AND N/A                 
         CLC   INVCODE,=C'NA'      N/A                                          
         BNE   OV4ERRND                                                         
*                                                                               
BT73     MVC   TTPCODE1,INVCODE    PROGRAM CODE                                 
         MVC   TTPCDE1B,INVCDCTL   CONTROL BITS                                 
****************************************************************                
*                   'PR' TRACKS                                *                
*                  REQUIRE = PGM #                             *                
*                  OPTIONAL = BK                               *                
*                  NO OPTIONS                                  *                
****************************************************************                
         CLC   TTPCODE1,=C'PR'     PR TRACK?                                    
         BNE   BT75                                                             
         TM    MYFLAG,NOBKPGM      ANY SOURCE ENTERED?                          
         BO    BT74                NO - REQUIRED                                
         CLI   8+L'GLBCD1A+8+L'GLBCD2A+5(R2),0                                  
         BE    BT79                ANY OPTIONS ENTERED?                         
BT74     MVC   RERROR(2),=AL2(PRREQD)                                           
         B     OV4ERR2             'PR' REQUIREMENTS                            
****************************************************************                
*                   'ES' TRACKS                                *                
*                  REQUIRE = BKPGM OR OPTIONS                  *                
*                  IF OPTIONS, OPTIONAL = PGM#                 *                
*                  NO BK                                       *                
****************************************************************                
BT75     CLC   TTPCODE1,=C'ES'     ES TRACK?                                    
         BNE   BT77                                                             
         OI    MYFLAG2,ESPJTRK                                                  
*                                                                               
         TM    MYFLAG,NOBKPGM      ANY SOURCE ENTERED?                          
         BZ    BT76                YES                                          
         CLI   8+L'GLBCD1A+8+L'GLBCD2A+5(R2),0                                  
         BNE   BT79                ANY OPTIONS ENTERED? - YES                   
         MVC   RERROR(2),=AL2(BKOROPT)  NO                                      
         B     OV4ERR2             'ES' REQUIREMENTS                            
*                                                                               
BT76     CLI   8+L'GLBCD1A+8+L'GLBCD2A+5(R2),0                                  
         BE    BT79                ANY OPTIONS ENTERED?                         
         TM    MYFLAG2,ONLYPGMN    ONLY PGM # ENTERED?                          
         BO    BT79                YES - IT'S OK                                
         MVC   RERROR(2),=AL2(ESREQD)                                           
         B     OV4ERR2             'ES' REQUIREMENTS                            
****************************************************************                
*                   'PJ' TRACKS                                *                
*                  REQUIRE = OPTIONS                           *                
*                  OPTIONAL = PGM#                             *                
*                  NO BK                                       *                
****************************************************************                
BT77     CLC   TTPCODE1,=C'PJ'     PJ TRACK?                                    
         BNE   BT79                                                             
         OI    MYFLAG2,ESPJTRK                                                  
*                                                                               
         CLI   8+L'GLBCD1A+8+L'GLBCD2A+5(R2),0                                  
         BNE   BT78                ANY OPTIONS ENTERED? - YES                   
         MVC   RERROR(2),=AL2(PJREQD)  NO                                       
         B     OV4ERR2             'PJ' REQUIREMENTS                            
*                                                                               
BT78     TM    MYFLAG,NOBKPGM      ANY BK/PGM?                                  
         BO    BT79                NO                                           
* !!!!   TM    MYFLAG2,ONLYPGMN    ONLY PGM # ENTERED?                          
* !!!!   BO    BT79                YES - IT'S OK                                
         MVC   RERROR(2),=AL2(PJREQD)                                           
         B     OV4ERR2             'PJ' REQUIREMENTS                            
*                                                                               
BT79     DS    0H                                                               
         BAS   RE,OVNXTFLD         PGM2 CODE                                    
         CLI   5(R2),0             ANY CODE HERE?                               
         BE    BT87                NO - PAV TRACK                               
*                                                                               
         CLI   8(R2),C'='          COPY FROM LAST LINE?                         
         BNE   BT80                                                             
         MVC   TTPCODE2,TTPCODE2-TTBOOKLN(R4)                                   
         MVC   TTPCDE2B,TTPCDE2B-TTBOOKLN(R4)                                   
         B     BT93                                                             
*                                                                               
BT80     DS    0H                                                               
         CLI   GLBNPR2H+5,0        ANY 2ND PROGRAM?                             
         BNE   BT85                                                             
         MVC   RERROR(2),=AL2(NO2PGM)    NO 2ND PROGRAM ENTERED -               
         B     OV4ERR2                   CAN'T HAVE 2ND PGM CODE                
*                                                                               
BT85     DS    0H                                                               
         CLI   5(R2),2             2 LETTER PGM CODE?                           
         BE    BT87                                                             
         CLC   8(4,R2),=C'COPY'    COPY?                                        
         BNE   OV4ERRND                                                         
         MVC   TTPCODE2,=C'CP'     COPY                                         
         B     BT93                                                             
*                                                                               
BT87     DS    0H                                                               
         CLI   GLBNPR2H+5,0        ANY 2ND PROGRAM?                             
         BE    BT93                NO                                           
*                                                                               
         BAS   RE,GETCODE          GET PROGRAM CODE                             
         CLC   =C'TEXT',TTBOOKS    JUST ADD TEXT RECORDS?                       
         BNE   BT87A               ONLY VALID WITH COPY AND N/A                 
         CLC   INVCODE,=C'NA'      N/A                                          
         BNE   OV4ERRND                                                         
*                                                                               
BT87A    MVC   TTPCODE2,INVCODE    PROGRAM CODE                                 
         MVC   TTPCDE2B,INVCDCTL   CONTROL BITS                                 
*                                                                               
         BAS   RE,NOPRPJES         CAN'T HAVE COMBO OF PR,PJ,ES                 
*                                                                               
****************************************************************                
*                   'PR' TRACKS                                *                
*                  REQUIRE = PGM #                             *                
*                  OPTIONAL = BK                               *                
*                  NO OPTIONS                                  *                
****************************************************************                
         CLC   TTPCODE2,=C'PR'     PR TRACK?                                    
         BNE   BT89                                                             
         TM    MYFLAG,NOBKPGM      ANY SOURCE ENTERED?                          
         BO    BT88                NO - REQUIRED                                
         CLI   8+L'GLBCD2A+5(R2),0                                              
         BE    BT93                ANY OPTIONS ENTERED?                         
BT88     MVC   RERROR(2),=AL2(PRREQD)                                           
         B     OV4ERR2             'PR' REQUIREMENTS                            
*                                                                               
****************************************************************                
*                   'ES' TRACKS                                *                
*                  REQUIRE = BKPGM OR OPTIONS                  *                
*                  IF OPTIONS, OPTIONAL = PGM#                 *                
*                  NO BK                                       *                
****************************************************************                
BT89     CLC   TTPCODE2,=C'ES'     ES TRACK?                                    
         BNE   BT91                                                             
         OI    MYFLAG2,ESPJTRK                                                  
*                                                                               
         TM    MYFLAG,NOBKPGM      ANY SOURCE ENTERED?                          
         BZ    BT90                YES                                          
         CLI   8+L'GLBCD2A+5(R2),0                                              
         BNE   BT93                ANY OPTIONS ENTERED? - YES                   
         MVC   RERROR(2),=AL2(BKOROPT)  NO                                      
         B     OV4ERR2             'ES' REQUIREMENTS                            
*                                                                               
BT90     CLI   8+L'GLBCD2A+5(R2),0                                              
         BE    BT93                ANY OPTIONS ENTERED?                         
         TM    MYFLAG2,ONLYPGMN    ONLY PGM # ENTERED?                          
         BO    BT93                YES - IT'S OK                                
         MVC   RERROR(2),=AL2(ESREQD)                                           
         B     OV4ERR2             'ES' REQUIREMENTS                            
****************************************************************                
*                   'PJ' TRACKS                                *                
*                  REQUIRE = OPTIONS                           *                
*                  OPTIONAL = PGM#                             *                
*                  NO BK                                       *                
****************************************************************                
BT91     CLC   TTPCODE2,=C'PJ'     PJ TRACK?                                    
         BNE   BT93                                                             
         OI    MYFLAG2,ESPJTRK                                                  
*                                                                               
         CLI   8+L'GLBCD2A+5(R2),0                                              
         BNE   BT92                ANY OPTIONS ENTERED? - YES                   
         MVC   RERROR(2),=AL2(PJREQD)  NO                                       
         B     OV4ERR2             'PJ' REQUIREMENTS                            
*                                                                               
BT92     TM    MYFLAG,NOBKPGM      ANY BK/PGM?                                  
         BO    BT93                NO                                           
* !!!!   TM    MYFLAG2,ONLYPGMN    ONLY PGM # ENTERED?                          
* !!!!   BO    BT93                YES - IT'S OK                                
         MVC   RERROR(2),=AL2(PJREQD)                                           
         B     OV4ERR2             'PJ' REQUIREMENTS                            
*                                                                               
BT93     DS    0H                                                               
         BAS   RE,OVNXTFLD                                                      
         CLI   5(R2),0             ANY OPTIONS?                                 
         BE    BT150                                                            
*                                                                               
         CLC   =C'TEXT',TTBOOKS    JUST ADD TEXT RECORDS?                       
         BE    OV4ERRND                                                         
*                                                                               
         CLI   8(R2),C'='          COPY FROM LAST LINE?                         
         BNE   BT95                                                             
*                                                                               
         TM    MYFLAG2,ESPJTRK     ES OR PJ TRACK?                              
         BO    BT94                YES                                          
*                                                                               
         TM    MYFLAG,YESSRCE     HAS BOOK/PGM BEEN INPUT?                      
         BZ    *+14                                                             
         MVC   RERROR(2),=AL2(BKOROPT)                                          
         B     OV4ERR2             EITHER SOURCE OR OPTION INPUT                
*                                                                               
BT94     MVC   TTUPTLN,TTUPTLN-TTBOOKLN(R4)                                     
         MVC   TTUPT,TTUPT-TTBOOKLN(R4)                                         
         MVC   TTOPTBK,TTOPTBK-TTBOOKLN(R4)                                     
         B     BT150                                                            
*                                                                               
BT95     DS    0H                                                               
         TM    MYFLAG2,ESPJTRK     ES OR PJ TRACK?                              
         BO    BT96                YES                                          
*                                                                               
         TM    MYFLAG,YESSRCE     HAS BOOK/PGM BEEN INPUT?                      
         BZ    *+14                                                             
         MVC   RERROR(2),=AL2(BKOROPT)                                          
         B     OV4ERR2             EITHER SOURCE OR OPTION INPUT                
*                                                                               
BT96     GOTO1 SCANNER,DMCB,(0,(R2)),WORK,C',=,='                               
         CLI   DMCB+4,0            DID IT WORK?                                 
         BE    OV4ERRND            NO                                           
*                                                                               
         XC    TTOPTBK,TTOPTBK                                                  
         XC    TTUPTLN,TTUPTLN                                                  
         XC    TTUPT,TTUPT                                                      
*                                                                               
         LA    RF,WORK             SCANNER BLOCK                                
BT100    DS    0H                                                               
         CLI   0(RF),0             ANYTHING IN BLOCK?                           
         BE    BT150               NOTHING HERE                                 
         CLI   0(RF),C' '          CAN BE NULL OR SPACE                         
         BE    BT150                                                            
*                                                                               
         CLC   =C'UPT',12(RF)                                                   
         BNE   BT120               NO - MUST BE BOOK PART                       
         ZIC   RE,1(RF)                                                         
         STC   RE,TTUPTLN          L'UPT PART                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TTUPT(0),22(RF)     MOVE IN UPT INTO DUMMY HEADER                
*                                                                               
         LA    RF,32(RF)                                                        
         B     BT100                                                            
*                                                                               
BT120    DS    0H                  MUST BE BOOK PART                            
         MVI   ERROR,INVALID                                                    
         CLC   =C'BK',12(RF)                                                    
         BNE   OV4ERRND                                                         
         XC    TEMPWORK,TEMPWORK   BUILD DUMMY HEADER                           
         ZIC   RE,1(RF)                                                         
         STC   RE,TEMPWORK+5       L'INPUT BOOK                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TEMPWORK+8(0),22(RF)                                             
*                                                                               
         ZIC   RE,TEMPWORK+5       L'INPUT BOOK                                 
         LA    RE,8(RE)            ADD 8 FOR HEADER                             
         STC   RE,TEMPWORK                                                      
         ST    RF,ASCANBLK                                                      
         GOTO1 VREBKLST,DMCB,TEMPWORK,(C'B',WORK),BOOKVAL,SCANNER               
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BE    OV4ERRND                                                         
*                                                                               
         MVC   TTOPTBK,WORK        OPTION BOOK                                  
         L     RF,ASCANBLK                                                      
         LA    RF,32(RF)                                                        
         B     BT100                                                            
*                                                                               
BT150    DS    0H                                                               
         ST    R3,APREVLIN         PREVIOUS LINE ON SCREEN                      
         LA    R3,L'TRACKTAB(R3)   NEXT LINE ENTRY IN TRACK TABLE               
         BAS   RE,OVNXTFLD                                                      
*                                                                               
         LA    RF,GLBLSTH                                                       
         CR    R2,RF                                                            
         BNH   BT10                                                             
*                                                                               
BLDTTABX DS    0H                                                               
         B     OV4EXIT                                                          
         EJECT                                                                  
****************************************************************                
*                 CHECK COMBO OF PJ,PR,EJ                                       
****************************************************************                
NOPRPJES NTR1                                                                   
         NI    MYFLAG2,X'FF'-PRPJES1                                            
         NI    MYFLAG2,X'FF'-PRPJES2                                            
*                                                                               
         CLC   TTPCODE1,=C'PJ'     PGM 1 HAS A PJ                               
         BNE   *+12                                                             
         OI    MYFLAG2,PRPJES1                                                  
         B     NPPE10                                                           
*                                                                               
         CLC   TTPCODE1,=C'PR'     PGM 1 HAS A PR                               
         BNE   *+12                                                             
         OI    MYFLAG2,PRPJES1                                                  
         B     NPPE10                                                           
*                                                                               
         CLC   TTPCODE1,=C'ES'     PGM 1 HAS A ES                               
         BNE   *+12                                                             
         OI    MYFLAG2,PRPJES1                                                  
         B     NPPE10                                                           
*                                                                               
NPPE10   CLC   TTPCODE2,=C'PJ'     PGM 2 HAS A PJ                               
         BNE   *+12                                                             
         OI    MYFLAG2,PRPJES2                                                  
         B     NPPE20                                                           
*                                                                               
         CLC   TTPCODE2,=C'PR'     PGM 2 HAS A PR                               
         BNE   *+12                                                             
         OI    MYFLAG2,PRPJES2                                                  
         B     NPPE20                                                           
*                                                                               
         CLC   TTPCODE2,=C'ES'     PGM 2 HAS A ES                               
         BNE   *+12                                                             
         OI    MYFLAG2,PRPJES2                                                  
         B     NPPE20                                                           
*                                                                               
NPPE20   TM    MYFLAG2,PRPJES1+PRPJES2       CAN'T COMBO THESE                  
         BNO   NPPEX                                                            
         CLC   TTPCODE1,TTPCODE2   SAME ONES?                                   
         BE    NPPEX               YES                                          
         MVC   RERROR(2),=AL2(SEPLINE)                                          
         B     OV4ERR2             DIFFERENT PGM CODES ON DIFF LINES            
*                                                                               
NPPEX    DS    0H                                                               
         B     OV4EXIT                                                          
         EJECT                                                                  
         DROP  R3                                                               
****************************************************************                
*                 GET PROGRAM CODE                                              
****************************************************************                
GETCODE  NTR1                                                                   
         BAS   RE,MOVE                                                          
*                                                                               
         LA    RE,CODETAB                                                       
         LA    R1,CODES                                                         
GC10     CLC   WORK(2),0(RE)                                                    
         BE    GC30                                                             
         LA    RE,L'CODETAB(RE)                                                 
         BCT   R1,GC10                                                          
*                                                                               
         LA    RE,MONTAB           NOT A CODE, LOOK FOR A MONTH/YEAR            
         LA    R1,MONTHSS                                                       
GC20     CLC   WORK(1),0(RE)                                                    
         BE    *+16                VALID MONTH CODE                             
         LA    RE,1(RE)                                                         
         BCT   R1,GC20                                                          
         B     OV4ERRND                                                         
*                                                                               
         CLI   WORK+1,C'0'         NOW LOOK FOR A NUMBER                        
         BL    OV4ERRND                                                         
         CLI   WORK+1,C'9'                                                      
         BH    OV4ERRND                                                         
         MVC   INVCODE,WORK                                                     
         MVI   INVCDCTL,PRO+INV                                                 
         B     GETCODEX                                                         
*                                                                               
GC30     DS    0H                                                               
         MVC   INVCODE,WORK                                                     
         MVC   INVCDCTL,2(RE)      CONTROL BITS                                 
*                                                                               
GETCODEX DS    0H                                                               
         B     OV4EXIT                                                          
*********************************************************************           
CODETAB  DS    0CL3                                                             
         DC    C'TP',AL1(PRO+TP)                                                
         DC    C'TT',AL1(PRO+TP)                                                
         DC    C'ES',AL1(PRO+INV)                                               
         DC    C'PJ',AL1(PRO+INV)                                               
         DC    C'PR',AL1(PRO)                                                   
         DC    C'PA',AL1(PRO+INV)                                               
         DC    C'PT',AL1(PRO+INV+MIX)                                           
         DC    C'TE',AL1(PRO+INV)                                               
         DC    C'PE',AL1(PRO+INV)                                               
         DC    C'NT',AL1(PRO+INV)                                               
         DC    C'FT',AL1(PRO+INV)                                               
         DC    C'MT',AL1(PRO+INV)                                               
         DC    C'YT',AL1(PRO+INV)                                               
         DC    C'JT',AL1(PRO+INV)                                               
         DC    C'OT',AL1(PRO+INV)                                               
         DC    C'RT',AL1(PRO+INV)                                               
         DC    C'NP',AL1(PRO+INV)                                               
         DC    C'FP',AL1(PRO+INV)                                               
         DC    C'MP',AL1(PRO+INV)                                               
         DC    C'YP',AL1(PRO+INV)                                               
         DC    C'OP',AL1(PRO+INV)                                               
         DC    C'RP',AL1(PRO+INV)                                               
         DC    C'JP',AL1(PRO+INV)                                               
         DC    C'NA',AL1(PRO+INV)       - DON'T ADD THIS TRACK                  
         DC    C'  ',AL1(PRO+INV)                                               
CODES    EQU   (*-CODETAB)/L'CODETAB                                            
         SPACE                                                                  
MONTAB   DC    C'NFMAYJO'                                                       
MONTHSS  EQU   (*-MONTAB)                                                       
         EJECT                                                                  
****************************************************************                
*                MOVE IN PROGRAM TO WORK                       *                
****************************************************************                
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
****************************************************************                
*                 GET BOOK TYPE IN EBCDIC                                       
****************************************************************                
GETBKTYP NTR1                                                                   
         XC    BKTYPE,BKTYPE                                                    
         TM    0(R5),X'20'         ESTIMATED BOOK?                              
         BZ    GETBK10                                                          
         MVI   BKTYPE,C'E'                                                      
         B     GETBKX                                                           
*                                                                               
GETBK10  TM    0(R5),X'04'         PROJECTED BOOK?                              
         BZ    GETBK20                                                          
         MVI   BKTYPE,C'P'                                                      
         B     GETBKX                                                           
*                                                                               
GETBK20  TM    0(R5),X'02'         SPECIAL BOOK?                                
         BZ    GETBK30                                                          
         MVI   BKTYPE,C'S'                                                      
         B     GETBKX                                                           
*                                                                               
GETBK30  TM    0(R5),X'08'         TIME PERIOD?                                 
         BZ    GETBKX                                                           
         MVI   BKTYPE,C'T'                                                      
         B     GETBKX                                                           
*                                                                               
GETBKX   DS    0H                                                               
         B     OV4EXIT                                                          
         EJECT                                                                  
****************************************************************                
*               VALID BOOK/STATION CHECK                       *                
****************************************************************                
VALBKSTA DS    0H                                                               
         NI    MYFLAG,X'FF'-NOTVALTK                                            
         LA    R2,DBLOCK3                                                       
         USING DBLOCKD,R2                                                       
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         LA    R0,BACKIO2                                                       
         ST    R0,DBAREC           I/O AREA                                     
         MVC   DBCOMFCS,ACOMFACS   COMFACS                                      
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELSTA,STAHLD     STATION                                      
         MVC   DBSELMED,STAHLD+4   MEDIA                                        
         MVI   DBSELSRC,C'N'       NSI SOURCE                                   
         MVC   DBSELAGY,AGENCY     AGENCY ALPHA                                 
         MVC   DBSELBK,TEMPBOOK    BOOK                                         
*                                                                               
         MVI   DBFUNCT,DBGETTLB    SET DEMAND FUNCTION                          
         GOTO1 DEMAND,DMCB,DBLOCKD,0,0                                          
         CLI   DBERROR,0                                                        
         BE    *+8                                                              
         OI    MYFLAG,NOTVALTK     NOT VALID TRACK FOR THIS STATION             
*                                                                               
         B     OV4EXIT                                                          
         DROP  R2                                                               
         EJECT                                                                  
*****************************************************************               
OV4EXIT  XMOD1 1                                                                
RELO5    DS    A                                                                
OV4ERRND DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 ERREX                                                            
OV4ERR2  DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 MYERROR                                                          
OV4REP   DC    CL8'REPFILE'                                                     
DBLOCK3  DS    CL256                                                            
         DS    XL14                                                             
*                                                                               
*  BUMP TO NEXT SCREEN FIELD                                                    
OVNXTFLD ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         LTORG                                                                  
BACKIO2  DS    XL2000                                                           
         EJECT                                                                  
*****************************************************************               
*                 OVERFLOW ROUTINES                             *               
*****************************************************************               
OVFLRTN5 NMOD1 0,*RM10OV5*,RR=R5                                                
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING OVFLRTN5+4096,RA                                                 
*                                                                               
         L     RC,4(R1)                                                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R5,RELO6                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRNCH5(RF)                                                     
*                                                                               
OVBRNCH5 B     BUFFER                                                           
****************************************************************                
BUFFER   DS    0H                                                               
         CLI   TRMODE,C'I'                                                      
         BE    BUFINIT                                                          
*                                                                               
*        LA    RF,BUFF                                                          
*        ST    RF,AIO                                                           
*                                                                               
         CLI   TRMODE,C'P'                                                      
         BE    BUFPUT                                                           
         CLI   TRMODE,C'F'                                                      
         BE    BUFFIN                                                           
         CLI   TRMODE,C'W'                                                      
         BE    BUFWRT                                                           
         DC    H'0'                                                             
*                                                                               
BUFINIT  DS    0H                  INITIALIZE                                   
         XC    TRPAGE(2),TRPAGE    CLEAR PAGE AND RECORD COUNT                  
         MVC   TEMPLEN,=XL2'0FA0'                                               
         LA    RE,BUFF                                                          
         ST    RE,TRAPAGE          SET POINTER TO PAGE AREA FOR BUFFER          
         LA    RF,4000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR THE PAGE AREA                          
         B     BUFFERX                                                          
         EJECT                                                                  
*                                                                               
* PUT TWO RECORDS INTO BUFFER - WHEN TWO ARE THERE, WRITE A PAGE TO TWA         
*                                                                               
BUFPUT   DS    0H                                                               
         L     R5,TRAPAGE                                                       
         CLI   0(R5),0             FIRST I/O AREA FREE                          
         BE    BUFPUT2             YES                                          
         LA    R5,2000(R5)         NO-TRY NEXT ONE                              
         CLI   0(R5),0                                                          
         BE    BUFPUT2             SECOND IS FREE                               
*                                                                               
         L     R5,TRAPAGE          WRITE PAGE WITH RECORDS IN IT FIRST          
         ZIC   R6,TRPAGE                                                        
         LA    R6,1(R6)                                                         
         STC   R6,TRPAGE           UPDATE PAGES ALREADY WRITTEN                 
         XC    DMCB+8(4),DMCB+8                                                 
         STC   R6,DMCB+8                                                        
         MVC   DMCB+10(2),2(RA)    TERMINAL NUMBER                              
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),TEMPLEN                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',,(R5)                        
         LR    RE,R5                                                            
         LA    RF,4000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0               RE-CLEAR PAGE AREA                           
*                                                                               
BUFPUT2  DS    0H                                                               
         GOTO1 MOVEREC,DMCB,AIO,(R5)                                            
         ZIC   R1,TRRECS                                                        
         LA    R1,1(R1)            INCREMENT COUNT OF RECORDS ALREADY           
         STC   R1,TRRECS           WRITTEN                                      
         B     BUFFERX                                                          
*                                                                               
* WRITE LAST PAGE OUT TO TWA                                                    
*                                                                               
BUFFIN   DS    0H                  END OF LINE ROUTINE                          
         L     R5,TRAPAGE                                                       
         ZIC   R6,TRPAGE                                                        
         LA    R6,1(R6)                                                         
         STC   R6,TRPAGE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         STC   R6,DMCB+8                                                        
         MVC   DMCB+10(2),2(RA)                                                 
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),TEMPLEN                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',,(R5)                        
         B     BUFFERX                                                          
         EJECT                                                                  
*                                                                               
* READ IN PAGES, MOVE RECORDS TO REC, AND WRITE THEM TO REPFILE                 
*                                                                               
BUFWRT   DS    0H                                                               
         SR    R3,R3               PAGE COUNTER                                 
*                                                                               
BUFWRT2  DS    0H                                                               
         LA    R3,1(R3)                                                         
         L     R5,TRAPAGE                                                       
         CLI   0(R5),0             ANYTHING HERE?                               
         BE    BUFFERX             NO - EXIT                                    
*                                                                               
         XC    DMCB+8(4),DMCB+8                                                 
         STC   R3,DMCB+8                                                        
         MVC   DMCB+10(2),2(RA)                                                 
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),TEMPLEN                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(R5)                        
         LA    R6,2                                                             
*                                                                               
BUFWRT4  DS    0H                                                               
         GOTO1 MOVEREC,DMCB,(R5),AIO                                            
         BAS   RE,FLADD                                                         
         LA    R5,2000(R5)                                                      
         BCT   R6,*+8                                                           
         B     BUFWRT6                                                          
         CLI   0(R5),0             SECOND RECORD ON PAGE                        
         BNE   BUFWRT4             YES                                          
*                                                                               
BUFWRT6  DS    0H                                                               
         CLM   R3,1,TRPAGE                                                      
         BL    BUFWRT2                                                          
*                                                                               
BUFFERX  EQU   *                                                                
         B     OV5EXIT                                                          
         EJECT                                                                  
******************************************************************              
*              ADD THE RECORD TO FILE                                           
******************************************************************              
FLADD    NTR1                                                                   
         USING REINVREC,R4                                                      
         USING RINVAEL,R5                                                       
         MVC   KEY,RINVREC                                                      
*                                                                               
         LA    R5,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   RINVACOD(2),=X'EF0C'                                             
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVAFST)                                 
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         OI    RINVAFLG,AEGLOBAL   CAME FROM GLOBAL/CHANGE                      
         MVI   RINVAWHY,C'A'                                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         BE    FLPUT                                                            
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',OV5REP),(0,AIO),(R5)                            
         GOTO1 =A(OVFLRTN3),DMCB,(2,DUB),(RC),RR=RELO6    (OVFILADD)            
         NI    DMINBTS,X'F7'       TURN OFF PASS DELETES                        
         B     OV5EXIT                                                          
*                                                                               
FLPUT    TM    KEY+27,X'80'                                                     
         BNO   FLP05                                                            
         MVI   KEY+27,0                                                         
*        BAS   RE,OVDIRWRT         UNDELETE THE POINTER                         
         GOTO1 =A(OVFLRTN3),DMCB,(4,DUB),(RC),RR=RELO6    (OVDIRWRT)            
FLP05    L     RE,AIO2                                                          
         ST    RE,AIO                                                           
         GOTO1 GETREC              GET OLD RECORD IN REC2                       
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',OV5REP),(X'EF',AIO),0                           
         L     R5,12(R1)                                                        
         CLI   12(R1),0                                                         
         BNE   FLP10               NOT THERE - BUILD IT FROM SCRATCH            
*                                                                               
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
         B     FLP15                                                            
*                                                                               
FLP10    DS    0H                                                               
         LA    R5,WORK                                                          
         GOTO1 DATCON,DMCB,(5,DUB),(3,RINVALST)                                 
*                                                                               
FLP15    MVI   RINVAWHY,C'C'                                                    
         OI    RINVAFLG,AEGLOBAL   CAME FROM GLOBAL/CHANGE                      
         L     RE,AIO3                                                          
         ST    RE,AIO                                                           
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',OV5REP),(X'EF',AIO),0                           
         GOTO1 HELLO,DMCB,(C'P',OV5REP),(0,AIO),(R5)                            
         GOTO1 =A(OVFLRTN3),DMCB,(3,DUB),(RC),RR=RELO6    (OVFILWRT)            
         NI    DMINBTS,X'F7'                                                    
         MVC   BSVDA,KEY+28                                                     
         B     OV5EXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              PARAMETER 1 =       A(FROM RECORD AREA)                          
*              PARAMETER 2 =       A(TO RECORD AREA)                            
*                                                                               
MOVEREC  NTR1                                                                   
         L     R2,0(R1)            FROM REC                                     
         L     R3,4(R1)            TO REC                                       
*                                                                               
         MVC   HALF,27(R2)         FROM REC LEN                                 
         LH    R5,HALF                                                          
         LA    R4,0(R5,R2)                                                      
         MVI   0(R4),0             FOR RECORDS ADDED BY HELLO                   
MOVE100  LTR   R5,R5                                                            
         BZ    MOVEXIT                                                          
*                                                                               
         CH    R5,=H'250'                                                       
         BNH   MOVEREST                                                         
         MVC   0(250,R3),0(R2)                                                  
         LA    R2,250(R2)                                                       
         LA    R3,250(R3)                                                       
         SH    R5,=H'250'                                                       
         B     MOVE100                                                          
MOVEREST BCTR  R5,R0                                                            
         EX    R5,MOVEVAR                                                       
MOVEXIT  L     R6,4(R1)                                                         
         LH    R5,27(R6)                                                        
         AR    R6,R5                                                            
         MVI   0(R6),0                                                          
         B     OV5EXIT                                                          
MOVEVAR  MVC   0(0,R3),0(R2)                                                    
         DROP  R5                                                               
         LTORG                                                                  
****************************************************************                
OV5EXIT  XMOD1 1                                                                
RELO6    DS    A                                                                
OV5ERRND DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 ERREX                                                            
OV5ERR2  DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 MYERROR                                                          
OV5REP   DC    CL8'REPFILE'                                                     
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*                 OVERFLOW ROUTINES                             *               
*****************************************************************               
OVFLRTN6 NMOD1 0,*RM10OV6*,RR=R5                                                
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING OVFLRTN5+4096,RA                                                 
*                                                                               
         L     RC,4(R1)                                                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R5,RELO7                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRNCH6(RF)                                                     
*                                                                               
OVBRNCH6 B     ENDDATE                                                          
         B     NEWEFFD                                                          
         B     GETPGMS                                                          
         B     GETDAYS                                                          
         B     GETTIMES                                                         
****************************************************************                
*                GET NEW END DATE                              *                
****************************************************************                
ENDDATE  DS    0H                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,GLBNEFFH         NEW EFF DATE                                 
         NI    MTFLAG,X'FF'-SRCEFF                                              
         CLI   5(R2),0                                                          
         BE    ENDD05                                                           
*                                                                               
         CLI   8(R2),C'*'          CHANGE SRC EFF DATE?                         
         BNE   ENDD02                                                           
         OI    MTFLAG,SRCEFF       YES                                          
*                                                                               
         LA    R2,GLBNPROH                                                      
         CLI   GLBNPROH+5,0        ANY PGM ENTERED?                             
         BNE   ENDD05E                                                          
         CLI   GLBNDAYH+5,0        ANY DAY ENTERED?                             
         BNE   ENDD05E                                                          
         CLI   GLBNTIMH+5,0        ANY TIME ENTERED?                            
         BNE   ENDD05E                                                          
         CLI   GLBBOK1H+5,0        MUST HAVE TRACKS ENTERED                     
         BNE   ENDD05E                                                          
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(L'GLBNEFF-1),GLBNEFF+1                                       
         ZIC   RF,GLBNEFFH+5                                                    
         BCTR  RF,0                                                             
         STC   RF,GLBNEFFH+5                                                    
         MVC   GLBNEFF(L'GLBNEFF-1),DUB                                         
*                                                                               
ENDD02   DS    0H                                                               
         GOTO1 SCANNER,DMCB,(0,GLBNEFFH),WORK,C',=,-'                           
         CLI   DMCB+4,0            DID IT WORK?                                 
         BE    OV6ERRND            NO                                           
*                                                                               
         MVC   RERROR(2),=AL2(CLOSEINV)                                         
         CLI   WORK+1,0            ANY (-N) TO CLOSE OUT INV HEADER?            
         BE    ENDD03              NO                                           
*                                                                               
         LA    R2,GLBNEFFH                                                      
         MVI   ERROR,INVALID       YES - THERE IS A (-N)                        
         TM    MTFLAG,SRCEFF                                                    
         BZ    ENDD10                                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(L'GLBNEFF),GLBNEFF                                           
         ZIC   RF,GLBNEFFH+5                                                    
         LA    RF,1(RF)                                                         
         STC   RF,GLBNEFFH+5                                                    
         MVC   GLBNEFF+1(L'GLBNEFF-1),DUB                                       
         MVI   GLBNEFF,C'*'                                                     
*                                                                               
         B     OV6ERRND                                                         
*                                                                               
ENDD03   DS    0H                                                               
         TM    MTFLAG,SRCEFF                                                    
         BO    ENDD05                                                           
         CLI   GLBENDH+5,0         NEW END DATE FIELD MUST HAVE A DATE          
         BE    OV6ERR2             NO - IT SHOULD BE                            
*                                                                               
ENDD05   DS    0H                                                               
         CLI   GLBENDH+5,0         ANY END DATE ENTERED?                        
         BNE   ENDD07              NO                                           
         TM    MTFLAG,SRCEFF                                                    
         BO    ENDD07                                                           
         LA    R2,GLBNEFFH                                                      
*&&DO                                                                           
         BNE   *+14                                                             
         MVC   RERROR,=AL2(NOENTRY)                                             
         B     OV6ERR2                                                          
*&&                                                                             
*                                                                               
         CLI   GLBNPROH+5,0        ANY PGM ENTERED?                             
         BNE   ENDD05A                                                          
         CLI   GLBNDAYH+5,0        ANY DAY ENTERED?                             
         BNE   ENDD05A                                                          
         CLI   GLBNTIMH+5,0        ANY TIME ENTERED?                            
         BNE   ENDD05A                                                          
         CLI   GLBBOK1H+5,0        MUST HAVE TRACKS ENTERED                     
         BNE   ENDD06                                                           
         MVC   RERROR,=AL2(NOENTRY)                                             
         B     OV6ERR2                                                          
*                                                                               
ENDD05A  DS    0H                                                               
         CLI   GLBNINVH+5,0        NO INV SHOULD BE ENTERED                     
         BNE   ENDD05E                                                          
         CLI   GLBBOK1H+5,0                                                     
         BNE   ENDD05E                                                          
         OI    MTFLAG,JUSTSRC      CHANGE ONLY SOURCE INV                       
         B     ENDDX                                                            
*                                                                               
ENDD05E  DS    0H                                                               
         MVC   RERROR,=AL2(NOENTRY)                                             
         B     OV6ERR2                                                          
*                                                                               
ENDD06   DS    0H                                                               
         OI    MTFLAG,JUSTTRAK    ADD ONLY TRACKS                               
         B     ENDDX                                                            
*                                                                               
ENDD07   MVI   ERROR,INVALID                                                    
         CLI   GLBENDH+5,0                                                      
         BNE   *+12                                                             
         TM    MTFLAG,SRCEFF                                                    
         BO    ENDDX                                                            
*                                                                               
         GOTO1 DATVAL,DMCB,(0,GLBEND),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    OV6ERRND                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,DTEHLD2+2)                               
         MVC   MTEND,DTEHLD2+2                                                  
         MVC   MTNEFFB,MTEFF                                                    
*                                                                               
         CLI   GLBNEFFH+5,0                                                     
         BNE   ENDDX                                                            
         MVC   MTNEFFB,MTEFF                                                    
         OI    MTFLAG,JUSTSRC                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,MTEFF),(2,WORK)                                   
         CLC   MTEND,WORK                                                       
         BL    OV6RDATE                                                         
*                                                                               
*        GOTO1 =A(OVFLRTN6),DMCB,(2,DUB),(RC),RR=RELO7   (GETPGMS)              
         B     ENDDX                                                            
*                                                                               
ENDD10   DS    0H                                                               
         MVC   RERROR(2),=AL2(ENDINVHD)                                         
         CLI   GLBENDH+5,0         NEW END DATE FIELD MUST BE BLANK             
         BNE   OV6ERR2                                                          
*                                                                               
         MVC   RERROR,=AL2(CLOSE7)                                              
         CLI   WORK+1,1            SHOULD ONLY BE ONE DIGIT                     
         BNE   OV6ERR2                                                          
         CLI   WORK+22,C'1'        MUST BE BETWEEN 1 AND 7                      
         BL    OV6ERR2                                                          
         CLI   WORK+22,C'7'                                                     
         BH    OV6ERR2                                                          
*                                                                               
         XC    NUMDAYS,NUMDAYS                                                  
         MVC   NUMDAYS,WORK+22   # DAYS TO CLOSE PREVIOUS RECORD                
*                                                                               
         LA    RE,GLBNEFF          DATE TO SUBTRACT FROM FOR END DATE           
         ST    RE,DMCB                                                          
         MVC   DMCB(1),WORK        L'INPUT DATE                                 
         LA    RE,WORK2                                                         
         ST    RE,DMCB+4                                                        
         OI    DMCB+4,X'40'                                                     
*                                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 PERVAL,DMCB                                                      
         CLI   DMCB+4,X'04'        VALID SINGLE DATE INPUT?                     
         BNE   OV6ERRND                                                         
*                                                                               
         LA    R3,WORK2                                                         
         USING PERVALD,R3                                                       
*                                                                               
         MVC   NEWEFFB,PVALBSTA    BIN YYMMDD START OF PERIOD                   
         MVC   NEWEFFC,PVALCSTA    BIN COMPRESSED START OF PERIOD               
*                                                                               
         MVC   NEWEFF(8),PVALCPER      NEW EFFECTIVE START DATE                 
         MVC   NEWEFF+8(2),=C'(-'                                               
         MVC   NEWEFF+10(1),NUMDAYS                                             
         MVI   NEWEFF+11,C')'                                                   
*                                                                               
         LA    RE,NEWEFF           DATE TO SUBTRACT FROM FOR END DATE           
         ST    RE,DMCB                                                          
         MVI   DMCB,L'NEWEFF       L'INPUT DATE MMMDD/YY(-N)                    
         LA    RE,WORK2                                                         
         ST    RE,DMCB+4                                                        
         OI    DMCB+4,X'40'                                                     
*                                                                               
         GOTO1 PERVAL,DMCB                                                      
         CLI   DMCB+4,X'04'        VALID SINGLE DATE INPUT?                     
         BNE   OV6ERRND                                                         
         MVC   DTEHLD2+2(2),PVALCSTA     END DATE COMPRESSED                    
         MVC   MTEND,PVALCSTA            NEW END DATE COMPRESSED                
*                                                                               
ENDDX    DS    0H                                                               
         B     OV6EXIT                                                          
         DROP  R3,R6                                                            
         EJECT                                                                  
****************************************************************                
*                GET NEW EFFECTIVE START DATE                  *                
****************************************************************                
NEWEFFD  DS    0H                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R4,2                                                             
*                                                                               
         LA    R2,GLBNEFFH         NEW EFF DATE                                 
         CLI   5(R2),0                                                          
         BE    OV6ERRND                                                         
*                                                                               
NE10     CLI   5(R2),0             MUST BE HERE                                 
         BE    NEWEFFDX                                                         
*                                                                               
         CH    R4,=H'1'            PROCESSING 2ND PROGRAM?                      
         BH    NE15                NO - DOING 1ST PROGRAM                       
         CLI   8(R2),C'='          SAME AS 1ST PROGRAMS?                        
         BNE   NE15                                                             
         MVC   MTNEFF2B,MTNEFFB                                                 
         MVC   MTNEFF2C,MTNEFFC                                                 
         B     NEWEFFDX                                                         
*                                                                               
NE15     DS    0H                                                               
         TM    MTFLAG,SRCEFF       CHANGE SRC EFF DATE ONLY?                    
         BO    NE40                                                             
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),WORK,C',=,-'                                   
         CLI   DMCB+4,0            DID IT WORK?                                 
         BE    OV6ERRND            NO                                           
*                                                                               
         LA    RE,8(R2)                                                         
         ST    RE,DMCB                                                          
         MVC   DMCB(1),WORK        L'INPUT DATE                                 
         LA    RE,WORK2                                                         
         ST    RE,DMCB+4                                                        
         OI    DMCB+4,X'40'                                                     
*                                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 PERVAL,DMCB                                                      
         CLI   DMCB+4,X'04'        VALID SINGLE DATE INPUT?                     
         BNE   OV6ERRND                                                         
*                                                                               
         LA    R3,WORK2                                                         
         USING PERVALD,R3                                                       
*                                                                               
         MVC   NEWEFFB,PVALBSTA    BIN YYMMDD START OF PERIOD                   
         MVC   NEWEFFC,PVALCSTA    BIN COMPRESSED START OF PERIOD               
*                                                                               
         CH    R4,=H'1'            PROCESSING 2ND PROGRAM?                      
         BH    NE20                NO - DOING 1ST PROGRAM                       
         MVC   MTNEFF2B,PVALBSTA   NEW EFF START DATE (BINARY)                  
         MVC   MTNEFF2C,PVALCSTA   NEW EFF START DATE (COMPRESSED)              
         B     NE30                                                             
*                                                                               
NE20     MVC   MTNEFFB,PVALBSTA    NEW EFF START DATE (BINARY)                  
         MVC   MTNEFFC,PVALCSTA    NEW EFF START DATE (COMPRESSED)              
*                                                                               
NE30     LA    R2,GLBNEF2H         2ND PGM EFFECTIVE DATE                       
         BCT   R4,NE10                                                          
         B     NEWEFFDX                                                         
*                                                                               
NE40     DS    0H                  CHANGE SRC EFF ONLY                          
         MVI   ERROR,INVALID                                                    
         GOTO1 DATVAL,DMCB,(0,GLBNEFF),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    OV6ERRND                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,DTEHLD)                                  
         GOTO1 DATCON,DMCB,(0,WORK),(3,MTNEFFB)                                 
         MVC   NEWEFFB,MTNEFFB     BIN YYMMDD START OF PERIOD                   
         MVC   NEWEFFC,DTEHLD      COMPRESSED START OF PERIOD                   
         MVC   MTNEFFC,DTEHLD      COMPRESSED START OF PERIOD                   
         MVC   PGMNEND,MTEND       END DATE COMPRESSED                          
*                                                                               
NEWEFFDX B     OV6EXIT                                                          
         DROP  R3,R6                                                            
         EJECT                                                                  
****************************************************************                
*                GET NEW PROGRAM NAMES                         *                
****************************************************************                
GETPGMS  DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        GET PROGRAM ELEMENT                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING RIPGELEM,R6                                                      
         MVI   ERROR,MISSING                                                    
*                                                                               
         LA    R2,GLBNPROH         PROGRAM 1                                    
         CLI   5(R2),0             SAME AS ORIGINAL?                            
         BNE   GP10                NO                                           
*                                                                               
         ZIC   RF,1(R6)            L'PROGRAM ELEMENT                            
         SH    RF,=H'2'            OVERHEAD BYTES                               
         STC   RF,MTPGM1LN         L'PROGRAM NAME                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MTPGM1(0),RIPGNAME     YES                                       
         B     GP30                                                             
*                                                                               
GP10     MVC   MTPGM1,8(R2)        MOVE IN PROGRAM FROM SCREEN                  
         MVC   MTPGM1LN,5(R2)      L'PROGRAM NAME                               
*                                                                               
GP30     DS    0H                                                               
         LA    R2,GLBNPR2H         PROGRAM 2                                    
         CLI   5(R2),0             SAME AS ORIGINAL                             
         BNE   GP35                NO                                           
*                                                                               
         OC    MTNEFF2B,MTNEFF2B   ANY 2ND DATE?                                
         BZ    GETPGMX             YES                                          
*                                                                               
         ZIC   RF,1(R6)            L'PROGRAM ELEMENT                            
         SH    RF,=H'2'            OVERHEAD BYTES                               
         STC   RF,MTPGM2LN         L'PROGRAM NAME                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MTPGM2(0),RIPGNAME                                               
         B     GETPGMX                                                          
*                                                                               
GP35     CLI   8(R2),C'='          SAME NAME AS PREVIOUS?                       
         BNE   GP40                NO                                           
         MVC   MTPGM2,MTPGM1       YES                                          
         MVC   MTPGM2LN,MTPGM1LN   L'PROGRAM NAME                               
         B     GETPGMX                                                          
*                                                                               
GP40     MVC   MTPGM2,8(R2)        MOVE IN PROGRAM FROM SCREEN                  
         MVC   MTPGM2LN,5(R2)      L'PROGRAM NAME                               
*                                                                               
GETPGMX  DS    0H                                                               
         B     OV6EXIT                                                          
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*                GET NEW DAYS                                  *                
****************************************************************                
GETDAYS  DS    0H                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
         MVC   PGMNOINV,RINVOINV   SAVE OFF OLD FORMAT FROM ORIG REC            
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'02'        GET DAY TIME ELEMENT                         
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING RIDTELEM,R6                                                      
         MVI   ERROR,INVALID                                                    
*                                                                               
         LA    R2,GLBNDAYH         1ST NEW DAY                                  
         CLI   5(R2),0             NEW DAY?                                     
         BNE   GD10                YES                                          
         MVC   MTDAY1,RIDTDAY      NO - SAME DAY                                
         MVC   MTDCODE1,PGMNOINV+1     SAME DAY CODE                            
         B     GD20                                                             
*                                                                               
GD10     DS    0H                                                               
         ZIC   R3,5(R2)            L'NEW DAY                                    
         GOTO1 DAYVAL,DMCB,((R3),8(R2)),MTDAY1,WORK                             
         CLI   MTDAY1,0                                                         
         BE    OV6ERRND                                                         
*                                                                               
         GOTO1 VINVDAY,DMCB,((R3),8(R2)),MTDCODE1,WORK,DAYVAL                   
         CLI   MTDCODE1,0                                                       
         BE    OV6ERRND                                                         
*                                                                               
GD20     DS    0H                                                               
         CLI   MTPGM2LN,0          ANY 2ND PROGRAM?                             
         BE    GETDAYX             NO - EXIT                                    
         LA    R2,GLBNDA2H         2ND NEW DAY                                  
         CLI   5(R2),0             NEW DAY?                                     
         BNE   GD30                YES                                          
         MVC   MTDAY2,RIDTDAY      NO - SAME DAY                                
         MVC   MTDCODE2,PGMNOINV+1     SAME DAY CODE                            
         B     GETDAYX                                                          
*                                                                               
GD30     DS    0H                                                               
         CLI   8(R2),C'='          SAME AS NEW DAY 1?                           
         BNE   GD35                                                             
         MVC   MTDAY2,MTDAY1                                                    
         MVC   MTDCODE2,MTDCODE1                                                
         B     GETDAYX                                                          
*                                                                               
GD35     ZIC   R3,5(R2)            L'NEW DAY                                    
         GOTO1 DAYVAL,DMCB,((R3),8(R2)),MTDAY2,WORK                             
         CLI   MTDAY2,0                                                         
         BE    OV6ERRND                                                         
*                                                                               
         GOTO1 VINVDAY,DMCB,((R3),8(R2)),MTDCODE2,WORK,DAYVAL                   
         CLI   MTDCODE2,0                                                       
         BE    OV6ERRND                                                         
*                                                                               
GETDAYX  DS    0H                                                               
         B     OV6EXIT                                                          
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*                GET NEW TIMES                                 *                
****************************************************************                
GETTIMES DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        GET DAY TIME ELEMENT                         
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING RIDTELEM,R6                                                      
         MVI   ERROR,INVALID                                                    
         MVC   SVTIME,RIDTTIME     SAVE OF TIME                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         LA    R2,GLBNTIMH         NEW TIME FOR 1ST PROGRAM                     
         CLI   5(R2),0                                                          
         BNE   GT20                                                             
         MVC   MTTIME1,SVTIME                                                   
         MVC   MTQTR1,RINVOINV                                                  
         MVC   MTLEN1,RINVOINV+2                                                
         B     GT30                                                             
*                                                                               
GT20     DS    0H                                                               
         LA    R2,GLBNTIMH         NEW TIME FOR 1ST PROGRAM                     
         BAS   RE,VALTIME                                                       
         MVC   MTTIME1,PGMNTIME                                                 
         MVC   MTQTR1,PGMNOINV                                                  
         MVC   MTLEN1,PGMNOINV+2                                                
*                                                                               
GT30     GOTO1 VHRTOQH,DMCB,(0,MTTIME1),TEMPSQHR                                
         GOTO1 VHRTOQH,DMCB,(0,MTTIME1+2),TEMPEQHR                              
         ZIC   RF,TEMPSQHR                                                      
         ZIC   RE,TEMPEQHR                                                      
         SR    RE,RF                                                            
         STC   RE,MTDURAT1         DURATION IN QTR HOURS                        
*                                                                               
GT40     DS    0H                                                               
         CLI   MTPGM2LN,0          ANY 2ND PROGRAM?                             
         BE    GETTIMEX                                                         
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         LA    R2,GLBNTI2H         NEW TIME FOR 2ND PROGRAM                     
         CLI   5(R2),0                                                          
         BNE   GT60                                                             
         MVC   MTTIME2,SVTIME                                                   
         MVC   MTQTR2,RINVOINV                                                  
         MVC   MTLEN2,RINVOINV+2                                                
         B     GT90                                                             
*                                                                               
GT60     DS    0H                                                               
         CLI   8(R2),C'='          SAME AS 1ST PGM?                             
         BNE   GT80                                                             
         MVC   MTTIME2,MTTIME1                                                  
         MVC   MTQTR2,MTQTR1                                                    
         MVC   MTLEN2,MTLEN1                                                    
         MVC   MTDURAT2,MTDURAT1                                                
         B     GETTIMEX                                                         
*                                                                               
GT80     DS    0H                                                               
         LA    R2,GLBNTI2H         NEW TIME FOR 1ST PROGRAM                     
         BAS   RE,VALTIME                                                       
         MVC   MTTIME2,PGMNTIME                                                 
         MVC   MTQTR2,PGMNOINV                                                  
         MVC   MTLEN2,PGMNOINV+2                                                
*                                                                               
GT90     GOTO1 VHRTOQH,DMCB,(0,MTTIME2),TEMPSQHR                                
         GOTO1 VHRTOQH,DMCB,(0,MTTIME2+2),TEMPEQHR                              
         ZIC   RF,TEMPSQHR                                                      
         ZIC   RE,TEMPEQHR                                                      
         SR    RE,RF                                                            
         STC   RE,MTDURAT2         DURATION IN QTR HOURS                        
*                                                                               
GETTIMEX DS    0H                                                               
         B     OV6EXIT                                                          
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*      VALIDATE THE TIME FIELD (R2 POINTS TO TIME FIELD)       *                
****************************************************************                
VALTIME  NTR1                                                                   
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVC   SVKLEN,RINVKLEN     SAVE OFF PROGRAM LENGTH                      
*                                                                               
         CLI   5(R2),0             ANY TIME HERE?                               
         BNE   VTM40                                                            
         MVC   PGMNTIME,SVTIME                                                  
         GOTO1 =A(OVFLRTN2),DMCB,(4,DUB),(RC),RR=RELO7     (GETQTR)             
         B     VTM80                                                            
*                                                                               
VTM40    MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'N'          NONE                                         
         BE    OV6ERRND                                                         
         CLI   8(R2),C'V'            VARIOUS ARE NOT VALID                      
         BE    OV6ERRND                                                         
*                                                                               
         ZIC   R5,5(R2)            LENGTH OF EXPRESSION                         
         LA    R3,6(R5,R2)                                                      
         CLC   0(2,R3),=C',B'                                                   
         BNE   *+8                                                              
         SH    R5,=H'2'                                                         
         GOTO1 TIMVAL,DMCB,((R5),8(R2)),PGMNTIME                                
         CLI   DMCB,X'FF'                                                       
         BE    OV6ERRND                                                         
*                                                                               
         GOTO1 =A(OVFLRTN2),DMCB,(4,DUB),(RC),RR=RELO7     (GETQTR)             
         CLC   0(2,R3),=C',B'                                                   
         BNE   VTM80                                                            
         OC    PGMNTIME+2(2),PGMNTIME+2                                         
         BNZ   OV6ERRND                                                         
         CLI   SVKLEN,0                                                         
         BNE   VTMEX                                                            
         MVI   SVKLEN,C'0'                                                      
         B     VTMEX                                                            
*                                                                               
VTM80    CLC   PGMNTIME+2(2),=C'CC'                                             
         BNE   VTM100                                                           
         CLI   SVKLEN,0                                                         
         BNE   VTMEX                                                            
         MVI   SVKLEN,C'9'                                                      
         B     VTMEX                                                            
*                                                                               
VTM100   MVC   HALF,PGMNTIME       START TIME TO MINUTES                        
         BAS   RE,TOMIN                                                         
         LH    R5,HALF             START MINUTE TO R5                           
*                                                                               
         MVC   HALF,PGMNTIME+2     END TIME TO MINUTES                          
         BAS   RE,TOMIN                                                         
         LH    R3,HALF             END MINUTES                                  
*                                                                               
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,30(R5)           IF NO END / ADD 30 TO START                  
         LR    RF,R3               END TIME MINUTES                             
*                                                                               
         CH    RF,=H'1440'                                                      
         BNH   *+8                                                              
         SH    RF,=H'1440'         PAST MIDNIGHT                                
         XR    RE,RE                                                            
*                                                                               
         D     RE,=F'60'           GET MILITARY END FROM MINUTES                
         MH    RF,=H'100'                                                       
         AR    RF,RE                                                            
         STH   RF,HALF                                                          
         MVC   PGMNTIME+2(2),HALF                                               
*                                                                               
         CR    R5,R3               START/END MINUTES                            
         BNH   *+8                                                              
         AH    R3,=H'1440'         ADD 24 X 60 TO END                           
*                                                                               
         SR    R3,R5               END - START                                  
         LR    RF,R3                                                            
         XR    RE,RE                                                            
         D     RE,=F'30'           GET NUMBER 1/2 HOURS                         
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         AH    RF,=H'1'            ADD 1 TO HALF HOURS                          
         STC   RF,SVKLEN                                                        
*                                                                               
         LA    R3,10               GET CODE FROM LENGTH TABLE                   
         LA    R5,LENGTH                                                        
         CLC   SVKLEN,0(R5)                                                     
         BNH   *+14                                                             
         LA    R5,2(R5)                                                         
         BCT   R3,*-14                                                          
         DC    H'0'                                                             
         MVC   SVKLEN,1(R5)                                                     
         MVC   PGMNOINV+2(1),SVKLEN                                             
*                                                                               
VTMEX    DS    0H                                                               
         B     OV6EXIT                                                          
         DROP  R6                                                               
*                                                                               
LENGTH   DC    AL1(01),C'0'        UP TO 1/2 HOUR                               
         DC    AL1(02),C'1'        FROM MORE THAN 1/2  TO     1 HOUR            
         DC    AL1(03),C'2'                         1     1 1/2                 
         DC    AL1(04),C'3'                     1 1/2         2                 
         DC    AL1(05),C'4'                         2     2 1/2                 
         DC    AL1(06),C'5'                     2 1/2         3                 
         DC    AL1(08),C'6'                         3         4                 
         DC    AL1(12),C'7'                         4         6                 
         DC    AL1(16),C'8'                         6         8                 
         DC    AL1(99),C'9'        OVER 8 HOURS                                 
         EJECT                                                                  
*                                                                               
TOMIN    NTR1                                                                   
         XR    RE,RE                                                            
         LH    RF,HALF                                                          
         D     RE,=F'100'                                                       
         MH    RF,=H'60'                                                        
         AR    RE,RF                                                            
         STH   RE,HALF                                                          
         B     OV6EXIT                                                          
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
OV6EXIT  XMOD1 1                                                                
RELO7    DS    A                                                                
OV6ERRND DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 ERREX                                                            
OV6ERR2  DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 MYERROR                                                          
OV6RDATE DS    0H                                                               
         MVI   ERROR,64            END DATE > START DATE                        
         LA    R2,GLBENDH                                                       
         GOTO1 ERREX                                                            
                                                                                
OV6REP   DC    CL8'REPFILE'                                                     
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*                 OVERFLOW ROUTINES                             *               
*****************************************************************               
OVFLRTN7 NMOD1 0,*RM10OV7*,RR=R5                                                
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING OVFLRTN6+4096,RA                                                 
*                                                                               
         L     RC,4(R1)                                                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R5,RELO8                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRNCH7(RF)                                                     
*                                                                               
OVBRNCH7 B     CHEFF                                                            
****************************************************************                
*        CHANGE EFFECTIVE DATE                                                  
*        AIO1=  SOURCE HEADER RECORD                                            
*        MTNEFFB(C)= NEW START DATE                                             
*        MTEND= NEW END DATE                                                    
****************************************************************                
CHEFF    DS    0H                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         TM    RINVGPRO,X'10'      GLOBALLY PROTECTED?                          
         BZ    *+12                                                             
         OI    MYFLAG3,GLBLPROT                                                 
         B     CHEFFX                                                           
*                                                                               
         XC    DYPART,DYPART                                                    
         MVC   DYPART,RINVDP       SAVE AWAY DAYPARTS                           
*                                                                               
CHEFF10  DS    0H                  BUILD TABLE OF KEYS TO COPY OVER             
         L     R4,AIO3                                                          
         USING SRCKEYS,R4                                                       
*                                                                               
         XC    SRCKFLG,SRCKFLG                                                  
         MVC   SRCKEY,KEY          SAVE AWAY SOURCE KEY                         
         XC    RELOKEY,RELOKEY                                                  
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,MTEFF                                                   
*                                                                               
         GOTO1 HIGH                                                             
         B     CHEFF20                                                          
*                                                                               
CHEFFSEQ DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
CHEFF20  DS    0H                                                               
         CLC   KEY(24),KEYSAVE     STILL SAME SET OF RMP RECS?                  
         BNE   CHEFF30                                                          
*                                                                               
         MVC   SRCSRC,RINVKSRC                                                  
         MVC   SRCBK,RINVKBK                                                    
*                                                                               
         LA    R4,SRCKEYLQ(R4)                                                  
         B     CHEFFSEQ                                                         
*                                                                               
CHEFF30  DS    0H                                                               
         MVC   0(SRCKEYLQ,R4),=XL3'FFFFFF'                                      
         L     R4,AIO3                                                          
*                                                                               
CHEFF40  DS    0H                  CHECK THE NEW EFF DATE                       
         CLC   0(3,R4),=XL3'FFFFFF'                                             
         BE    CHEFF100                                                         
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,MTNEFFB                                                 
         MVC   RINVKSRC,SRCSRC                                                  
         MVC   RINVKBK,SRCBK                                                    
*                                                                               
         XC    SRCKFLG,SRCKFLG                                                  
*                                                                               
         MVC   KEYSAVE(27),KEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',OV7RDHI),=C'REPDIR  ',KEY,KEY                
         TM    DMCB+8,X'02'        RECORD IS DELETED?                           
         BZ    *+8                                                              
         OI    SRCKFLG,SRCDEL+SRCPUT                                            
*                                                                               
         CLC   KEY(27),KEYSAVE     DOES THIS REC ALREADY EXIST?                 
         BNE   *+12                                                             
         OI    SRCKFLG,SRCPUT                                                   
         B     CHEFF50                                                          
*                                                                               
         OI    SRCKFLG,SRCADD                                                   
*                                                                               
CHEFF50  DS    0H                                                               
         LA    R4,SRCKEYLQ(R4)                                                  
         B     CHEFF40                                                          
*                                                                               
CHEFF100 DS    0H                  ADD/PUT RECORDS                              
         L     R4,AIO3                                                          
*                                                                               
CHEFF110 DS    0H                  CHECK THE NEW EFF DATE                       
         CLC   0(3,R4),=XL3'FFFFFF'                                             
         BE    CHEFF200                                                         
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,MTEFF                                                   
         MVC   RINVKSRC,SRCSRC                                                  
         MVC   RINVKBK,SRCBK                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     DOES THIS REC ALREADY EXIST?                 
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         TM    SRCKFLG,SRCDEL        DOES THIS EXIST AND DELETED?               
         BZ    CHEFF120                                                         
         TM    SRCKFLG,SRCPUT                                                   
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,MTNEFFB                                                 
         MVC   RINVKSRC,SRCSRC                                                  
         MVC   RINVKBK,SRCBK                                                    
*                                                                               
         MVC   KEYSAVE(27),KEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'88',OV7RDHI),=C'REPDIR  ',KEY,KEY                
         TM    DMCB+8,X'02'        RECORD IS DELETED?                           
         BO    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         NI    RINVKEY+27,X'FF'-X'80'    MARK UNDELETED                         
         GOTO1 =A(OVFLRTN3),DMCB,(4,DUB),(RC),RR=RELO8   (MYDIRWRT)             
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'88',=C'GETREC'),=C'REPFILE ',KEY+28,AIO,+        
               DMWORK                                                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
*!!!     GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   RINVKSTD,MTNEFFB       NEW EFF START DATE                        
         MVC   RINVPEFF(2),MTNEFFC    NEW EFF START DATE                        
         MVC   RINVPEFF+2(2),MTEND    NEW END DATE                              
         NI    RINVCNTL,X'FF'-X'80'   MARK UNDELETED                            
*                                                                               
         MVC   RELOKEY,KEY                                                      
         GOTO1 =A(OVFLRTN3),DMCB,(3,DUB),(RC),RR=RELO8   (MYFILWRT)             
         B     CHEFF150                                                         
*                                                                               
CHEFF120 DS    0H                                                               
         TM    SRCKFLG,SRCPUT      JUST PUT THIS RECORD?                        
         BZ    CHEFF130                                                         
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,MTNEFFB                                                 
         MVC   RINVKSRC,SRCSRC                                                  
         MVC   RINVKBK,SRCBK                                                    
*                                                                               
         MVC   KEYSAVE(27),KEY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'88',OV7RDHI),=C'REPDIR  ',KEY,KEY                
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   RINVKSTD,MTNEFFB       NEW EFF START DATE                        
         MVC   RINVPEFF(2),MTNEFFC    NEW EFF START DATE                        
         MVC   RINVPEFF+2(2),MTEND    NEW END DATE                              
*                                                                               
         MVC   RELOKEY,KEY                                                      
         GOTO1 =A(OVFLRTN3),DMCB,(3,DUB),(RC),RR=RELO8   (MYFILWRT)             
         B     CHEFF150                                                         
*                                                                               
CHEFF130 DS    0H                                                               
         TM    SRCKFLG,SRCADD      ADD THIS RECORD?                             
         BO    *+6                                                              
         DC    H'00'               MUST BE FLAGGED                              
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,MTNEFFB                                                 
         MVC   RINVKSRC,SRCSRC                                                  
         MVC   RINVKBK,SRCBK                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R6,AIO                                                           
         MVC   RINVKSTD,MTNEFFB       NEW EFF START DATE                        
         MVC   RINVPEFF(2),MTNEFFC    NEW EFF START DATE                        
         MVC   RINVPEFF+2(2),MTEND    NEW END DATE                              
*                                                                               
         MVC   RELOKEY,KEY                                                      
         GOTO1 =A(OVFLRTN3),DMCB,(2,DUB),(RC),RR=RELO8   (MYFILADD)             
         B     CHEFF150                                                         
*                                                                               
CHEFF150 DS    0H                                                               
         CLI   RINVKSRC,0          HEADER?                                      
         BNE   CHEFF160                                                         
*                                                                               
         BAS   RE,PROCPSV          ADD/DELETE PASSIVE KEYS                      
*                                                                               
CHEFF160 DS    0H                                                               
         LA    R4,SRCKEYLQ(R4)                                                  
         B     CHEFF110                                                         
*                                                                               
CHEFF200 DS    0H                  DELETE SOURCE HEADERS                        
         L     R4,AIO3                                                          
*                                                                               
CHEFF210 DS    0H                                                               
         CLC   0(3,R4),=XL3'FFFFFF'                                             
         BE    CHEFF300                                                         
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STAHLD                                                  
         MVC   RINVKINV,INVHLD                                                  
         MVC   RINVKSTD,MTEFF                                                   
         MVC   RINVKSRC,SRCSRC                                                  
         MVC   RINVKBK,SRCBK                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     DOES THIS REC ALREADY EXIST?                 
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,KEY                                                           
         OI    RINVKEY+27,X'80'          MARK DELETED                           
         GOTO1 =A(OVFLRTN3),DMCB,(4,DUB),(RC),RR=RELO8   (MYDIRWRT)             
*                                                                               
         L     R6,AIO                                                           
         OI    RINVCNTL,X'80'            MARK DELETED                           
         GOTO1 =A(OVFLRTN3),DMCB,(3,DUB),(RC),RR=RELO8   (MYFILWRT)             
         B     CHEFF250                                                         
*                                                                               
CHEFF250 DS    0H                                                               
         LA    R4,SRCKEYLQ(R4)                                                  
         B     CHEFF210                                                         
*                                                                               
CHEFF300 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RELOKEY                                                  
*                                                                               
CHEFFX   DS    0H                                                               
         B     OV7EXIT                                                          
         DROP  R6                                                               
****************************************************************                
*              ADD/DELETE PASSIVE POINTER                                       
****************************************************************                
PROCPSV  NTR1                                                                   
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   BSVDA,KEY+28        DISK ADDRESS                                 
*                                                                               
         LA    R4,WORK2                                                         
         USING RIDPKEY,R4                                                       
         XC    WORK2,WORK2                                                      
*                                                                               
         LA    R3,DYPART                                                        
*                                                                               
PPSV10   DS    0H                                                               
         CLI   0(R3),X'FF'         NO MORE DAYPARTS?                            
         BE    PPSV100                                                          
         CLI   0(R3),X'40'         ONLY SPACE HERE?                             
         BE    PPSV100                                                          
*                                                                               
         MVI   RIDPKTYP,X'92'                                                   
         MVC   RIDPKREP,RINVKREP                                                
         MVC   RIDPKSTA,RINVKSTA                                                
         MVC   RIDPKDPT,0(R3)                                                   
         MVC   RIDPKINV,RINVKINV                                                
         MVC   RIDPKSTD,RINVKSTD                                                
*                                                                               
*  IF SELF ASSIGNED GET NEXT DAYPART                                            
*  ONLY COMPUTER GENERATED NUMBERS GET THE DAY,QTR HOUR                         
*  AND THE LENGTH FILLED IN.                                                    
*                                                                               
         TM    RINVSTAT,X'80'                                                   
         BO    PPSV50              BIT ON SELF ASSIGNED                         
*                                                                               
         MVC   RIDPKDAY,RINVOINV+1   MOVE DAY CODE,                             
         MVC   RIDPKQTR,RINVOINV     QUARTER HOUR,                              
         MVC   RIDPKLEN,RINVOINV+2   AND PROGRAM LENGTH TO KEY                  
*                                                                               
         LA    RE,EFFDATE          SPECIAL DAYPARTS                             
*                                                                               
PPSV30   DS    0H                                                               
         CLI   0(RE),X'FF'                                                      
         BE    PPSV50                                                           
         CLC   0(1,R3),0(RE)                                                    
         BE    PPSV40                                                           
         LA    RE,1(RE)                                                         
         B     PPSV30                                                           
*                                                                               
PPSV40   DS    0H                                                               
         XC    RIDPKDAY,RIDPKDAY                                                
         MVC   RIDPKDTE,RINVPEFF                                                
*                                                                               
PPSV50   DS    0H                                                               
         LA    R3,1(R3)                                                         
         LA    R4,32(R4)                                                        
         B     PPSV10                                                           
*                                                                               
PPSV100  DS    0H                                                               
         LA    R4,WORK2                                                         
*                                                                               
PPSV110  DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    PPSV150             END OF LIST                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),0(R4)                                                    
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                READ FOR DELETES AS WELL                     
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY ALREADY EXISTS                           
         BE    PPSV120                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(28),0(R4)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         GOTO1 =A(OVFLRTN3),DMCB,(5,DUB),(RC),RR=RELO8   (MYDIRADD)             
         MVC   RELOKEY,KEY                                                      
         B     PPSV130                                                          
*                                                                               
PPSV120  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(28),0(R4)                                                    
         MVC   KEY+28(4),BSVDA                                                  
         MVC   RELOKEY,KEY                                                      
         GOTO1 =A(OVFLRTN3),DMCB,(4,DUB),(RC),RR=RELO8   (MYDIRWRT)             
*                                                                               
PPSV130  DS    0H                                                               
         LA    R4,32(R4)                                                        
         B     PPSV110                                                          
*                                                                               
PPSV150  DS    0H                  MARK ORIGINAL PASSIVES DELETED               
         LA    R4,WORK2                                                         
*                                                                               
PPSV160  DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    PPSV200             END OF LIST                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(24),0(R4)                                                    
         MVC   KEY+24(3),MTEFF     ORIGINAL EFF DATE                            
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                READ FOR DELETES AS WELL                     
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         CLC   KEYSAVE(27),KEY     KEY ALREADY EXISTS                           
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         OI    KEY+27,X'80'        MARK PASSIVE DELETED                         
         GOTO1 =A(OVFLRTN3),DMCB,(4,DUB),(RC),RR=RELO8   (MYDIRWRT)             
*                                                                               
         LA    R4,32(R4)                                                        
         B     PPSV160                                                          
*                                                                               
PPSV200  DS    0H                                                               
*                                                                               
PROCPSVX DS    0H                                                               
         XIT1                                                                   
         DROP  R4,R6                                                            
*                                                                               
EFFDATE  DC    C'VSJ',X'FF'                                                     
         EJECT                                                                  
****************************************************************                
OV7EXIT  XMOD1 1                                                                
RELO8    DS    A                                                                
OV7ERRND DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 ERREX                                                            
OV7ERR2  DS    0H                                                               
         ST    R2,ACURFORC                                                      
         GOTO1 MYERROR                                                          
*                                                                               
OV7RDHI  DC    CL8'DMRDHI'                                                      
OV7REP   DC    CL8'REPFILE'                                                     
*                                                                               
SRCKEY   DS    XL27                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*                                                                   *           
* AUDIT  (AUDIT TRAIL MAINTAINANCE)   ADDED 9/00 BY FDOY AS NEW     *           
*                                     AUDIT STREAM, YES IT IS A     *           
*                                     BIT INEFFICIENT, BUT WE NEED  *           
*                                 A WAY TO TRACK RECORD CORRUPTION  *           
*********************************************************************           
AUDIT    NTR1  BASE=*,LABEL=*                                                   
         XC    AUDITFLD,AUDITFLD        CLEAR AUDITITNG FIELD                   
                                                                                
         LA    R2,AUDITFLD                                                      
         USING INVAUDIT,R2                                                      
                                                                                
         L     R3,AIO                                                           
         USING RINVREC,R3                                                       
                                                                                
         MVI   IAUDPRG#,10              THIS WAS AN RMP UPDATE                  
         MVC   IAUDREC#,RECNUM          RECORD #                                
         MVC   IAUDACT#,ACTNUM          ACTION #                                
                                                                                
         CLC   AIO,AIO1                 RECORD IN AIO1?                         
         BNE   AUD010                   NO                                      
         OI    IAUDFLG1,AUDIO1          YES, MARK IT                            
         B     AUD030                                                           
                                                                                
AUD010   CLC   AIO,AIO2                 RECORD IN AIO2?                         
         BNE   AUD020                   NO                                      
         OI    IAUDFLG1,AUDIO2          YES, MARK IT                            
         B     AUD030                                                           
                                                                                
AUD020   CLC   AIO,AIO3                 RECORD IN AIO1?                         
         BNE   AUD030                   NO                                      
         OI    IAUDFLG1,AUDIO3          YES, MARK IT                            
                                                                                
AUD030   CLI   RINVKSRC,X'FF'      IS THIS A TEXT RECORD?                       
         BNE   AUD060              NO, SKIP NEXT                                
                                                                                
******                             YES, TEST IF COMPRESSED DATE                 
*                                       IS PLACED IN RINVPEFF OFFSET            
*                                                                               
         GOTO1 DATCON,DMCB,(3,RINVKSTD),(2,AUDITDAT)  GET CMPRSSED DATE         
         CLC   AUDITDAT,RINVPEFF        CHK IF IT IS PLACED AT THIS LOC         
         BNE   AUD060                   IF NO , WE ARE PROBABLY OK              
         OI    IAUDFLG2,TXTCRPT1        IF YES, SET FLAG                        
*                                          BECAUSE THIS IS BAD MOJO             
*                                                                               
******                                                                          
                                                                                
**           CALL REPFACS  AND UPDATE AUDIT TRAIL                               
                                                                                
AUD060   GOTO1 (RFAUDIT,VREPFACS),DMCB,AIO,(10,ACOMFACS),AUDITFLD,(RA)          
                                                                                
         XIT1                                                                   
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
****************************************************************                
SRCKEYS  DSECT                                                                  
SRCSRC   DS    XL1                                                              
SRCBK    DS    XL2                                                              
SRCKFLG  DS    XL1                                                              
SRCADD   EQU   X'01'               ADD THIS REC                                 
SRCPUT   EQU   X'02'               PUT THIS REC                                 
SRCDEL   EQU   X'04'               KEY IS DELETED                               
SRCKEYLQ EQU   *-SRCSRC                                                         
*                                                                               
PLINED   DSECT                                                                  
PRINVNUM DS    CL4                                                              
         DS    CL3                                                              
PREFFDTE DS    CL17                                                             
         DS    CL3                                                              
PRPRGNM  DS    CL27                                                             
         DS    CL3                                                              
PRDYTIME DS    CL20                                                             
         DS    CL3                                                              
PRDAYPT  DS    CL7                                                              
         DS    CL3                                                              
PRAVDAY  DS    CL11                                                             
         DS    CL3                                                              
PRAVTIME DS    CL11                                                             
         SPACE 2                                                                
LLINED   DSECT                                                                  
LINVNUM  DS    CL6                                                              
         DS    CL1                                                              
LEFFDTE  DS    CL17                                                             
         DS    CL1                                                              
LPROGRM  DS    CL20                                                             
         DS    CL1                                                              
LDPT     DS    CL6                                                              
         DS    CL1                                                              
LDYTIME  DS    CL18                                                             
*                                                                               
TTABD    DSECT                     DSECT TO MAP OVER TRACKTAB                   
TTBOOKLN DS    XL1                 L'BOOKS FIELD                                
TTBOOKS  DS    CL17                BOOKS (OFF SCREEN)                           
TTSRCEBK DS    XL3                 SOURCE BOOK                                  
TTSRCEPG DS    XL3                 SOURCE PROGRAM NUMBER                        
TTPCODE1 DS    CL2                 PGM 1 CODE                                   
TTPCDE1B DS    B                   CONTROL BITS FOR PROGRAM CODE                
TTPCODE2 DS    CL2                 PGM 2 CODE                                   
TTPCDE2B DS    B                   CONTROL BITS FOR PROGRAM CODE                
TTUPTLN  DS    XL1                 L'UPT PART                                   
TTUPT    DS    CL13                UPT                                          
TTOPTBK  DS    XL4                 OPTIONS BOOK                                 
TTTABLN  EQU   *-TTBOOKS           LENGTH OF 1 LINE OF ENTRY                    
*                                                                               
         EJECT                                                                  
* REPFACSQ                                                                      
* RERMPAUD                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RERMPFFD                                                                      
* RERMPC4D                                                                      
* RERMPC5D                                                                      
* RERMPC6D                                                                      
* DDGENTWA                                                                      
* RERMPWTWA                                                                     
* DEDBEXTRAD                                                                    
* REGENINVA                                                                     
* RERMPWORKD                                                                    
* REGENVAL                                                                      
* DEDBLOCK                                                                      
* DEDEMFILE                                                                     
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* REBKLSTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE REPFACSQ         ROUTINE # EQUATES FOR REPFACS                 
       ++INCLUDE RERMPAUD         INVENTORY AUDIT FIELD DSECT                   
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RERMPFFD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                  USES TTABD DSECT (ABOVE)                     
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPC4D          KEY FIELDS                                   
         EJECT                                                                  
*                                                                               
         ORG   GL1HEREH                                                         
       ++INCLUDE RERMPC5D          REQUEST SCREEN                               
         EJECT                                                                  
*                                                                               
         ORG   GL1HEREH                                                         
       ++INCLUDE RERMPC6D          PROCESSING SCREEN                            
         EJECT                                                                  
*                                                                               
         ORG   GLBWORK+100                                                      
MAINTAB  DS    0XL104                                                           
MTSCRN   DS    XL1                 CURRENT SCREEN                               
MTINV    DS    CL4                 ORIGINAL INVENTORY #                         
MTEFF    DS    XL3                 ORIGINAL EFFECTIVE DATE                      
MTEND    DS    XL2                 NEW END DATE                                 
MTEND2   DS    XL2                 NEW END DATE FOR 1ST PROGRAM                 
MTNEFFB  DS    XL3                 NEW EFF START DATE (BINARY)                  
MTNEFFC  DS    XL2                 NEW EFF START DATE (COMPRESSED)              
MTNEFF2B DS    XL3                 NEW EFF START DATE (BINARY)                  
MTNEFF2C DS    XL2                 NEW EFF START DATE (COMPRESSED)              
*                                                                               
MTPGM1LN DS    XL1                 L'NAME OF 1ST PROGRAM                        
MTPGM1   DS    CL27                NAME OF 1ST PROGRAM                          
MTDAY1   DS    XL1                 NEW DAY                                      
MTTIME1  DS    XL4                 NEW TIME                                     
MTDURAT1 DS    XL1                 DURATION                                     
MTINV1   DS    CL4                 NEW INVENTORY #                              
MTOINV1  DS    0XL3                OLD FORMAT INVENTORY #                       
MTQTR1   DS    CL1                    QUARTER HOUR NO.                          
MTDCODE1 DS    CL1                    DAY CODE                                  
MTLEN1   DS    CL1                    PROGRAM LENGTH                            
*                                                                               
MTPGM2LN DS    XL1                 L'NAME OF 2ND PROGRAM                        
MTPGM2   DS    CL27                NAME OF 2ND PROGRAM                          
MTDAY2   DS    XL1                 NEW DAY                                      
MTTIME2  DS    XL4                 NEW TIME                                     
MTDURAT2 DS    XL1                 DURATION                                     
MTINV2   DS    CL4                 NEW INVENTORY #                              
MTOINV2  DS    0XL3                OLD FORMAT INVENTORY #                       
MTQTR2   DS    CL1                    QUARTER HOUR NO.                          
MTDCODE2 DS    CL1                    DAY CODE                                  
MTLEN2   DS    CL1                    PROGRAM LENGTH                            
MTLNQ    EQU   *-MAINTAB                                                        
*                                                                               
MTFLAG   DS    XL1                 SAVE FLAG                                    
JUSTTRAK EQU   X'01'               ADD ONLY TRACKS                              
RELOMAIN EQU   X'02'               RELOAD MAINTENANCE SCREEN                    
JUSTSRC  EQU   X'04'               UPDATE ONLY SOURCE INV                       
SRCEFF   EQU   X'08'               CHANGE EFFECTIVE DATE ONLY                   
*                                                                               
SV1STA   DS    CL5                 FIRST STATION W/ VALID HEADER                
*                                                                               
TRACKTAB DS    9XL48               TABLE TO HOLD BACKTRACK INFO                 
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE RERMPWTWA                                                      
       ++INCLUDE DEDBEXTRAD                                                     
*                                                                               
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
*                                                                               
       ++INCLUDE RERMPWORKD                                                     
         PRINT ON                                                               
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
*  ALL FIELDS DEFINED ABOVE THE DOUBLE LINE OF ASTERIKS                         
*  MUST ALSO BE DEFINED IN THE RERMP30 PHASE.                                   
*                                                                               
INVLIST  DS    F                   POINTER TO INVENTORY INFO                    
INVDYTIM DS    CL60                EXTENDED DAY TIME DEMO TABLE                 
*                                                                               
INVMED   DS    CL1                 MEDIA                                        
INVSTAT  DS    CL5                 STATION                                      
INVMKT   DS    CL2                 MARKET                                       
INVSRC   DS    CL1                 SOURCE                                       
INVFBK   DS    CL2                 FROM BOOK                                    
INVTYP   DS    CL1                 I OR P                                       
INVEFF   DS    CL2                 EFFECTIVE DATE - COMPRESSED                  
INVNO    DS    CL1                 NUMBER IN INVENTORY LIST                     
INVBAD   DS    CL1                 0=NO ERROR, N=NUMBER OF BAD ITEM             
TOTWGHT  DS    CL2                 TOTAL NUMBER QTR HOURS                       
INVTOBK  DS    CL20                TO BOOK CODES (UP TO 5-4BYTE CODES)          
*                                                                               
INVIND   DS    CL1                 INVENTORY TYPE INDICATOR                     
INVDAYS  DS    CL1                 1=MON, 7=SUN                                 
INVTIM   DS    CL4                 MILITARY TIME                                
INVCODE  DS    CL2                 PROGRAM CODE                                 
INVCDCTL DS    B                   CONTROL BITS FOR PROGRAM CODE                
INVBTYPE DS    C                   BOOK TYPE (USER INPUT, APPLIES TO            
*                                  DEMO FILE TRANSFERS)                         
INVFRBT  DS    C                   BOOK TYPE (ON INV TO INV TRANSFER            
*                                                                               
TRBKLIST DS    CL64                BOOK ENTRIES BUILT BY REBKLST                
         SPACE                                                                  
TRBKCNT  DS    X                   COUNT OF BOOK ENTRIES                        
TRMODE   DS    C                   COMMUNICATION TO BUFFER ROUTINE              
TRWTOV   DS    C                   USER WEIGHTING OVERRIDE (Y/N)                
TRHOOKSW DS    C                   HOOK ENTERED FOR DEMAND CALL (Y/N)           
TRSVKEY  DS    CL27                                                             
TRFNOVER DS    C                   Y=SUPPRESS TIME PERIOD FOOTNOTING            
TRAPAGE  DS    A                   A(2304 BYTE PAGE)                            
TRPAGE   DS    X                   PAGES WRITTEN TO TWA                         
TRRECS   DS    X                   RECORDS GENERATED DURING LINE EDIT           
         SPACE 1                                                                
DEMEDIA  DS    CL1                 FROM MEDIA                                   
DEMSTA   DS    CL5                      STATION                                 
DEMRKT   DS    CL2                      MARKET FOR DEMOS                        
*                                                                               
HALF2    DS    H                                                                
BYTE2    DS    CL1                                                              
BYTE3    DS    CL1                                                              
BYTE4    DS    CL1                                                              
*                                                                               
TAPEOPT  DS    CL1                 Y ==> TAPE PRECISION FOR DEMO CALCS          
RIDBLK   DS    CL8                 DEMUP 4TH PARAMETER                          
*****************************************************                           
ACMMNCTE DS    A                   A(LINK TO 'TALK' TO OTHER PHASES)            
*                                                                               
TIMECHG  DS    CL1                 TIME CHANGE(S=SPRING F=FALL)                 
WGTWEEK  DS    CL1                 INCLUDE WEEKS IN WEIGHT FACTOR               
*                                                                               
INVPRG#  DS    XL3                 PROGRAM# FOR SPECIFIC LOOKUP                 
         DS    XL28                SPARE                                        
*****************************************************                           
*                                                                               
*  ERROR EQUATES                                                                
CLOSE7   EQU   707                 CLOSE OUT RECORD UP TO 7 DAYS                
ENDINVHD EQU   708                 END INV REC IN NEW END DATE/ -#              
CLOSEINV EQU   710                 MUST CLOSE OUT INVENTORY HEADER              
*NOTCLOSE EQU   712                 PREVIOUS INVENTORY REC. NOT CLOSED          
SAMEINV  EQU   713                 CAN'T HAVE SAME INV #                        
SEPLINE  EQU   720                 OTHER PGM CODE MUST BE ON OTHER LINE         
NO2PGM   EQU   721                 2ND PROGRAM IS NOT ENTERED                   
NEEDBKOP EQU   722                 REQUIRES PURE NUMBER                         
BKOROPT  EQU   727                 MUST HAVE EITHER SOURCE OR OPTIONS           
ALLDUP   EQU   731                 DUPLICATE KEYS ON ALL STATION                
NOENTRY  EQU   732                 MUST CLOSE OUT HEADER,ADD NEW HEADER         
SAMEKEY  EQU   739                 BOTH NEW HDRS CAN'T HAVE SAME EFF.           
PRREQD   EQU   740                 PR TRACK REQUIREMENTS                        
ESREQD   EQU   741                 ES TRACK REQUIREMENTS                        
PJREQD   EQU   742                 PJ TRACK REQUIREMENTS                        
HEADDNE  EQU   760                 HEADER DOESN'T EXIST FOR ANY STA             
ALLDONE  EQU   763                 ALL STATION PROCESSED, HIT ENTER             
PROSTA   EQU   771                 HIT ENTER TO PROCESS STATIONS                
ENTCONT  EQU   772                 HIT ENTER TO CONTINUE PROCESSING             
AFT2027  EQU   851                 NO EFF. CAN BE ON OR AFTER 1/1/27            
*                                                                               
OPTBLCK2 DS    CL56                PERVAL OUTPUT                                
DYPART   DS    CL6                 DAY-PART FOR FILTER                          
         DC    X'FF'                                                            
DYPTLEN  DS    CL1                 NUMBER OF DAYPART FILTERS                    
DATEADDR DS    F                   ADDRESS OF DATE OPTIONS                      
DATE1    DS    CL2                 COMPRESSED DATE                              
DATE2    DS    CL2                 COMPRESSED DATE                              
*                                                                               
*****************************************************                           
*****************************************************                           
STAHLD   DS    CL5                 STATION HOLD AREA                            
INVHLD   DS    CL4                 INVENTORY HOLD AREA                          
DTEHLD   DS    CL3                 DATE HOLD AREA                               
DTEHLD2  DS    CL2                 2 BYTE DATE HOLD AREA                        
DTEHLDE2 DS    CL2                 2 BYTE END DATE HOLD                         
TIMEHLD  DS    CL4                                                              
IBLK     DS    CL5                 INPUT BLOCK FOR KSRC RETURN                  
OBLK     DS    CL5                 INPUT BLOCK FOR KSRC RETURN                  
TEMPLEN  DS    CL2                 LENGTH OF TEMPSTR PAGE                       
*                                                                               
NUMDAYS  DS    CL1                 # OF DAYS TO SUBTRACT FROM LAST INV          
*                                                                               
NEWEFF   DS    CL12                MMMDD/YY(-N) EBCDIC                          
NEWEFFB  DS    XL3                 BIN YYMMDD START OF PERIOD                   
NEWEFFC  DS    XL2                 BIN COMPRESSED START OF PERIOD               
*                                                                               
*  PRINT ELEMENT ADDRESS STORAGE LOCATIONS                                      
DYTMPTR  DS    F                   DAY/TIME ELEMENT                             
PROGPTR  DS    F                   PROGRAM ELEMENT                              
AVPRPTR  DS    F                   AVAIL PROGRAM ELEMENT                        
OVFLSW   DS    CL1                 TOO MANY LINES TO PRINT                      
*                                                                               
APROCSCR DS    F                   SELECT FIELD ON PROC SCREEN                  
ATRBKLST DS    F                   A(IN BOOK LIST)                              
         DS    F                   (SPARE)                                      
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
PGM1     EQU   X'01'               ADDING PROGRAM 1 RECORDS                     
PGM2     EQU   X'02'               ADDING PROGRAM 2 RECORDS                     
NOBKPGM  EQU   X'04'               NO SOURCE ENTERED                            
NOHEAD   EQU   X'08'               RECORD ALREADY EXISTS - DON'T ADD            
NOTVALTK EQU   X'10'               NOT A VALID TRACK FOR PARTICULAR STA         
GOTDEM   EQU   X'20'               GOT STATION FROM DEMAND CALL                 
ADDPGM2  EQU   X'40'               ADDING PROGRAM 2 HEADER                      
YESSRCE  EQU   X'80'               BOOK/PGM WAS INPUT                           
*                                                                               
MYFLAG2  DS    XL1                 FLAGS                                        
BADTRACK EQU   X'01'               DID NOT ADD THE BAD TRACK                    
GOTCF    EQU   X'02'               GOT A 'CF' ELEMENTS                          
NONEWHED EQU   X'04'               DIDN'T ADD ANY NEW HEADERS                   
ONLYPGMN EQU   X'08'               ONLY PROGRAM # ENTERED                       
ESPJTRK  EQU   X'10'               ES OR PJ TRACK                               
PRPJES1  EQU   X'20'               FOUND PR,PJ,ES CODE FOR PGM 1                
PRPJES2  EQU   X'40'               FOUND PR,PJ,ES CODE FOR PGM 2                
GOTFIRST EQU   X'80'               ALREADY GOT FIRST STATION                    
*                                                                               
MYFLAG3  DS    XL1                 FLAGS                                        
GLBLPROT EQU   X'01'               GLOBALLY PROTECTED                           
NOTCLOSE EQU   X'02'               PREV. HEADER NOT CLOSED OUT                  
*                                                                               
TEMPWORK DS    CL256               TEMPORARY HEADER                             
WORK2    DS    CL200               EXTRA WORK AREA                              
SAVEKEY  DS    CL27                                                             
TEMPKEY  DS    CL27                                                             
ORIGKEY  DS    CL27                                                             
CHNGLEN  DS    CL1                                                              
*                                                                               
TEMPBOOK DS    XL2                 CURRENT BOOK                                 
*                                                                               
BASETAB  DS    4XL5                TABLE OF BOOKS/BASE BOOKS                    
*               XL2                PROCESSING BOOK                              
*               XL1                PROCESSING BOOK TYPE                         
*               XL2                BASE BOOK (FROM DEMAND)                      
*                                                                               
TMPNDAY  DS    XL1                 # OF DAYS                                    
SVNDAY   DS    XL1                 PREV # OF DAYS                               
*                                                                               
TEMPSTIM DS    XL2                 START TIME IN MILITARY                       
TEMPETIM DS    XL2                 END TIME IN MILITARY                         
TEMPDAY  DS    XL1                 DAY                                          
TEMPQHR  DS    XL1                 TEMP STORAGE FOR QUARTER HOUR                
TEMPMTIM DS    XL2                  "      "     "  MILITARY TIME               
SDQCNT   DS    H                   # OF ENTRIES IN SDQTAB TABLE                 
SRCEBK   DS    XL2                 SOURCE BOOK                                  
SRCEPGM  DS    XL3                 SOURCE PROGRAM NUMBER                        
TEMPSQHR DS    XL1                 TEMP START QUARTER HOUR                      
TEMPEQHR DS    XL1                 TEMP END QUARTER HOUR                        
TEMPDURA DS    XL1                 DURATION IN QTR HOURS                        
CURRDURA DS    XL1                 DURATION IN QTR HOURS                        
SVDIFF   DS    XL1                 DIFFERENCE IN DURATION HOURS                 
CURRDIFF DS    XL1                 DIFFERENCE IN DURATION HOURS                 
SVDAY    DS    XL1                 DAY                                          
CURRDAY  DS    XL1                 DAY                                          
DAYCODE  DS    XL1                 DAY                                          
*                                                                               
SVPURE   DS    XL2                 PURE NUMBER                                  
TMPPURE  DS    10XL2               PURE NUMBERS FROM CF ELEMS                   
SVSETIM  DS    XL4                 START AND END TIMES                          
*                                                                               
BKTYPE   DS    CL1                 BOOK TYPE IN EBCDIC                          
TODAYBIN DS    XL3                 TODAY'S DATE IN BINARY                       
APREVLIN DS    F                   A(PREVIOUS LINE ON SCREEN)                   
ASCANBLK DS    F                   A (IN SCANNER BLOCK)                         
*                                                                               
BSVDA    DS    F                   SAVED DISK ADDRESS                           
DYTIMTAB DS    CL41                DAY TIME TABLE                               
*                                                                               
ATRANS   DS    A                                                                
VT81030  DS    A                   ADDRESS OF TRANSFER MODULE                   
*                                                                               
VREBKLST DS    F                   A(REBKLST)                                   
RELOKEY  DS    XL27                                                             
*                                                                               
SVTIME   DS    XL4                 SAVE PROGRAM TIME                            
SVKLEN   DS    XL1                 SAVE PROGRAM LENGTH                          
*                                                                               
PGMNAME  DS    CL27                NEW PROGRAM NAME                             
PGMNAMLN DS    XL1                 L'NEW PROGRAM NAME                           
PGMNDAY  DS    XL1                 PROGRAM DAY                                  
PGMNTIME DS    XL4                 PROGRAM TIME                                 
PGMNINV  DS    CL4                 PROGRAM INVENTORY #                          
PGMNOINV DS    CL3                 OLD FORMAT INVENTORY #                       
PGMNEFFB DS    XL3                 NEW EFF START DATE (BINARY)                  
PGMNEFFC DS    XL2                 NEW EFF START DATE (COMPRESSED)              
PGMNEND  DS    XL2                 NEW END DATE (IF EXISTS)                     
PGMNDURA DS    XL1                 PROGRAM DURATIONS                            
*                                                                               
MYWORK   DS    CL8                 DEMUP 4TH PARAM                              
*                                                                               
FIRSTSW  DS    CL1                                                              
DAYINP   DS    CL1                                                              
CURRPTR  DS    F                   CURRENT POINTER ON SCREEN                    
*                                                                               
AUDITFLD DS    XL10                FOR REAUDIT                                  
AUDITDAT DS    XL2                 FOR COMPRESSED DATE COMPARE                  
AUDITWRK DS    XL64                                                             
*                                                                               
AMENUTAB DS    A                   A(IN MENU TABLE)                             
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
MENUTAB  DS    CL305               MENU STATION MEMBER TABLE                    
         EJECT                                                                  
* INVENTORY LIST ENTRY DSECT                                                    
*                                                                               
INVLD    DSECT                                                                  
INVLREC  DS    0CL10                                                            
INVLFLE  DS    CL1                 P=PAV, I=INVENTORY                           
INVLTYP  DS    CL1                 X'80'  INVENTORY NUMBER                      
*                                  X'40'  FIRST IN DAY/TIME EXP.                
*                                  X'20'  LAST IN DAY/TIME EXP.                 
*                                  X'08'  ADD EXPRESSION                        
INVLWT   DS    CL1                 WEIGHT (BINARY)                              
INVLDATA DS    0CL6                                                             
INVLSTIM DS    CL2                 START TIME                                   
INVLETIM DS    CL2                 END TIME                                     
INVLDAY  DS    CL1                 DAY                                          
         DS    CL1                 SPARE                                        
         ORG   INVLDATA                                                         
INVLNUMB DS    CL4                 NUMBER                                       
INVLDATE DS    CL3                 START DATE (Y/M/D BINARY)                    
         DS    CL1                 SPARE                                        
*                                                                               
* DSECT TO COVER STATION/DAY/QHS TABLE                                          
*                                                                               
SDQTABD  DSECT                                                                  
SDQSTA   DS    CL(L'PISTA)         STATION                                      
SDQDAY   DS    XL(L'PIDAY)         DAY                                          
SDQSQH   DS    XL(L'PISQH)         START QH                                     
SDQEQH   DS    XL(L'PIEQH)         END   QH                                     
SDQLENQ  EQU   *-SDQTABD                                                        
*                                                                               
         PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE REBKLSTD                                                       
         PRINT ON                                                               
*                                                                               
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T810FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T810FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'165RERMP04A  05/01/02'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
