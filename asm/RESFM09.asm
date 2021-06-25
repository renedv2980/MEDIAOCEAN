*          DATA SET RESFM09    AT LEVEL 035 AS OF 03/05/09                      
*PHASE T81809B,*                                                                
         TITLE 'T81809 - RESFM09 - STATION COMMISSION RECORDS'                  
*                                                                               
*********************************************************************           
*                                                                   *           
*        RESFM09 --- STATION COMMISSION RECORDS HANDLER             *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUN30/89 (MRR) --- CHANGE SCREEN, LIST AND RECORD FOR 1 RATE      *           
*                                                                   *           
* JUL19/89 (MRR) --- MAKE REPORT MATCH LIST                         *           
*                    MAKE EFFECTIVE DATE REQUIRED                   *           
*                                                                   *           
* AUG02/89 (MRR) --- CALL EFFECTIVE DATE, EFFECTIVE MONTH           *           
*                                                                   *           
* 12DEC89  (EFJ) --- PREVENT CHANGE OF HYSTERICAL RATES             *           
*                                                                   *           
* FEB25/91 (MRR) --- LIST WAS ONLY TESTING 1ST CHARACTER OF REP CODE*           
*                                                                   *           
* 14JUN91  (EFJ) --- (FLAG DAY '91) PREVENT ADD/DEL OF REC USING    *           
*                     SAME RULES AS 12DEC89 FIX (HYSTERICAL RATES)  *           
*                                                                   *           
* 21FEB92  (SKU) --- ADD MARKET AND AR INTERFACE TO REPORT          *           
*                                                                   *           
* 29JUL96  (BU ) --- PERMIT HISTORICAL CHANGES FOR ALLIED RADIO     *           
*                    PARTNERS UNTIL FURTHER NOTICE.                 *           
*                                                                   *           
* 03OCT96  (SEP) --- ALLOW LOW POWER TV STATION ENTRY               *           
*                                                                   *           
* 27JAN97  (BU ) --- PERMIT HISTORICAL CHANGES FOR PETRY            *           
*                             UNTIL FURTHER NOTICE.                 *           
*                                                                   *           
* 10/07/98 (NRK) -22- STOOPID CHANGE:ONLY PROCESS RATES .LT. 9% IF  *           
*                     USER HITS <PF7>.                              *           
*                                                                   *           
* 25NOV98  (BU ) --- PROFILE CONTROL OF RATES WARNING               *           
*                                                                   *           
* 05MAR09  (KUI) --- BYPASS DATE CHECK FOR BL                       *           
*                                                                   *           
*********************************************************************           
*                                                                               
T81809   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1809**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DELREC              CHECK FOR HISTORICAL RATES                   
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    DELREC              CHECK FOR HISTORICAL RATES                   
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LIST                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     DS    0H                                                               
         MVI   STATUS,0                                                         
         XC    STATION(14),STATION                                              
         SPACE 1                                                                
****************************************************************                
*              VALIDATE STATION                                *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,COMSTAH                                                       
         CLI   ACTNUM,ACTLIST      FIELD NOT REQUIRED FOR LIST                  
         BE    VK10                                                             
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BNE   VK20                                                             
VK10     CLI   5(R2),0                                                          
         BE    VK50                                                             
VK20     GOTO1 ANY                                                              
         MVC   AIO,AIO2                                                         
         GOTO1 VALISTA             STATION IS REQUIRED                          
         MVC   AIO,AIO1                                                         
         MVC   STATION,WORK                                                     
         SPACE 1                                                                
****************************************************************                
*              VALIDATE OFFICE                                 *                
****************************************************************                
         SPACE 1                                                                
VK50     LA    R2,COMOFFH          FIELD IS OPTIONAL                            
         CLI   5(R2),0                                                          
         BE    VK100                                                            
VK70     MVC   AIO,AIO2                                                         
         GOTO1 VALIOFF                                                          
         MVC   AIO,AIO1                                                         
         MVC   OFFICE,WORK                                                      
         SPACE 1                                                                
****************************************************************                
*              VALIDATE ADVERTISER                             *                
****************************************************************                
         SPACE 1                                                                
VK100    LA    R2,COMADVH          FIELD IS OPTIONAL                            
         CLI   5(R2),0                                                          
         BNE   VK110                                                            
         MVC   WORK,SPACES         PREPARE TO CLEAR ADV NAME                    
         B     VK140                                                            
         SPACE 1                                                                
VK110    CLI   ACTNUM,ACTLIST                                                   
         BE    VK120                                                            
         CLI   ACTNUM,ACTREP                                                    
         BE    VK120                                                            
         SPACE 1                                                                
         OC    OFFICE,OFFICE       BUT FOR ADD, FIELDS ABOVE ARE                
         BNZ   VK120               REQUIRED IF THIS ONE IS USED                 
         MVC   CONHEAD+10(L'MISSOFF),MISSOFF                                    
         LA    R2,COMOFFH                                                       
         B     MYEND                                                            
         SPACE 1                                                                
VK120    MVC   AIO,AIO2                                                         
         GOTO1 VALIADV                                                          
         MVC   AIO,AIO1                                                         
         MVC   ADV,WORK                                                         
VK140    MVC   COMADVN,WORK+10                                                  
         OI    COMADVNH+6,X'80'    TRANSMIT FIELD                               
         SPACE 1                                                                
****************************************************************                
*              VALIDATE TYPE                                                    
****************************************************************                
         SPACE 1                                                                
VK150    LA    R2,COMTYPEH         FIELD IS OPTIONAL                            
         MVI   ERROR,INVALID                                                    
VK160    CLI   5(R2),0                                                          
         BE    VK200                                                            
         SPACE 1                                                                
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK170                                                            
         CLI   ACTNUM,ACTREP                                                    
         BE    VK170                                                            
         SPACE 1                                                                
         OC    OFFICE,OFFICE       BUT FOR ADD, FIELDS ABOVE ARE                
         BNZ   VK165               REQUIRED IF THIS ONE IS USED                 
         MVC   CONHEAD+10(L'MISSOFF),MISSOFF                                    
         LA    R2,COMOFFH                                                       
         B     MYEND                                                            
         SPACE 1                                                                
VK165    OC    ADV,ADV                                                          
         BNZ   VK170                                                            
         MVC   CONHEAD+10(L'MISSADV),MISSADV                                    
         LA    R2,COMADVH                                                       
         B     MYEND                                                            
         SPACE 1                                                                
VK170    CLI   5(R2),1                                                          
         BH    ERREND                                                           
         MVC   TYPE,8(R2)                                                       
         SPACE 1                                                                
****************************************************************                
*              VALIDATE DATE                                                    
****************************************************************                
         SPACE 1                                                                
VK200    DS    0H                                                               
         LA    R2,COMDATEH         DATE IS REQUIRED                             
         CLI   5(R2),0                                                          
         BNE   VK210                                                            
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKXIT                                                            
         CLI   ACTNUM,ACTREP                                                    
         BE    VKXIT                                                            
         MVC   CONHEAD+10(L'MISSDAT),MISSDAT                                    
         B     MYEND                                                            
         SPACE 1                                                                
VK210    EQU   *                                                                
         MVI   ERROR,INVALID                                                    
VK220    GOTO1 DATVAL,DMCB,(2,8(R2)),DUB   VALIDATE M/Y                         
         OC    DMCB(4),DMCB        ERROR                                        
         BZ    ERREND                                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK   SO SHOULD NOT BE VALID              
         OC    DMCB(4),DMCB                 M/D/Y                               
         BNZ   ERREND                                                           
         GOTO1 DATCON,DMCB,DUB,(3,WORK)     YMD BINARY                          
         MVC   DATE,WORK                    DISCARD DAY                         
         SPACE 1                                                                
VKXIT    MVC   AIO,AIO1            BUILD COMM KEY IN AIO1                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCOMKEY,R4                                                       
         MVI   RCOMKTYP,X'29'                                                   
         MVC   RCOMKREP,AGENCY                                                  
         MVC   RCOMKSTA,STATION                                                 
         MVC   RCOMKOFF,OFFICE                                                  
         MVC   RCOMKADV,ADV                                                     
         MVC   RCOMKTPE,TYPE                                                    
         MVC   RCOMKDAT,DATE                                                    
VKXX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
VREC     DS    0H                                                               
         OI    CONSERVH+1,X'01'    SVC RQST FLD ALWAYS MODIFIED                 
         OI    CONSERVH+6,X'80'    AND TRANSMIT IT                              
*                                                                               
         TM    COMR1H+4,X'20'      PRE-VALID?                                   
         BNZ   VR20                                                             
         CLC   CONACT(3),=C'ADD'   ACTN ADD?                                    
         BE    VR20                CHECKED ELSEWHERE                            
         CLI   DDS,C'Y'            IS THIS A DDS TERM?                          
         BE    VR20                YES - ALL CHANGES VALID                      
         BAS   RE,VCHG                                                          
         BE    VR20                                                             
*                                                                               
*   ALLIED RADIO PARTNERS TEST - REMOVE WHEN NO LONGER NEEDED                   
*                                                                               
         CLC   =C'AQ',AGENCY       IS THIS ALLIED RADIO?                        
         BE    VR20                YES - PERMIT CHANGE                          
*                                                                               
*   PETRY TEST - REMOVE WHEN NO LONGER NEEDED                                   
*                                                                               
         CLC   =C'PV',AGENCY       IS THIS PETRY?                               
         BE    VR20                YES - PERMIT CHANGE                          
*                                                                               
*   BLAIR TEST - REMOVE WHEN NO LONGER NEEDED                                   
*                                                                               
         CLC   =C'BL',AGENCY       IS THIS BLAIR?                               
         BE    VR20                YES - PERMIT CHANGE                          
*                                                                               
         MVC   CONHEAD+10(L'HISTERR),HISTERR                                    
         LA    R2,COMR1H                                                        
         B     MYEND                                                            
*                                                                               
VR20     DS    0H                                                               
         MVI   ELCODE,1                                                         
         GOTO1 REMELEM             DELETE OLD X'01' ELEMENT                     
         SPACE 1                                                                
         XC    ELEM,ELEM           AND REBUILD                                  
         LA    R6,ELEM                                                          
         USING RCOMELEM,R6                                                      
         MVC   RCOMCODE(2),=X'011E'                                             
         SPACE 1                                                                
****************************************************************                
*              VALIDATE RATE                                   *                
****************************************************************                
         SPACE 1                                                                
*                                                                               
* RATE FOR THE 'ALL' LEVEL SHOULD BE AT LEAST 9%.  IF NOT, THEN MAKE            
* SURE THEY REALLY WANT TO ENTER IT.                                            
*                                                                               
         CLI   COMOFFH+5,0         ANY DATA IN OFFICE FIELD?                    
         BH    VREC0100            YES - SO PROCESS RATE                        
         CLI   COMADVH+5,0         ANY DATA IN ADV FIELD?                       
         BH    VREC0100            YES - SO PROCESS RATE                        
         CLI   COMTYPEH+5,0        ANY DATA IN TYPE FIELD?                      
         BH    VREC0100            YES - SO PROCESS RATE                        
*                                                                               
         CLC   COMR1(2),=C'09'     ELSE - AT LEAST 09% ENTERED?                 
         BNL   VREC0100            YES - SO CONTINUE                            
*                                                                               
         LR    R2,RA               SET A(TWA)                                   
*                                                                               
         A     R2,=F'3508'                                                      
         USING SVDSECT,R2                                                       
         TM    SVPGPBIT,X'04'      6TH BIT: WARNING MESSAGE?                    
         BNO   VREC0100            NO  - BYPASS IT                              
*                                                                               
         CLI   PFKEY,7             ELSE - WAS PF7 ENTERED?                      
         BE    VREC0100            YES - SO PROCESS THE RATE                    
*                                                                               
         MVC   CONHEAD(L'BELOW9),BELOW9                                         
         LA    R2,COMR1H                                                        
         B     MYEND010                                                         
*                                                                               
VREC0100 EQU   *                                                                
*                                                                               
         LA    R2,COMR1H                                                        
         BAS   RE,PCT                                                           
         ST    R0,RCOMRAT1                                                      
*                                                                               
         GOTO1 ADDELEM                                                          
         OI    COMR1H+4,X'20'      SET PRE-VALID                                
         DROP  R6                                                               
         SPACE 1                                                                
VRXIT    B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              PRE-DELETE/ADD RECORD ROUTINE                   *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DELREC   DS    0H                                                               
         CLI   DDS,C'Y'            IS THIS A DDS TERM?                          
         BE    XIT                 YES - ALL CHANGES VALID                      
         CLC   =C'PV',AGENCY       IS THIS PETRY?                               
         BE    VR20                YES - PERMIT CHANGE                          
         CLC   =C'BL',AGENCY       IS THIS BLAIR?                               
         BE    VR20                YES - PERMIT CHANGE                          
*                                                                               
         BAS   RE,VCHG                                                          
         BE    XIT                                                              
*                                                                               
         MVC   CONHEAD+10(L'HISTDAT),HISTDAT                                    
         LA    R2,CONACTH                                                       
         B     MYEND                                                            
*                                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DKEY     DS    0H                                                               
         MVC   SVKEY,KEY           SAVE KEY AND                                 
         MVC   SVDMWORK,DMWORK+4   SAVE D/A (MAY NEED IF LATER GETREC)          
         L     R4,AIO              RECORD SELECTED                              
         USING RCOMKEY,R4                                                       
         SPACE 1                                                                
         LA    R2,COMSTAH          STATION                                      
         MVC   8(4,R2),RCOMKSTA                                                 
         CLI   RCOMKSTA+4,C'L'     LOW POWER TV                                 
         BE    DK11                                                             
         CLI   RCOMKSTA+4,C'A'                                                  
         BE    DK10                                                             
         CLI   RCOMKSTA+4,C'F'                                                  
         BE    DK10                                                             
         CLI   RCOMKSTA+4,C'C'                                                  
         BNE   DK20                                                             
DK10     MVC   12(3,R2),=C'-AM'                                                 
         MVC   13(1,R2),RCOMKSTA+4                                              
         B     DK20                                                             
DK11     MVC   12(2,R2),=C'-L'                                                  
DK20     OI    COMSTAH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
         LA    R2,COMOFFH          OFFICE                                       
         MVC   8(2,R2),RCOMKOFF                                                 
         OI    COMOFFH+6,X'80'                                                  
         SPACE 1                                                                
         LA    R2,COMADVH          ADVERTISER                                   
         MVC   8(4,R2),RCOMKADV                                                 
         OI    COMADVH+6,X'80'                                                  
         XC    WORK,WORK                                                        
         OC    RCOMKADV,RCOMKADV   DO WE NEED ADVERTISER NAME                   
         BZ    DK30                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 VALIADV             GET IT IN IO AREA 2                          
         OI    STATUS,X'80'        INDICATE GETREC DONE                         
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
DK30     LA    R2,COMADVNH         ADVERTISER NAME                              
         MVC   8(20,R2),WORK+10                                                 
         OI    COMADVNH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,COMTYPEH         TYPE                                         
         MVC   8(1,R2),RCOMKTPE                                                 
         OI    COMTYPEH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,COMDATEH         DATE                                         
         XC    COMDATE,COMDATE                                                  
         OC    RCOMKDAT,RCOMKDAT                                                
         BZ    DK40                                                             
         XC    WORK(3),WORK                                                     
         MVC   WORK(2),RCOMKDAT                                                 
         GOTO1 DATCON,DMCB,(3,WORK),(9,8(R2))                                   
DK40     OI    COMDATEH+6,X'80'    TRANSMIT FIELD                               
         DROP  R4                                                               
DKXIT    DS    0H      IF NOT ADD, MAY NEED TO DO ANOTHER                       
         CLI   ACTNUM,ACTADD       GETREC BEFORE THE PUTREC                     
         BE    DKXX                                                             
         TM    STATUS,X'80'        HAVE WE DONE ANOTHER GETREC                  
         BZ    DKXX                                                             
         NI    STATUS,X'7F'        YES AND TURN OFF INDICATOR                   
         MVC   AIO,AIO2            PUT IT IN AIO2                               
         MVC   KEY(27),SVKEY                                                    
         MVC   KEY+28(4),SVDMWORK                                               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            NEW RECORD IS IN AIO1                        
DKXX     B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DREC     DS    0H                                                               
         L     R4,AIO                                                           
         USING RCOMREC,R4                                                       
         LA    R6,RCOMELEM                                                      
         USING RCOMELEM,R6                                                      
         SPACE 1                                                                
         LA    R2,COMR1H                                                        
         EDIT  (4,RCOMRAT1),(6,COMR1),FILL=0,ZERO=NOBLANK                       
         OI    COMR1H+6,X'80'      TRANSMIT FIELD                               
         OI    COMR1H+4,X'20'      SET PRE-VALID                                
         SPACE 1                                                                
DRXIT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*                        LIST AND PRINT ROUTINE                *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
LIST     DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR5                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
LR5      LA    R4,KEY                                                           
         USING RCOMKEY,R4                                                       
         OC    KEY(27),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVI   RCOMKTYP,X'29'                                                   
         MVC   RCOMKREP,AGENCY                                                  
         OC    STATION,STATION                                                  
         BZ    LR7                                                              
         MVC   RCOMKSTA,STATION                                                 
         OC    OFFICE,OFFICE                                                    
         BZ    LR7                                                              
         MVC   RCOMKOFF,OFFICE                                                  
         OC    ADV,ADV                                                          
         BZ    LR7                                                              
         MVC   RCOMKADV,ADV                                                     
         CLI   TYPE,0                                                           
         BE    LR7                                                              
         MVC   RCOMKTPE,TYPE                                                    
         OC    DATE,DATE                                                        
         BZ    LR7                                                              
         MVC   RCOMKDAT,DATE                                                    
LR7      MVC   SAVEKEY,KEY                                                      
LR10     GOTO1 HIGH                                                             
LR15     CLC   KEY(13),SAVEKEY     CORRECT REP                                  
         BNE   LRXIT                                                            
         OC    STATION,STATION                                                  
         BZ    LR17                                                             
         CLC   KEY+13(5),STATION                                                
         BNE   LRXIT                                                            
LR17     OC    OFFICE,OFFICE                                                    
         BZ    LR19                                                             
         CLC   KEY+18(2),OFFICE                                                 
         BNE   LR200                                                            
LR19     OC    ADV,ADV                                                          
         BZ    LR21                                                             
         CLC   KEY+20(4),ADV                                                    
         BNE   LR200                                                            
LR21     CLI   TYPE,0                                                           
         BE    LR23                                                             
         CLC   KEY+24(1),TYPE                                                   
         BNE   LR200                                                            
LR23     OC    DATE,DATE                                                        
         BZ    LR30                                                             
         CLC   KEY+25(2),DATE                                                   
         BNE   LR200                                                            
LR30     MVC   LISTAR,SPACES       CLEAR OUT LIST LINE                          
         LA    R2,LISTAR                                                        
         USING LISTD,R2                                                         
LR50     MVC   LSTA(4),RCOMKSTA                                                 
         CLI   RCOMKSTA+4,C'L'                                                  
         BE    LR56                                                             
         CLI   RCOMKSTA+4,C'A'                                                  
         BE    LR55                                                             
         CLI   RCOMKSTA+4,C'F'                                                  
         BE    LR55                                                             
         CLI   RCOMKSTA+4,C'C'                                                  
         BNE   LR57                                                             
LR55     MVC   LSTA+4(3),=C'-AM'                                                
         MVC   LSTA+5(1),RCOMKSTA+4                                             
         B     LR57                                                             
LR56     MVC   LSTA+4(2),=C'-L'                                                 
LR57     MVC   LOFF,RCOMKOFF                                                    
         MVC   LADV,RCOMKADV                                                    
         MVC   LTYPE,RCOMKTPE                                                   
         OC    RCOMKDAT,RCOMKDAT                                                
         BZ    LR60                                                             
         XC    WORK(3),WORK                                                     
         MVC   WORK(2),RCOMKDAT                                                 
         GOTO1 DATCON,DMCB,(3,WORK),(9,LDATE)                                   
         SPACE 1                                                                
LR60     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         EDIT  (4,RCOMRAT1),(6,LRAT1),FILL=0,ZERO=NOBLANK                       
         DROP  R4                                                               
         SPACE 1                                                                
LR100    CLI   MODE,PRINTREP                                                    
         BNE   LR150                                                            
*                                                                               
         MVC   SVKEY,KEY           SAVE TO PRESERVE SEQ ORDER                   
*                                                                               
         LA    R4,P                                                             
         USING PRINTD,R4                                                        
         MVC   PSTA,LSTA                                                        
         MVC   POFF,LOFF                                                        
         MVC   PADV,LADV                                                        
         MVC   PTYPE,LTYPE                                                      
         MVC   PDATE,LDATE                                                      
         MVC   PRAT1,LRAT1                                                      
         DROP  R4                                                               
*                                                                               
         L     R4,AIO                                                           
         USING RCOMREC,R4                                                       
         XC    KEY,KEY             GET STATION RECORD                           
         LA    R5,KEY                                                           
         USING RSTAREC,R5                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RCOMKREP                                                
         MVC   RSTAKSTA,RCOMKSTA                                                
         DROP  R4,R5                                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   LR120                                                            
*                                                                               
         LA    R4,P                                                             
         USING PRINTD,R4                                                        
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSTAELEM,R6                                                      
         MVI   ELCODE,1            GET MARKET NAME                              
         BAS   RE,GETEL                                                         
         BNE   LR110                                                            
         MVC   PMARKET,RSTAMKT                                                  
         DROP  R6                                                               
*                                                                               
LR110    L     R6,AIO                                                           
         USING RSTAXXEL,R6                                                      
         MVI   ELCODE,8            GET A/R INTERFACE CODE                       
         BAS   RE,GETEL                                                         
         BNE   LR120                                                            
         MVC   PFACE,RSTAOSI                                                    
         DROP  R4,R6                                                            
*                                                                               
LR120    GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                REESTABLISH SEQ ORDER                        
         B     LR200                                                            
         SPACE 1                                                                
LR150    GOTO1 LISTMON             FOR LIST                                     
         SPACE 1                                                                
LR200    GOTO1 SEQ                 NEXT RECORD                                  
         LA    R4,KEY                                                           
         B     LR15                                                             
         SPACE 1                                                                
LRXIT    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
PCT      NTR1                                                                   
         MVI   ERROR,MISSING                                                    
         SR    R0,R0                                                            
         CLI   5(R2),0                                                          
         BE    ERREND              MUST INPUT SOMETHING                         
         MVI   ERROR,NOTNUM                                                     
         TM    4(R2),X'08'         VALID NUMERIC                                
         BZ    ERREND                                                           
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),6                                                          
         BH    ERREND                                                           
         SPACE 1                                                                
         LA    R1,6                                                             
         ZIC   RE,5(R2)                                                         
         SR    R1,RE                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   R0,DUB                                                           
         SPACE 1                                                                
         LTR   R1,R1                                                            
         BZ    PCT20                                                            
PCT10    MH    R0,=H'10'                                                        
         BCT   R1,PCT10                                                         
PCT20    XIT1  REGS=(R0)                                                        
         EJECT                                                                  
****************************************************************                
* VALCHG: VALIDATE ACTN CHANGE - CC=0 VALID, CC<>0 INVALID     *                
* BUILD EOM RECORD FROM INFO ON THIS RECORD.  IF THE END DATE  *                
* ON THE EOM RECORD IS NOT GREATER THAN OR EQUAL TO TODAYS     *                
* DATE, THEN THEY CANNOT CHANGE THE RATE UNLESS THEY ARE A DDS *                
* TERMINAL (TESTED BY CALLER).                                 *                
****************************************************************                
VCHG     NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK)                                    
         L     R6,AIO                                                           
         USING RCOMREC,R6                                                       
         LA    R5,KEY                                                           
         USING REOMREC,R5                                                       
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   REOMKTYP,X'18'                                                   
         MVC   REOMKREP,RCOMKREP                                                
         MVC   REOMKYR,WORK                                                     
         MVC   AIO,AIO3                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         GOTO1 DATCON,DMCB,(5,WORK),(2,WORK)                                    
         L     R5,AIO                                                           
         LA    R2,REOMDATE+2       SKIP FIRST                                   
         LA    R3,CALTBL                                                        
         LA    R3,L'CALTBL(R3)     SKIP FIRST                                   
* START LOOP THRU DATES                                                         
VC10     CLC   0(2,R2),WORK                                                     
         BNL   VC20                                                             
         LA    R2,2(R2)                                                         
         LA    R3,L'CALTBL(R3)                                                  
         B     VC10                                                             
VC20     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK)                                    
         MVC   WORK+1,0(R3)        PUT BINARY MONTH INTO DATE                   
         CLC   RCOMKDAT,WORK                                                    
         BNL   VALEX                                                            
         MVC   KEY(27),SAVEKEY                                                  
         ST    R6,AIO                                                           
         GOTO1 HIGH                READ BACK LAST REC...                        
         GOTO1 GETREC                                                           
         LA    R3,1                SET CC NEQ 0 FOR EXIT                        
         LTR   R3,R3                                                            
         B     XIT                                                              
VALEX    DS    0H                                                               
         MVC   KEY(27),SAVEKEY                                                  
         ST    R6,AIO                                                           
         CLI   MODE,RECADD                                                      
         BE    VALEX2                                                           
         GOTO1 HIGH                READ BACK LAST REC...                        
         GOTO1 GETREC                                                           
VALEX2   SR    R3,R3               SET CC EQ 0 FOR EXIT                         
         B     XIT                                                              
         DROP  R5,R6                                                            
*                                                                               
CALTBL   DS    0XL1                                                             
         DC    X'00'               NOT USED                                     
         DC    X'01'               JAN                                          
         DC    X'02'               FEB                                          
         DC    X'03'               MAR                                          
         DC    X'04'               APR                                          
         DC    X'05'               MAY                                          
         DC    X'06'               JUN                                          
         DC    X'07'               JUL                                          
         DC    X'08'               AUG                                          
         DC    X'09'               SEP                                          
         DC    X'0A'               OCT                                          
         DC    X'0B'               NOV                                          
         DC    X'0C'               DEC                                          
         EJECT                                                                  
****************************************************************                
*  HEDSPECS                                                    *                
****************************************************************                
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H1,50,C'COMMISSION RATE'                                         
         SSPEC H2,50,C'---------------'                                         
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         SSPEC H6,1,C'STATION'                                                  
         SSPEC H6,10,C'MARKET'                                                  
         SSPEC H6,32,C'A/R INTERFACE'                                           
         SSPEC H6,47,C'OFFICE'                                                  
         SSPEC H6,55,C'ADVERTISER'                                              
         SSPEC H6,67,C'TYPE'                                                    
         SSPEC H6,75,C'EFF MONTH'                                               
         SSPEC H6,86,C'RATE'                                                    
         SSPEC H7,1,95C'-'                                                      
         DC    X'00'                                                            
         SPACE 4                                                                
HOOK     NTR1                                                                   
         LA    R3,H3                                                            
         OC    STATION(14),STATION                                              
         BZ    HK60                                                             
         OC    STATION,STATION                                                  
         BZ    HK10                                                             
         MVC   0(16,R3),=C'STATION FILTER -'                                    
         MVC   17(5,R3),STATION                                                 
         LA    R3,25(R3)                                                        
HK10     OC    OFFICE,OFFICE                                                    
         BZ    HK20                                                             
         MVC   0(15,R3),=C'OFFICE FILTER -'                                     
         MVC   16(2,R3),OFFICE                                                  
         LA    R3,21(R3)                                                        
HK20     OC    ADV,ADV                                                          
         BZ    HK30                                                             
         MVC   0(19,R3),=C'ADVERTISER FILTER -'                                 
         MVC   20(4,R3),ADV                                                     
         LA    R3,27(R3)                                                        
HK30     CLI   TYPE,0                                                           
         BE    HK40                                                             
         MVC   0(13,R3),=C'TYPE FILTER -'                                       
         MVC   14(1,R3),TYPE                                                    
         LA    R3,18(R3)                                                        
HK40     OC    DATE,DATE                                                        
         BZ    HK60                                                             
         MVC   0(13,R3),=C'DATE FILTER -'                                       
         XC    WORK(3),WORK                                                     
         MVC   WORK(2),DATE                                                     
         GOTO1 DATCON,DMCB,(3,WORK),(9,14(R3))                                  
         SPACE 1                                                                
HK60     B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
MYEND    MVC   CONHEAD(9),=C'* ERROR *'                                         
MYEND010 EQU   *                                                                
         MVI   ERROR,X'FE'         USING MY OWN ERROR MESSAGE                   
         MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* MY OWN ERROR MESSAGES                                                         
         SPACE 2                                                                
MISSOFF  DC    C'OFFICE REQUIRED'                                               
MISSADV  DC    C'ADVERTISER REQUIRED'                                           
MISSDAT  DC    C'EFFECTIVE DATE REQUIRED'                                       
HISTERR  DC    C'CAN''T CHANGE HISTORICAL RATE'                                 
HISTDAT  DC    C'CAN''T ADD/DELETE HISTORICAL RECORDS'                          
BELOW9   DC    C'WARNING: RATE BELOW 9%.  HIT <PF7> TO PROCESS.'                
         EJECT                                                                  
*        DSECT TO COVER THE PROFILE ENTRY                                       
*                                                                               
SVDSECT  DSECT                                                                  
*                                                                               
SVPGENTY DS    0CL12                                                            
SVPGREP  DS    CL2                 REP/AGENCY/USER POWER CODE                   
SVPGP#   DS    CL1                 SFM PROGRAM NUMBER (18)                      
         DS    CL1                                                              
SVPGPBIT DS    CL8                 64 PROFILES                                  
*                                                                               
         EJECT                                                                  
LISTD    DSECT                                                                  
LSTA     DS    CL7                                                              
         DS    CL4                                                              
LOFF     DS    CL2                                                              
         DS    CL7                                                              
LADV     DS    CL4                                                              
         DS    CL6                                                              
LTYPE    DS    CL1                                                              
         DS    CL6                                                              
LDATE    DS    CL6                                                              
         DS    CL4                                                              
LRAT1    DS    CL6                                                              
*                                                                               
PRINTD   DSECT                                                                  
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PMARKET  DS    CL20                                                             
         DS    CL2                                                              
PFACE    DS    CL10                                                             
         DS    CL5                                                              
POFF     DS    CL2                                                              
         DS    CL6                                                              
PADV     DS    CL4                                                              
         DS    CL8                                                              
PTYPE    DS    CL1                                                              
         DS    CL7                                                              
PDATE    DS    CL6                                                              
         DS    CL5                                                              
PRAT1    DS    CL6                                                              
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMF9D                                                                      
* RESFME9D                                                                      
* REGENCOM                                                                      
* REGENEOM                                                                      
* RESFMWORKD                                                                    
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMF9D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFME9D                                                       
         EJECT                                                                  
       ++INCLUDE REGENCOM                                                       
         EJECT                                                                  
       ++INCLUDE REGENEOM                                                       
         EJECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
STATION  DS    CL5                                                              
OFFICE   DS    CL2                                                              
ADV      DS    CL4                 ADVERTISER                                   
TYPE     DS    CL1                                                              
DATE     DS    CL2                                                              
SAVEKEY  DS    CL27                                                             
SVKEY    DS    CL27                FOR INTERVENING GETRECS                      
SVDMWORK DS    F                                                                
STATUS   DS    XL1                 X'80' - DONE GETREC FOR VALIDATION           
MARKET   DS    CL20                MARKET NAME FOR REPORT                       
FACECODE DS    CL10                A/R INTERFACE CODE FOR REPORT                
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035RESFM09   03/05/09'                                      
         END                                                                    
