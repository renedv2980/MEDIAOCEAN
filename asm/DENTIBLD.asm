*          DATA SET DENTIBLD   AT LEVEL 090 AS OF 05/01/02                      
*PHASE DENTIBLA DENTIBLD                                                        
DENTIBLD CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BLD**,RA,R4,RR=RE                                            
         USING DEMCOND,R8          R8 POINTS TO GLOBAL WORKING STORAGE          
         SPACE 1                                                                
         ST    RE,RELO                                                          
         L     RC,ARREC                                                         
         LA    RC,4(RC)            RC POINTS TO RATING SERVICE RECORD           
         SPACE 1                                                                
         L     R2,AIREC            R2 POINTS TO INTERIM RECORD - SORT           
         USING INTERD,R2           KEY, VALUES, AND DEMO VALUES                 
         SPACE 1                                                                
         B     *+4(R1)             ROUTINE HOOK                                 
         SPACE 1                                                                
         B     READ                PROCESS INPUT TAPE                           
         B     CNVWR               SORT RECORD HOOK                             
         B     MORET               E-O-F ON INPUT                               
         B     EXIT                                                             
*                                                                               
XIT      XIT1                       COMMUNAL USE EXIT FOR SUBROUTINES           
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*READ -  MAIN CONTROL OF THIS PROGRAM VIS A VIS THE CONTROLLER                  
***********************************************************************         
READ     DS    0H                                                               
         CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ20              NO                                           
         MVI   MYMODE,C'G'         SET TO 1ST-TIME-THRU                         
         MVI   INTAPESW,0          DON'T INIT NEXT TIME IN                      
         OPEN  (IN1,(INPUT))                                                    
         L     R3,ARREC                                                         
         LA    R3,4(R3)                                                         
         MVC   0(10,R3),=CL10'NO INPUT'                                         
         LA    R0,NETMAP           DESTN                                        
         ST    R0,ANETMAP                                                       
         LH    R1,=Y(MAPLN)        DEST LENGTH                                  
         SR    RE,RE               SOURCE ADDRESS                               
         SR    RF,RF               # SOURCE BYTES=0                             
         MVCL  R0,RE               CLEAR BUFFER                                 
*                                                                               
         SR    R0,R0                                                            
         L     R0,ANETMAP                                                       
         AH    R0,=Y(MAPNXT)       A(SYNMAP) = A(NETMAP) + MAPNXT               
         ST    R0,ASYNMAP                                                       
         LH    R1,=Y(MAPLN)                                                     
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,ASYNMAP                                                       
         AH    R0,=Y(MAPNXT)       A(NHTMAP) = A(SYNMAP) + MAPNXT               
         ST    R0,ANHTMAP                                                       
         LH    R1,=Y(MAPLN)                                                     
         MVCL  R0,RE                                                            
         MVC   SVADDRS(12),ANETMAP                                              
*                                                                               
         SR    R1,R1                                                            
         L     R1,ANETMAP          SET ID CHAR ON BIT MAPS: N,S OR H            
         MVI   0(R1),C'N'                                                       
         L     R1,ASYNMAP                                                       
         MVI   0(R1),C'S'                                                       
         L     R1,ANHTMAP                                                       
         MVI   0(R1),C'H'                                                       
*                                                                               
READ20   DS    0H                                                               
         BAS   RE,GETNS            GET N-REC PTRS AND FILL MAPS                 
         BAS   RE,RELS             RELEASE MAPS & J-RECDS                       
         B     ENDJOB              DONE PROCESSING                              
         EJECT                                                                  
                                                                                
***********************************************************************         
*GETNS - READ THROUGH THE N-RECS ON PAVDIR.                                     
*        DETERMINE WHETHER REGULAR NETWORK STATION OR SYN STATION.              
*        PICK UP PRG NUMBER AND SET IT IN THE APPROPRIATE MAP.                  
***********************************************************************         
*                                                                               
GETNS    NTR1                                                                   
         DS    0H                                                               
GETN05   LA    R5,PAVKEY                                                        
         USING PNKEY,R5                                                         
         MVC   COMMAND,=C'DMRSEQ '                                              
         CLI   MYMODE,C'G'         FIRST TIME?                                  
         BNE   GETN10              NO                                           
         MVI   MYMODE,C'L'         LOOP THRU THIS ROUTINE GET N'S               
         XC    PAVKEY,PAVKEY       YES,FIRST TIME IN                            
         MVC   PNCODE(3),=C'NNN'   LOOK UP 1ST N-REC                            
         MVC   COMMAND,=C'DMRDHI '                                              
*                                                                               
GETN10   L     R6,ASREC                                                         
         LA    R6,4(R6)                                                         
         GOTO1 VDATAMGR,DMCB,COMMAND,=C'PAVDIR',PAVKEY,(R6)                     
         CLI   DMCB+8,0            ANY READ ERRORS                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PAVKEY(3),0(R6)     KEY READ IN                                  
         BE    GETN20                                                           
         CLC   PAVKEY(3),=C'NNN'   HAD WE BEEN LKG FOR ALL PRGMS?               
         BNE   GETNX                NO, WE WE'RE LKG FOR WKLY NHTI              
         XC    PAVKEY,PAVKEY        YES, LK FOR WKLY NHTI PTRS                  
         MVC   PAVKEY(3),=C'NWN'   READ NHTI WKLY POINTERS                      
         MVC   COMMAND,=C'DMRDHI'                                               
         B     GETN10                                                           
                                                                                
GETN20   MVC   PAVKEY(18),0(R6)    COPY KEY                                     
         OC    PNPNUM,PNPNUM       MISSING PRGM# (MAYBE A HUT RECD)             
         BZ    GETN05              BUMP TO NEXT N-REC                           
         MVC   NTINUM,PNPNUM                                                    
*                                                                               
* NHTI:                                                                         
         MVI   STATN,C'H'                                                       
         SR    R6,R6                                                            
         L     R6,ANHTMAP                                                       
         CLI   PNSTAT+4,C'H'       NHTI-NETWORKS                                
         BNE   GETN25                                                           
         CLC   =C'ABC',PNSTAT      ABC -> NETW                                  
         BE    *+10                                                             
         CLC   =C'CBS',PNSTAT      CBS -> NETW                                  
         BE    *+10                                                             
         CLC   =C'NBC',PNSTAT      NBC -> NETW                                  
         BE    *+10                                                             
         CLC   =C'FOX',PNSTAT      FOX -> NETW                                  
         BNE   GETN30              THEN IT'S GENUINE NHT STATION                
         MVI   STATN,C'N'          GOES ON NETWORK TAB                          
         L     R6,ANETMAP                                                       
         B     GETN30                                                           
*                                                                               
* NETW:                                                                         
GETN25   MVI   STATN,C'N'          NETWORK STATIONS                             
         L     R6,ANETMAP                                                       
         CLI   PNSTAT+4,C'T'       NTI-SYN                                      
         BE    GETN30                                                           
         CLI   PNSTAT+4,C'D'       NTI-DAILY NETWORK                            
         BE    GETN30                                                           
         CLI   PNSTAT+4,C'N'       NAD-NETWORK                                  
         BE    GETN30                                                           
* SYND:                                                                         
         MVI   STATN,C'S'          SYNDICATORS                                  
         L     R6,ASYNMAP                                                       
         CLI   PNSTAT+4,C'S'       NTI - SYN                                    
         BE    GETN30                                                           
         CLI   PNSTAT+4,C'M'       NAD - SYN                                    
         BE    GETN30                                                           
* UNKNOWN *                  DON'T SET IN ANY MAP--JUST PRINT OUT KEY           
         L     R6,VPRINTER                                                      
         USING DPRINT,R6                                                        
         MVC   P(18),PAVKEY                                                     
         GOTO1 VHEXOUT,DMCB,PAVKEY,P+50,18                                      
         GOTO1 VPRINTER                                                         
         MVI   MYMODE,C'L'                                                      
         B     GETN05                                                           
         DROP  R6                                                               
*                                                                               
GETN30   DS    0H                  R6 -> BITMAP  R5 -> PAVKEY                   
         GOTO1 VNTIPRG,DMCB,=C'MARK',(R6),NTINUM,RR=RELO                        
         CLC   SVADDRS(12),ANETMAP                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     GETN05              GET NEXT N-RECD                              
*                                                                               
GETNX    MVI   MYMODE,C'B'         NEXT STEP IS TO BEGIN RELS MAP RECS          
         B     XIT                 END OF 'INPUT'                               
         EJECT                                                                  
**********************************************************************          
*RELEASE- RELEASE RECDS FOR BIT MAPS                                            
**********************************************************************          
RELS     NTR1                      GENERATE 'PPPP' MAP RECDS                    
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,ANETMAP),0                              
         DS    0H                                                               
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,ASYNMAP),0                              
         DS    0H                                                               
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,ANHTMAP),0                              
*                                                                               
         MVC   AMAP,ANETMAP        RELEASE J-RECD PTRS FOR EACH MAP             
         BAS   RE,JRECS                                                         
         MVC   AMAP,ASYNMAP                                                     
         BAS   RE,JRECS                                                         
         MVC   AMAP,ANHTMAP                                                     
         BAS   RE,JRECS                                                         
*                                                                               
RELSX    B     XIT                                                              
         EJECT                                                                  
* ********************************************************************          
* JRECS-  GENERATE J-RECD PTRS FOR EACH OF THE NTI #'S IN BITMAP                
* ********************************************************************          
JRECS    NTR1                                                                   
         XC    FREE,FREE           COUNTS # FREE PRG #'S                        
         XC    USED,USED                                                        
         SR    RE,RE                                                            
         SR    R2,R2                                                            
         L     R2,AMAP                                                          
         LA    R6,PAVKEY           SET UP KEY FIELDS                            
         USING PJKEY,R6                                                         
         XC    PAVKEY,PAVKEY                                                    
         MVI   PJCODE,PJCODEQU     JNN- PPPP                                    
         MVI   PJMEDIA,C'N'                                                     
         MVI   PJSRC,C'N'                                                       
         MVC   PJSTAT,=C'PPPP'     INTSTA(4)=PPPP                               
         MVC   PJSTAT+4(1),0(R2)   'N'=NET  'S'=SYN  'H'=NHT                    
*                                                                               
         LA    R5,1                R1=NTI NUMBER=DDS NUMBER                     
         SR    R3,R3                                                            
         MVC   WORD,1(R2)          ROTATE THRU BITS OF WORDS                    
         LA    R3,1(R2)            R3 -> WORD CURRENTLY WORKING WITH            
         ST    R3,AWORD            A(WORD IN MAP)                               
         ICM   R3,15,WORD          R3=BITS OF WORD                              
         LA    R0,32               SHIFT THRU 32 BITS IN A WORD                 
*                                                                               
JREC10   TM    WORD,X'80'          GENERATE J-REC FOR THIS NTI#?                
         BO    JREC20              YES                                          
         LA    RE,1(RE)            FREE BIT                                     
JREC12   SLL   R3,1                TRY NEXT BIT                                 
         STCM  R3,15,WORD                                                       
         LA    R5,1(R5)            BUMP NTI NUMBER                              
         C     R5,=F'65535'        MAX NTI#=65535                               
         BH    JREC40              DONE                                         
         BCT   R0,JREC10                                                        
*                                                                               
         L     R1,AWORD            GET ANOTHER WORD FROM MAP                    
         LA    R1,4(R1)                                                         
         ST    R1,AWORD                                                         
         MVC   WORD,0(R1)                                                       
         ICM   R3,15,WORD                                                       
         LA    R0,32               RESET LOOP CTR                               
         B     JREC10                                                           
*                                                                               
JREC20   STC   R0,LPCTR            SAVE LOOP COUNTER                            
         ST    RE,FREE                                                          
         L     RE,USED                                                          
         LA    RE,1(RE)                                                         
         ST    RE,USED                                                          
         STCM  R5,3,PJINTNUM       DDS PROGRAM NUMBER IN HEX                    
         LR    R1,R5                                                            
         MH    R1,=H'10'                                                        
         CVD   R1,DUB                                                           
         MVC   PJEXTNUM(5),DUB+2   PWOS                                         
         GOTO1 ABLDREC,DMCB,(C'P',PJKEY)                                        
         GOTO1 APUTTAPE            OUTPUT J-REC PTR                             
         SR    R0,R0                                                            
         IC    R0,LPCTR            RESTORE LP CNTR THRU BITS OF WORD            
         L     RE,FREE                                                          
         B     JREC12              NEXT BIT, NEXT NTI NUMBER                    
*                                                                               
JREC40   DS    0H                  PRINT NUMBER OF FREE BITS IN MAP             
         ST    RE,FREE                                                          
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         L     R2,AMAP                                                          
         CLI   0(R2),C'N'          NETWORK                                      
         BNE   *+10                                                             
         MVC   P(6),=C'NETW :'                                                  
         CLI   0(R2),C'S'          SYN                                          
         BNE   *+10                                                             
         MVC   P(6),=C'SYND :'                                                  
         CLI   0(R2),C'H'          NHT                                          
         BNE   *+10                                                             
         MVC   P(6),=C'NHTI : '                                                 
         EDIT  (4,FREE),(8,P+7)                                                 
         MVC   P+16(5),=C'AVAIL'                                                
         EDIT  (4,USED),(8,P+30)                                                
         MVC   P+40(5),=C'USED '                                                
         GOTO1 VPRINTER                                                         
*                                                                               
JRECX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
* ********************************************************************          
* CNVWR- NO IRECS -> NO SORT                                                    
* ********************************************************************          
                                                                                
CNVWR    DS    0H                                                               
         MVI   BYPSORT,0                                                        
         B     EXIT                                                             
                                                                                
* *******************************************************************           
* IOERROR - READ ERROR - SET ERROR FLAG AND CLOSE FILE                          
* *******************************************************************           
*                                                                               
IOERROR  DS    0H                                                               
         GOTO1 VLOGIO,DMCB,1,=C'***READ ERROR ON INPUT***'                      
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'82'                                                   
         B     EXIT                                                             
                                                                                
********************                                                            
* EOF ON INPUT FILE                                                             
********************                                                            
MORET    DS    0H                                                               
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
*=====================================================================          
*                     LITERAL POOL                                              
*=====================================================================          
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*                     WORKING STORAGE                                           
*=====================================================================          
         DS    0F                                                               
RELO     DS    A                                                                
ANETMAP  DS    A                                                                
ASYNMAP  DS    A                                                                
ANHTMAP  DS    A                                                                
*                                                                               
MYMODE   DC    X'00'                                                            
LPCTR    DS    X                                                                
STATN    DS    C                                                                
BYTE     DS    C                                                                
WORD     DS    F                                                                
AWORD    DS    F                                                                
AMAP     DS    A                                                                
SVADDRS  DS    3F                                                               
FREE     DS    F                                                                
USED     DS    F                                                                
NTINUM   DS    XL2                                                              
COMMAND  DS    CL8                                                              
PAVKEY   DS    XL24                                                             
                                                                                
         SPACE 3                                                                
* FILE DCB AND BUFFER AREA                                                      
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=401,                                              X        
               BLKSIZE=16842,                                          X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
         SPACE 3                                                                
*                                                                               
* ---------------------------------------------------------------------         
*BIT MAPS: EACH BIT = NTI PRGM NUMBER. IF BIT IS ON,'1', # IN USE               
*                                                                               
*NOTE!!!! - DO NOT CHANGE THE ORDER OF THESE BIT MAPS!!!                        
* ---------------------------------------------------------------------         
*                                                                               
MAPLN    EQU   8192                LENGTH OF ALL BIT MAPS                       
MAPHDR   EQU   8                   HDR TELLS WHICH BIT MAP FOLLOWS              
MAPNXT   EQU   MAPLN+MAPHDR                                                     
         DS    0F                                                               
         DC    C'*NETMAP*'                                                      
NETMAP   DS    8192X               NET STATIONS BIT MAP                         
         DC    C'*SYNMAP*'                                                      
SYNMAP   DS    8192X               SYNICATION BIT MAP                           
         DC    C'*NHTMAP*'                                                      
NHTMAP   DS    8192X               NHTI BIT MAP                                 
*                                                                               
MAPX     DS    0H                                                               
*                                                                               
         EJECT                                                                  
*        DEINTD                                                                 
       ++INCLUDE DEINTD                                                         
         SPACE 1                                                                
*        DEINTNTID                                                              
       ++INCLUDE DEINTNT3D                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
*        DEDEMFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
*        DEDEMCNVD                                                              
       ++INCLUDE DEDEMCNVD                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'090DENTIBLD  05/01/02'                                      
         END                                                                    
