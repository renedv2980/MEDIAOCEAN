*          DATA SET DEDEM05    AT LEVEL 092 AS OF 05/20/15                      
*PHASE T21B05B                                                                  
         TITLE 'DEDEM05 - $DEM LIST DEMOS && MODIFIER AVAILABILITY'             
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* JUN18/02 080 BPOO - RELINK WITH NEW DEMO TABLE FOR BBM              *         
* FEB07/02 079 BPOO - FIX CANADIEN DEMO TABLE LOOKUP AND FIX          *         
*                     IT TO LOOK UP USTV TABLE FOR NOW                *         
* JAN11/01 076 BPOO - SUPPORT RADAR FILE                              *         
* NOV02/00 075 BPOO - MOVE DEM TABLES TO PHASE DEM81                  *         
* SEP22/00 074 BPOO - falink stuff...change screen one line lower     *         
* Mar08/00 073 GLEE - Relinked for bigger buy record I/O area         *         
*                                                                     *         
* Dec14/99 072 GLEE - New demo table for NHTI                         *         
*                                                                     *         
* Sep13/99 068 GLEE - Set CC upon exiting DEMPROC mode                *         
*                                                                     *         
* Nov16/95 007 GLEE - Prepatory stage for STEREO support              *         
*                                                                     *         
***********************************************************************         
                                                                                
                                                                                
************************UPDATE LOG********************************              
* 28MAR94  RELEASE R6 DUTY AS WRKING REG IN DEMPROC ROUTINE      *              
*          REMOVED SELF-MODIFYING CODE NEAR DEMBLDA              *              
******************************************************************              
                                                                                
***********************************************************************         
*                                                                               
* THIS OVERLAY PROGRAM IS TO CREATE A DEMOGRAPHIC LIST FOR THIS                 
*        AGENCY, MEDIA, AND BOOK                                                
* LISTING DEMOGRAPHIC NAME, NUMBER, AND RATING SERVICE PROVIDED                 
* MODIFIERS, AND CALCULATED MODIFIERS.                                          
*                                                                               
***********************************************************************         
         SPACE 1                                                                
DEM05    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DEM5**,RA,RR=RE                                              
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
         ST    RE,RELO05                                                        
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   APMODE,FORMHEAD     FORMAT HEADLINES & INITIALISE                
         BE    DEMHEAD                                                          
         CLI   APMODE,PROCESS      READ & POST RECORDS TO BUFFER                
         BE    DEMPROC                                                          
         CLI   APMODE,FORMLINE     FORMAT A PRINT A BUFFER RECORD               
         BE    DEMLINE                                                          
*                                                                               
EXITH    DS    0H                  EXIT W/ CC HIGH                              
         LA    R0,1                                                             
         J     EXITCR                                                           
                                                                                
EXITL    DS    0H                  EXIT W/ CC LOW                               
         LHI   R0,-1                                                            
         J     EXITCR                                                           
                                                                                
EXITE    DS    0H                  EXIT W/ CC EQUAL                             
         SR    R0,R0                                                            
         J     EXITCR                                                           
                                                                                
EXITCR   DS    0H                                                               
         AR    R0,R9                                                            
         CR    R0,R9               SET CC                                       
         J     EXIT                 AND EXIT                                    
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* FORMAT HEADLINES & INITIALIZE FOR DEMO LOOK-UPS                               
*                                                                               
DEMHEAD  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    DEMHD10                                                          
*                                                                               
         MVC   DEMHD3(60),HEADING                                               
         SPACE 1                                                                
* SET BINSRCH PARMS                                                             
         SPACE 1                                                                
         LA    R0,BINDNAM-BINRECD  DISPLACEMENT TO KEY                          
         LA    RE,L'BINDNAM+L'BINDEMO  L'KEY                                    
         LA    RF,BINEND-BINRECD   L'RECORD                                     
         CLI   OPTLIST,C'N'        TEST LIST IN NUMERIC SEQUENCE                
         BNE   DEMHEAD2                                                         
         LA    R0,BINDEMO-BINRECD  DISPL TO DEMO NUM                            
         LA    RE,L'BINDEMO        LENGTH OF DEMO NUM                           
DEMHEAD2 ST    RE,BINLKEY                                                       
         STC   R0,BINDKEY                                                       
         ST    RF,BINLREC                                                       
         SPACE 1                                                                
* GET ALL MODIFIER & MASTER DISPL TABLE ADDRESSES                               
         SPACE 1                                                                
         L     R2,AFAC             COMFACS ADDRESS                              
         USING COMFACSD,R2                                                      
         MVC   TABLST,TABLIST      GET MODIFIER TABLE ADDRESS                   
         GOTO1 CDEMADDR,DMCB,(X'FF',TABLST),AFAC                                
*                                                                               
         XC    DUB,DUB             SPECIAL CALL FOR SYSFACS                     
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         GOTO1 CSWITCH,DUB                                                      
         L     RF,0(R1)            RF=V(SYSFACS)                                
         L     RF,YVSSB-YSYSFACD(RF) RF=V(SSB)                                  
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
         OC    ALET,ALET                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         SPACE 1                                                                
********************************************************************            
* CONVERT DBFIL/DBMED INTO INTFIL & INTMED USING TFLCNVRT TABLE                 
********************************************************************            
         MVC   DUB(3),DBFIL                                                     
         MVC   DUB+3(1),DBMED                                                   
         CLI   DBMED,C'O'          OVERNIGHTS SAME AS USTV                      
         BNE   *+8                                                              
         MVI   DUB+3,C'T'                                                       
         CLI   DBMED,C'U'          COUNTY COVERAGE                              
         BNE   *+10                                                             
         MVC   DUB(3),=C'CUN'                                                   
         ICM   R1,7,TFLCNVT+1      GET ADDR FROM DEMADDR                        
         BNZ   *+6                                                              
         DC    H'0'                IF NO TABLE ADDRESS, QUIT                    
         LAM   AR1,AR1,ALET                                                     
         SAC   512                                                              
*                                                                               
DEMHEAD4 CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DUB(4),0(R1)                                                     
         BE    *+12                                                             
         LA    R1,6(,R1)                                                        
         B     DEMHEAD4                                                         
         MVC   INTFM,4(R1)         SAVE INTERNAL FILE 2 BYTE                    
         SAC   0                   SWITH OUT OF ACCESS REG MODE                 
         LAM   AR1,AR1,=F'0'       & CLEAR AR1                                  
         EJECT                                                                  
********************************************************************            
* FIND CORRECT MODIFIER TABLE                                                   
********************************************************************            
         MVC   DUB(2),INTFM        BUILD                                        
         MVC   DUB+2(2),AGYALPH         TABLE                                   
         MVI   DUB+4,X'FF'                   COMPARAND                          
DEMHEAD5 SR    R1,R1                                                            
         ICM   R1,7,TMODIF+1                                                    
         BNZ   *+6                                                              
         DC    H'0'                IF NO TABLE ADDRESS, QUIT                    
         LAM   AR1,AR1,ALET                                                     
         SAC   512                                                              
         USING MODHDRD,R1                                                       
         MVI   SW,1                                                             
DEMHEAD6 CLC   DUB(5),MODFILE      CORRECT TABLE                                
         BE    DEMHEAD8            YES                                          
         ICM   R1,7,MODAET         GET NEXT TABLE ADDRESS                       
         LA    R1,1(,R1)                                                        
         CLI   0(R1),0             AT END OF TABLES?                            
         BNE   DEMHEAD6                                                         
         CLI   SW,1                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SW,2                                                             
         MVC   DUB+2(2),=X'FFFF'   RECHECK TABLE FOR DEFAULT                    
         B     DEMHEAD5            NOW CK TABLE WITH DEFAULT                    
*                                                                               
DEMHEAD8 DS    0H                  GET ALL MODF FROM TBL. STORE SORTED          
         MVI   DEMOMOD,X'FF'       FILL EMPTY                                   
         MVC   DEMOMOD+1(L'DEMOMOD-1),DEMOMOD TABLE                             
         SR    R3,R3                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,3,MODLDE         TABLE ELEMENT LENGTH                         
         ICM   RF,7,MODAET         END OF TABLE ADDR                            
         BCTR  RF,0                                                             
         LA    R1,MODHDRLN(,R1)    START OF THIS TABLE                          
         LA    R2,DEMOMOD          TABLE TO BE BUILT                            
         LA    R4,DEMHD1           PRINT MODIFIER DEFINITIONS                   
         LA    R0,13               13 ENTRIES IN HEADING 1                      
FILLMOD  CLI   1(R1),C'*'          BYPASS THIS MODIFIER?                        
         BE    FILLMODB            INSERT HERE                                  
         LA    R3,1(,R3)           ADD 1 TO MODIFIER COUNT                      
         MVC   0(1,R2),0(R1)       MOVE MODIFIER TO TABLE                       
         LA    R2,1(,R2)           BUMP MODIFIER TABLE POINTER                  
         MVC   0(1,R4),0(R1)       ALSO TO HEADING                              
         MVI   1(R4),C'='                                                       
         MVC   2(3,R4),2(R1)       MODIFIER NAME                                
* IF MORE THAN 13 MODIFIERS, REMAINDER GO ON HEADING LINE 2,                    
* IF MORE THAN 26 MODIFIERS, THEN ONLY FIRST & LAST 13 WILL SHOW                
         BCT   R0,FILLMODA         ONLY 13                                      
         LA    R4,DEMHD2           START IN ON NEXT HEADING                     
         LA    R0,13                                                            
         B     FILLMODB                                                         
FILLMODA MVI   5(R4),C','                                                       
         LA    R4,6(,R4)                                                        
FILLMODB BXLE  R1,RE,FILLMOD                                                    
         STH   R3,MODCTR           STORE COUNT OF MODIFIERS                     
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          BLANK LAST COMMA                             
         SAC   0                                                                
         LAM   AR1,AR1,=F'0'       CLEAR AR1                                    
         DROP  R1                                                               
         EJECT                                                                  
********************************************************************            
* GET SYST MASTER DISPL TABLE ADDRESS FOR THIS BOOK                             
********************************************************************            
         ICM   R1,7,TMSTR+1                                                     
         BNZ   *+6                                                              
         DC    H'0'                IF NO TABLE ADDRESS, QUIT                    
         MVC   DUB(2),INTFM                                                     
         MVC   DUB+2(1),DBSRC                                                   
         MVC   DUB+3(2),BKS                                                     
         OC    DUB+3(2),DUB+3      TEST FOR LATEST BOOK                         
         BZ    *+10                SEARCH USING ZERO BOOK VALUE                 
         XC    DUB+3(2),=X'FFFF'   CONVERT BOOK TO 2'S COMPLEMENT               
DEMHEADA CLC   DUB(3),0(R1)        THIS POSSIBLE TABLE                          
         BNE   DEMHEADB            NO                                           
         CLC   DUB(5),0(R1)        NOW CHECK BOOK                               
         BNH   DEMHEADC            FOUND TABLE                                  
DEMHEADB ICM   RE,7,DSPAET-DSPHDRD(R1)                                          
         LA    R1,1(R1,RE)                                                      
         CLC   0(2,R1),=XL2'0000'  AT END OF TABLES?                            
         BNE   DEMHEADA            NO, KEEP ON LOOKING                          
         DC    H'0'                                                             
DEMHEADC LA    R1,DSPHDRLN(,R1)          SAVE TABLE START ADDRESS               
         ST    R1,TMSTADR                                                       
         EJECT                                                                  
********************************************************************            
* FIND CORRECT DEMO NAME TABLE                                                  
********************************************************************            
         DS    0H                                                               
         MVC   DUB(2),INTFM        INTERNAL FILE/MEDIA                          
*  FUDGE CANADIAN MEDIA TO LOOK UP USTV TO GET RIGHT TABLE                      
         CLI   DUB+1,C'C'                                                       
         BNE   *+8                                                              
         MVI   DUB+1,C'T'                                                       
         MVC   DUB+2(2),AGYALPH    AGENCY                                       
         MVI   DUB+4,X'FF'         NOT USED YET                                 
         MVI   SW,1                                                             
DEMHEADD ICM   R1,7,TDEMNAM+1      DEMO NAME TABLE ADDR                         
         BNZ   *+6                                                              
         DC    H'0'                IF NO TABLE ADDRESS, QUIT                    
         LAM   AR1,AR1,ALET                                                     
         SAC   512                                                              
                                                                                
DEMHEADE CLC   DUB(5),0(R1)        THIS OK ON FILE/MEDIA                        
         BE    DEMHEADF            YES FOUND TABLE                              
         ICM   R1,7,DSPAET-DSPHDRD(R1)                                          
         LA    R1,1(,R1)                                                        
         CLC   0(2,R1),=XL2'0000'  AT END OF TABLES?                            
         BNE   DEMHEADE            NO, KEEP ON LOOKING                          
         CLI   SW,1                                                             
         BNE   *+18                                                             
         MVI   SW,2                                                             
         MVC   DUB+2(2),=X'FFFF'   RECHECK TABLE FOR DEFAULT                    
         B     DEMHEADD            NOW CK TABLE WITH DEFAULT                    
         CLI   SW,2                                                             
         BNE   *+18                                                             
         MVI   SW,3                                                             
         MVC   DUB(2),=X'FFFF'                                                  
         B     DEMHEADD                                                         
         DC    H'0'                                                             
DEMHEADF MVC   TDNAMEL,NAMLDE-NAMHDRD(R1)                                       
         LA    R1,NAMHDRLN(,R1)    SAVE TABLE START ADDRESS                     
         ST    R1,TDEMADR                                                       
         XC    DEMCTR,DEMCTR       CLEAR DEMO CTR                               
         SPACE 1                                                                
         LA    R2,DBLOCK1          BUILD DBLOCK FOR DEMOVAL/DEMOCON             
         USING DBLOCKD,R2                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBCOMFCS,AFAC                                                    
         MVC   DBSELMED,DBMED                                                   
         CLI   DBMED,C'O'                                                       
         BNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         B     EXIT                                                             
*                                                                               
DEMHD10  DS    0H                  THIS PART FOR STEREO SESSION ONLY            
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         CLI   TSKEYL,0            SEE IF WE NEED TO SET LENGTHS AGA            
         BNE   DEMHD20              NOPE, THEY WERE SET BEFORE                  
                                                                                
         MVI   TSKEYL,TDRKEYL      SET KEY LENGTH                               
                                                                                
         LA    R0,TDRRECL+2                                                     
         STH   R0,TSRECL           SET MAX LENGTH OF RECORD                     
*                                                                               
DEMHD20  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
* BUILD BINSRCH TABLE OF DEMO CODES, NAMES, & MODIFIERS                         
         SPACE 1                                                                
* R2 - MODIFIER TABLE ADDR R0,1,3,4,5 - WORK REGS                               
         SPACE 1                                                                
DEMPROC  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    DMPROC50                                                         
*                                                                               
         LA    R2,DEMOMOD          DEMO MODIFIER TABLE-BUILT IN DEMHEAD         
         XC    DEMRMOD(31),DEMRMOD ZERO BUILD AREA AND DEMVALSW                 
***      MVC   DFLDH,SFLDH         FILL DUMMY FIELD HEADER                      
         MVC   DFLDH,SFLDH2        FILL DUMMY FIELD HEADER                      
         LH    R1,DEMCTR                                                        
         LA    R1,1(,R1)                                                        
         STH   R1,DEMCTR                                                        
         STC   R1,MDCOMP+1         DEMO NUMBER                                  
         STC   R1,DEMONUM          ALSO BUILD AREA                              
         L     R1,TDEMADR          INTO WORK REG                                
         LAM   AR1,AR1,ALET        R1 PTS TO ACTUAL DNAME TABLE                 
         SAC   512                  IN THE DATATSPACE                           
DEMBLDA  CLC   0(1,R1),DEMONUM     FIND DEMO NAME                               
         BE    DEMBLDB                                                          
         BH    DEMBLDH             IF NOT IN TABLE, BYPASS                      
         AH    R1,TDNAMEL          ADD TABLE ELEMENT LENGTH                     
         B     DEMBLDA                                                          
DEMBLDB  DS    0H                                                               
         CLC   DBFIL,=C'CTP'                                                    
         BNE   *+14                                                             
         MVC   DEMONAM(5),NAM5-NAMDTAD(R1) SAVE DEMO NAME                       
         B     *+10                                                             
         MVC   DEMONAM,NAM7-NAMDTAD(R1) SAVE DEMO NAME                          
*                                                                               
         SAC   0                                                                
         LAM   AR1,AR1,=F'0'                                                    
         CLC   DBFIL,=C'CTP'                                                    
         BNE   *+14                                                             
         MVC   DFLDW+1(5),DEMONAM                                               
         B     *+10                                                             
         MVC   DFLDW+1(7),DEMONAM  ALSO SET FOR DEMOVAL                         
         LH    R0,MODCTR           NUMBER OF MODIFIERS                          
         LA    R3,DEMRMOD                                                       
         LA    R4,DEMCMOD                                                       
DEMBLDC  MVC   MDCOMP(1),0(R2)     DEMO NUM STILL THERE, MOVE MODIFIER          
         MVC   DFLDW(1),0(R2)                                                   
*                                                                               
         CLC   DBFIL,=C'CTP'                                                    
         BNE   *+10                                                             
         MVC   DBLOCK1(3),=C'CUN'                                               
*                                                                               
         GOTO1 VDEMOVAL,DMCB,(1,DFLDH),(1,WORK),(C'S',DBLOCK1),0                
*        CLC   =H'16',DEMCTR                                                    
*        BNE   *+6                                                              
*        DC    H'0'                                                             
         CLI   0(R1),0             WAS THERE AN ERROR                           
         BNE   DEMBLDG                                                          
         MVI   DEMVALSW,1          SET ON DEMO VALID SW                         
         L     R1,TMSTADR                                                       
DEMBLDD  CLI   0(R1),X'FF'         TEST E-O-T                                   
         BE    DEMBLDF                                                          
         CLC   MDCOMP(2),0(R1)                                                  
         BE    DEMBLDE                                                          
         LA    R1,5(R1)                                                         
         B     DEMBLDD                                                          
DEMBLDE  MVC   0(1,R3),0(R2)       FROM RATING SERVICE                          
         LA    R3,1(,R3)                                                        
         B     DEMBLDG                                                          
DEMBLDF  MVC   0(1,R4),0(R2)       CALCULATED MODIFER                           
         LA    R4,1(,R4)                                                        
DEMBLDG  LA    R2,1(,R2)                                                        
         BCT   R0,DEMBLDC          CHECK ALL MODIFIERS EACH DEMO                
         CLI   DEMVALSW,0          WERE ANY VALID MODIFIERS FOUND               
         BE    DEMBLDH             NO                                           
         MVC   POSTLINE(BINEND-BINRECD),DEMONAM                                 
         GOTO1 APOST                                                            
*                                                                               
DEMBLDH  SAC   0                                                                
         LAM   AR1,AR1,=F'0'                                                    
         CLC   DEMCTR,=H'256'      AT END OF DEMOS                              
         BL    DEMPROC                                                          
*&&DO                                                                           
         B     EXIT                                                             
*&&                                                                             
         B     DEMPRCX                                                          
***************** DEM32 PROCESS *********                                       
                                                                                
*                                                                               
DMPROC50 LA    R5,DBLOCK1          INITIALIZE DBLOCK FOR MARKET READS           
*                                                                               
         LA    R4,TSARBLCK                                                      
         USING TSARD,R4                                                         
         SR    R1,R1                                                            
         ICM   R1,7,TSAREC+1                                                    
         MVC   0(2,R1),TSRECL      MOVE IN LENGTH OF RECORD                     
         LA    R1,2(R1)                                                         
                                                                                
         USING TSDEMRCD,R1                                                      
         MVC   TDRBOOK,=X'0260'     MOVE IN DUMMY BOOK                          
         MVI   TDRTYPE,TDRTBKQ      RECORD TYPE,                                
         MVI   TDRDUMMY,0           AND A DUMMY VALUE                           
         DROP  R1                                                               
                                                                                
         GOTO1 APOST                                                            
DEMPRCX  DS    0H                                                               
*&&DO                                                                           
         B     EXIT                                                             
*&&                                                                             
         B     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
*                                                                               
* R0, R3, & R4 WORK REGISTERS                                                   
* R2 - PRINT LINE POINTER                                                       
* R5 - ADDR OF WORK AREA FOR NEXT BINSRCH RECORD                                
*                                                                               
***********************************************************************         
* FORMAT PRINT LINES OF ALL DEMOS                                               
*                                                                               
DEMLINE  DS    0H                                                               
         TM    DEMFLAG1,DF1STERO+DF1DEM32                                       
         BO    DMLINE20                                                         
*                                                                               
         L     R5,BINAREC                                                       
         USING BINRECD,R5          R5=A(RECORD)                                 
         CLI   BINDNAM,X'FF'       IGNORE DUMMY E-O-F RECORDS                   
         BE    EXIT                                                             
         LA    R2,LINE1                                                         
         USING PLINED,R2                                                        
         EDIT  (B1,BINDEMO),(3,PDEMO) EDIT DEMO NUMBER                          
         MVC   PDNAM,BINDNAM       MOVE IN DEMO NAME                            
* CODE TO PRINT OUT RATING SERVICE MODIFIERS                                    
         CLI   BINRMOD,0           ANY MODIFIERS TO PRINT                       
         BE    DEMLINE4                                                         
         LA    R0,(L'PDEMRTG/2)+1                                               
         LA    R3,PDEMRTG          PRINT LINE PTR                               
         LA    R4,BINRMOD                                                       
DEMLINE2 MVC   0(1,R3),0(R4)       MOVE IN MODIFIER                             
         CLI   1(R4),0             ANY MODIFIERS LEFT                           
         BE    DEMLINE4                                                         
         MVI   1(R3),C','                                                       
         LA    R3,2(,R3)           BUMP PRINT PTR                               
         LA    R4,1(,R4)           BUMP MODIFIER PTR                            
         BCT   R0,DEMLINE2                                                      
         DC    H'0'                                                             
* CODE TO PRINT OUT CALCULATED MODIFIERS                                        
DEMLINE4 CLI   BINCMOD,0           ANY MODIFIERS TO PRINT                       
         BE    DEMLINE8            NO                                           
         LA    R0,(L'PDEMCAL/2)+1                                               
         LA    R3,PDEMCAL          PRINT LINE PTR                               
         LA    R4,BINCMOD                                                       
DEMLINE6 MVC   0(1,R3),0(R4)       MOVE IN MODIFIER                             
         CLI   1(R4),0             ANY MODIFIERS LEFT                           
         BE    DEMLINE8                                                         
         MVI   1(R3),C','                                                       
         LA    R3,2(,R3)           BUMP PRINT PTR                               
         LA    R4,1(,R4)           BUMP MODIFIER PTR                            
         BCT   R0,DEMLINE6                                                      
         DC    H'0'                                                             
DEMLINE8 B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
DMLINE20 DS    0H                                                               
***********************************************************                     
* DEM32 DEMLINE AREA.                                                           
*                                                                               
*======= GET DEMOS AND DEMO NAMES                                               
*                                                                               
         MVC   MYFILSRC(L'DBFIL),DBFIL                                          
         MVC   MYFILSRC+L'DBFIL(L'DBSRC),DBSRC                                  
         MVC   MYFILSRC+L'DBFIL+L'DBSRC(L'DBMED),DBMED                          
* FIND MATCH BETWEEN MYFILSRC IN ADDRESS TABLE                                  
         L     RE,ADEMFIL                                                       
         CLC   =C'LPM',8(RE)                                                    
         BNE   NOTLPM                                                           
         L     R3,=A(DEMOSTB8)                                                  
         A     R3,RELO05                                                        
         B     FNDADDR2                                                         
NOTLPM   DS    0H                                                               
*                                                                               
         LA    R4,ADEMOSTB                                                      
LOOKATB  CLC   =X'0000',0(R4)                                                   
         BNE   *+6                                                              
         DC    H'0'               IF NOT IN TABLE , DIE!!!                      
         CLC   MYFILSRC,0(R4)                                                   
         BE    FNDADDR                                                          
         LA    R4,ADEMOSTL(R4)                                                  
         B     LOOKATB                                                          
*                                                                               
FNDADDR  DS    0H                                                               
         LA    R4,ADEMDISP(R4)                                                  
         L     R3,0(R4)                                                         
         A     R3,RELO05                                                        
         LA    R6,0                                                             
***      LA    R4,DMSTB1X           REUSE R4 -POINT TO END OF DEMOS TAB         
FNDADDR2 DS    0H                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   FALEMPC,=Y(FMHDEMOS)                                             
         SR    R1,R1                                                            
         GOTO1 ADM32SET                                                         
*                                                                               
DEMOSLP  CLC   =X'0004',0(R3)       CATEGORY DATA CODE                          
         BNE   NOTCAT               NOT A CATEGORY                              
         MVC   FALDMPC,=Y(FMDDMCAT)                                             
         XC    WORK,WORK                                                        
         MVC   WORK(DEMOSNML),2(R3)                                             
         LA    R0,WORK                                                          
         LA    R1,DEMOSNML                                                      
         LA    R3,DMCATLEN(R3)                                                  
         BAS   RE,D32DLGD                                                       
*                                                                               
NOTCAT   MVC   WORK(DEMOSLQ),0(R3)                                              
         LA    R0,WORK                                                          
         LA    R1,DEMOSLQ                                                       
         MVC   FALDMPC,=Y(FMDDEMOS)                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(DEMOSNML),DMSDDISP(R3)  DESCRIPTION                         
         LA    R0,WORK                                                          
         LA    R1,DEMOSNML                                                      
         MVC   FALDMPC,=Y(FMDDMSNM)                                             
         BAS   RE,D32DLGD                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(DEMOTYPL),DMTPDISP(R3)  DEMO TYPES                          
         LA    R0,WORK                                                          
         LA    R1,DEMOTYPL                                                      
         LA    R3,DEMOSTL(R3)                                                   
         MVC   FALDMPC,=Y(FMDDMSTP)                                             
         BAS   RE,D32DLGD                                                       
**       CR    R3,R4                                                            
**       BL    DEMOSLP                                                          
         CLC   =X'0000',0(R3)                                                   
         BNE   DEMOSLP                                                          
*                                                                               
DEMLINX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
D32DLGD  NTR1                                                                   
         ST    R0,ADLDATA                                                       
         ST    R1,LDLDATA                                                       
         MVI   ADMODE,ADMADD                                                    
         GOTO1 ADM32ADD                                                         
         XIT1                                                                   
***********************************************************                     
* LITERALS ETC.                                                                 
*                                                                               
         DC    XL30'00'            *** PATCH AREA FOR TESTING ONLY **           
         LTORG                                                                  
         SPACE 1                                                                
HEADING  DC    CL49'NUM   DEMONAME   RATING SERVICE ',CL11'CALCULATED'          
*                   999   XXXXXXX    XXXXXXXXXXXXXXX   XXXXXXXXXXXXXXX          
*                      333   7   4444     29        333     29                  
         SPACE 1                                                                
* TABLSTL USED TO GET MODIFIER & MASTER DEMO TABLE ADDRESSES                    
TABLIST  DS    0CL1                                                             
         DC    X'D6',XL3'0'        DEMO MODIFIER TABLE                          
         DC    X'E2',XL3'0'        CONVERT FILES & SUBFILES                     
         DC    X'D5',XL3'0'        DEMO NAME TABLE                              
         DC    X'D0',XL3'0'        DEMO SYST MASTER DISPL TABLE                 
         DC    X'FF'               END OF LIST MARKER                           
TABLISTL EQU   *-TABLIST                                                        
SFLDH    DS    0XL8                                                             
         DC    AL1(28)             DUMMY FLDH AND WORK FLD LEGTH                
         DC    X'00'               ATTRIBUTE BYTE                               
         DC    AL2(80*1)           START OF DATA SCREEN ADDRESS                 
         DC    X'80'               INPUT INDICATORS                             
         DC    AL1(8)              INPUT DATA LENGTH                            
         DC    X'00'               OUTPUT INDICATORS                            
         DC    AL1(8)              OUTPUT LENGTH                                
SFLDH2   DS    0XL8                                                             
         DC    AL1(26)             DUMMY FLDH AND WORK FLD LEGTH                
         DC    X'00'               ATTRIBUTE BYTE                               
         DC    AL2(80*1)           START OF DATA SCREEN ADDRESS                 
         DC    X'80'               INPUT INDICATORS                             
         DC    AL1(6)              INPUT DATA LENGTH                            
         DC    X'00'               OUTPUT INDICATORS                            
         DC    AL1(6)              OUTPUT LENGTH                                
         EJECT                                                                  
* CONTAINS THE A(OF DEMOS TABLE TO POINT TO) FOR EACH SRC\FIL COMBO             
ADEMOSTL  EQU   9     LENGTH OF EACH ADDRESS TABLE ENTRY                        
ADEMOSTN  EQU   8     NUMBER OF ENTRIES IN ADDR TABLE                           
ADEMDISP  EQU   5     DISPLACEMENT TO ADDRESS OF TABLE                          
*                                                                               
ADEMOSTB DC    CL5'TP NT',AL4(DEMOSTB5)                                         
         DC    CL5'PAVNT',AL4(DEMOSTB5)                                         
         DC    CL5'TP NW',AL4(DEMOSTB2)   NSI/WTP                               
         DC    CL5'TP ST',AL4(DEMOSTB5)                                         
         DC    CL5'TP MT',AL4(DEMOSTB5)                                         
         DC    CL5'PAVMT',AL4(DEMOSTB5)                                         
         DC    CL5'TP AT',AL4(DEMOSTB7)                                         
         DC    CL5'TP AR',AL4(DEMOSTB1)                                         
         DC    CL5'TP HN',AL4(DEMOSTB4)   NHTI/TP                               
         DC    CL5'PAVHN',AL4(DEMOSTB4)   NHTI/PAV                              
*****    DC    CL5'TP AC',AL4(DEMOSTB1)                                         
         DC    CL5'TP AC',AL4(DEMOSTB3)                                         
***      DC    CL5'TP NC',AL4(DEMOSTB1)                                         
         DC    CL5'TP NC',AL4(DEMOSTB6)                                         
         DC    CL5'TP RR',AL4(DEMOSTB1)   RADAR                                 
         DC    CL5'TP NO',AL4(DEMOSTB5)   OVERNIGHTS TP                         
         DC    CL5'CTPNU',AL4(DEMOSTB9)   COUNTY COVERAGE                       
         DC    CL5'TP FT',AL4(DEMOSTB5)   FUSION                                
         DC    CL5'PAVNO',AL4(DEMOSTB5)   OVERNIGHTS PAV                        
         DC    AL2(0)                                                           
*                                                                               
MYFILSRC DS    CL(L'DBFIL+L'DBSRC+L'DBMED)                                      
       ++INCLUDE DEDEMLIST                                                      
*                                                                               
*                                                                               
*                                                                               
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* DEDEMWRK                                                                      
       ++INCLUDE DEDEMWRK                                                       
         ORG   APSAVE                                                           
         SPACE 1                                                                
* SAVE STORAGE FOR OVERLAY                                                      
*                                                                               
         ORG                                                                    
         EJECT                                                                  
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
TMSTADR  DS    F                                                                
TDEMADR  DS    F                                                                
MODCTR   DS    H                   COUNT OF MODIFIERS                           
DEMCTR   DS    H                   DEMO CTR                                     
TDNAMEL  DS    H                                                                
INTFM    DS    CL2                                                              
ALET     DS    A                                                                
TABLST   DS    0XL(TABLISTL)                                                    
TMODIF   DS    XL4                                                              
TFLCNVT  DS    XL4                                                              
TDEMNAM  DS    XL4                                                              
TMSTR    DS    XL4                                                              
         ORG   TABLST+TABLISTL                                                  
DEMOMOD  DS    CL26                LIST OF MODIFIERS FROM DEMADDR               
DEMONAM  DS    CL7                 DEMO NAME                                    
DEMONUM  DS    XL1                 DEMO NUMBER                                  
DEMRMOD  DS    XL15                RATING SERVICE MODIFIERS                     
DEMCMOD  DS    XL15                CALCULATED MODIFIERS                         
DEMVALSW DS    0XL1                THEN AS DEMO VALID SW                        
SW       DS    XL1                 USED FIRST FOR DEFAULT TABLE SW              
MDCOMP   DS    CL2                 MODIFIER & DEMO NUMBER FOR SEARCH            
         SPACE 1                                                                
DFLDH    DS    CL8                 FLD H & WORK FOR DEMOVAL                     
DFLDW    DS    CL20                VALIDATION OF MODIFIERS & DEMO NAMES         
RELO05   DS    F                                                                
         SPACE 1                                                                
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINDNAM  DS    CL7                 DEMO NAME                                    
BINDEMO  DS    XL1                 DEMO NUMBER                                  
BINRMOD  DS    XL15                RATING SERVICE MODIFIERS                     
BINCMOD  DS    XL15                CALCULATED MODIFIERS                         
BINEND   EQU   *                                                                
*                                                                               
* DSECT TO COVER TSAR DEMO RECORD                                               
*                                                                               
* DSECT TO COVER TSAR DEMO RECORD                                               
*                                                                               
TSDEMRCD DSECT                                                                  
TDRKEY   DS    0X                  KEY OF TSAR DEMO RECORD                      
TDRTYPE  DS    XL1                  RECORD TYPE                                 
TDRTMSGQ EQU   X'10'                 (DUMMY) MESSAGE RECORD                     
TDRTBKQ  EQU   X'20'                 BOOK RECORD                                
TDRBOOK  DS    XL2                  BOOK VALUE                                  
TDRKEYL  EQU   *-TSDEMRCD          KEY LENGTH                                   
TDRDUMMY DS    XL1                  DUMMY DATA FIELD                            
TDRRECL  EQU   *-TSDEMRCD          REC LENGTH                                   
*                                                                               
PLINED   DSECT                                                                  
PDEMO    DS    CL3                                                              
         DS    CL3                                                              
PDNAM    DS    CL7                                                              
         DS    CL4                                                              
PDEMRTG  DS    CL29                                                             
         DS    CL3                                                              
PDEMCAL  DS    CL29                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'092DEDEM05   05/20/15'                                      
         END                                                                    
