*          DATA SET FABOOTA    AT LEVEL 003 AS OF 12/20/00                      
*          DATA SET FABOOT     AT LEVEL 026 AS OF 12/19/00                      
*PHASE FACPAKA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
         TITLE 'FABOOT - FACILITIES BOOT STRAP'                                 
         PRINT NOGEN                                                            
FABOOT   CSECT                                                                  
         NBASE 0,**FABOOT,WORK=WORK                                             
         ST    R1,BOOTCMRG                                                      
         L     R1,0(R1)            GET MVS PARM LIST FOR UPSI VALUE             
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,0(R1)          R0=L'PARM DATA                               
         BZ    BOOT02                                                           
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         BRAS  RE,GETUPSI                                                       
*                                                                               
BOOT02   LA    R2,PARM             *CARE* THE RETURN ORDER IS FIXED             
         EXTRACT (R2),FIELDS=(TIOT,ASID)                                        
         L     RF,PARM                                                          
         MVC   MVSNAME,0(RF)       GET JOBNAME FROM TIOT                        
         MVC   ASIDFLD,PARM+4      GET ASID                                     
*                                                                               
         LA    RF,MVSNAME+L'MVSNAME-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         AHI   RF,1                                                             
         LA    RE,MVSNAME                                                       
         SR    RF,RE                                                            
         STH   RF,MVSNAMEL         SET LENGTH OF JOBNAME                        
*                                                                               
*&&UK*&& TM    BOOTUPSI,X'80'      UPSI 1 FOR CARD DECK INPUT                   
*&&UK*&& BZ    BOOT30                                                           
         EJECT                                                                  
***********************************************************************         
* READ AND PROCESS INPUT PARAMETERS                                   *         
* NOTE: THERE ARE 2 TYPES OF BOOK                                     *         
* 1. A NORMAL BOOK FOR A SINGLE FACPAK                                *         
* 2. A BOOK COVERING MULTIPLE FACPAKS                                 *         
* FOR (2), THE FACPAKS ARE SEPERATED BY *START <NAME> WITH A CARD     *         
* *END TO DENOTE THE END OF THE CARDS FOR THIS FACPAK                 *         
***********************************************************************         
         SPACE 1                                                                
BOOT04   GOTO1 VCARDS,PARM,CARD,=C'RE00'                                        
         CLC   =C'/*',CARD                                                      
         BE    BOOTERR4                                                         
         CLC   =C'*START',CARD     MULTIPLE FACPAK BOOK START?                  
         BE    BOOT06              YES                                          
         TM    BFLAGS,BFSTART                                                   
         BZ    BOOT12                                                           
         B     BOOT04                                                           
*                                                                               
BOOT06   OI    BFLAGS,BFMULTI+BFSTART                                           
         LA    RF,CARD+L'CARD-1    FIND LAST NON-SPACE                          
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RE,CARD+6           FIND FIRST NON-SPACE AFTER *START            
BOOT08   CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         AHI   RE,1                                                             
         B     BOOT08                                                           
*                                                                               
         CR    RF,RE               MAKE SURE LENGTHS LOOK GOOD                  
         BL    BOOTERR1                                                         
         AHI   RF,1                                                             
         SR    RF,RE               RF HOLDS LENGTH OF NAME                      
*                                                                               
         CH    RF,MVSNAMEL         MAKE SURE NAMES ARE SAME LENGTH              
         BNE   BOOT04                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8              NOW TRY TO MATCH NAMES                       
         BNE   BOOT04              NO GOOD                                      
         CLC   MVSNAME(0),0(RE)                                                 
*                                                                               
BOOT10   GOTO1 VCARDS,PARM,CARD,=C'RE00'                                        
         CLC   CARD(2),=C'/*'                                                   
         BE    BOOT30                                                           
         TM    BFLAGS,BFMULTI     MULTIPLE FACPAK BOOK?                         
         BZ    BOOT12             NO                                            
*                                                                               
         CLC   =C'*START',CARD    IGNORE ANY OTHER *START CARDS                 
         BE    BOOT10                                                           
         CLC   =C'*END',CARD      C'*END' MEANS WE'RE THROUGH                   
         BE    BOOT30                                                           
*                                                                               
BOOT12   CLI   CARD,C'*'          IGNORE COMMENTS/COMMANDS                      
         BE    BOOT10                                                           
         CLC   =C'FANET=',CARD    NO LONGER USED                                
         BE    BOOT10                                                           
         CLC   =C'FAPHLS=',CARD   NO LONGER USED                                
         BE    BOOT10                                                           
         CLC   =C'FASTART=',CARD  NO LONGER USED                                
         BE    BOOT10                                                           
*                                                                               
         CLC   =C'FACPAK=',CARD    OVERRIDE FACPAK                              
         BNE   BOOT14                                                           
         MVC   FACPAK,CARD+7                                                    
         B     BOOT10                                                           
*                                                                               
BOOT14   CLC   =C'FAPARMS=',CARD   OVERRIDE FAPARMS                             
         BNE   BOOT16                                                           
         MVC   FAPARMS,CARD+8                                                   
         B     BOOT10                                                           
*                                                                               
BOOT16   CLC   =C'TASKS=',CARD     OVERRIDE NUMBER OF TASKS                     
         BNE   BOOT18                                                           
         LHI   R1,0                SET PACKING 1 BYTE                           
         CLI   CARD+6,C'0'         MAKE SURE NUMERIC                            
         BL    BOOTERR1                                                         
         CLI   CARD+6,C'9'                                                      
         BH    BOOTERR1                                                         
*                                                                               
         CLI   CARD+7,C' '         ONE OR TWO BYTE INPUT?                       
         BNH   BOOT16A             ONE                                          
*                                                                               
         LHI   R1,1                SET PACKING 2 BYTES                          
         CLI   CARD+7,C'0'         MAKE SURE NUMERIC                            
         BL    BOOTERR1                                                         
         CLI   CARD+7,C'9'                                                      
         BH    BOOTERR1                                                         
*                                                                               
BOOT16A  EX    R1,BOOT16PK         NOTE TASKS=0 MEANS USE FAPARMS               
         CVB   R1,DUB                                                           
         CHI   R1,SBMXTSK                                                       
         BH    BOOTERR1                                                         
         STC   R1,FATASKS                                                       
         B     BOOT10                                                           
*                                                                               
BOOT16PK PACK  DUB,CARD+6(0)                                                    
*                                                                               
BOOT18   CLC   =C'UPSI=',CARD      OVERRIDE UPSI VALUE                          
         BNE   BOOT20                                                           
         MVI   BOOTUPSI,0                                                       
         LA    R1,CARD+5                                                        
         LA    R0,8                                                             
         BRAS  RE,GETUPSI                                                       
         B     BOOT10                                                           
*                                                                               
BOOT20   CLC   =C'PROTECT=',CARD   SET FACPAK STORAGE PROTECTION ON             
         BNE   BOOT22                                                           
         CLI   CARD+8,C'Y'                                                      
         BNE   BOOT10                                                           
         MVI   FAPROT,C'Y'                                                      
         B     BOOT10                                                           
*                                                                               
BOOT22   CLC   CARD(4),=C'AOR='    SET FACPAK AS AN AOR                         
         BNE   BOOT24                                                           
         MVI   FATOR,C'N'          SET FACPAK IS NOT TOR                        
         MVC   FAAORID,CARD+4      SET AOR IDENTIFIER                           
         B     BOOT10                                                           
*                                                                               
BOOT24   B     BOOTERR1                                                         
         EJECT                                                                  
***********************************************************************         
* WRITE OFF FACPAK INFORMATION FOR RECOVER PROGRAM                    *         
***********************************************************************         
         SPACE 1                                                                
BOOT30   OPEN  (ASIDHELP,OUTPUT)                                                
         LTR   RF,RF                                                            
         BNZ   BOOTERR5                                                         
*                                                                               
         XC    CARD,CARD                                                        
T        USING FAASIDD,CARD                                                     
         MVC   T.FAASNAME,MVSNAME                                               
         MVC   T.FAASASID,ASIDFLD                                               
         MVC   T.FAASTOR,FATOR                                                  
         MVC   T.FAASAORN,FAAORID                                               
*                                                                               
         PUT   ASIDHELP,CARD                                                    
         CLOSE ASIDHELP                                                         
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* FIND LOW/HIGH CORE AVAILABLE FOR FACPAK                             *         
***********************************************************************         
         SPACE 1                                                                
         CLI   FAPROT,C'Y'         FACPAK STORAGE PROTECTION?                   
         BE    BOOT32              YES                                          
*                                                                               
         GETMAIN VC,LA=COREMIN,A=COREADR,BNDRY=PAGE                             
         LTR   RF,RF               GETMAIN ERROR?                               
         BNZ   BOOTERR2                                                         
         FREEMAIN VC,A=COREADR     GET AND FREE MAX CORE AVAILABLE              
         LTR   RF,RF                                                            
         BNZ   BOOTERR2                                                         
         B     BOOT34                                                           
*                                                                               
BOOT32   GETMAIN VC,LA=COREMIN,A=COREADR,BNDRY=PAGE,SP=132                      
         LTR   RF,RF               GETMAIN ERROR?                               
         BNZ   BOOTERR2                                                         
         FREEMAIN VC,A=COREADR,SP=132  GET AND FREE MAX CORE AVAILABLE          
         LTR   RF,RF                                                            
         BNZ   BOOTERR2                                                         
*                                                                               
BOOT34   L     R1,CORELEN          GET AMOUNT OF CORE ACQUIRED                  
         S     R1,CORESPR          MINUS SPARE CORE REQUIRED                    
         BNP   BOOTERR2                                                         
*                                                                               
         LA    R1,2047(R1)         SET MODULUS 2K                               
         SRL   R1,11                                                            
         SLL   R1,11                                                            
         ST    R1,COREMAX          SET NEW MAX CORE REQUIRED                    
         XC    COREMIN,COREMIN                                                  
*                                                                               
         CLI   FAPROT,C'Y'         TEST IF FACPAK STORAGE PROTECTION            
         BE    BOOT36                                                           
*                                                                               
         GETMAIN VC,LA=COREMIN,A=COREADR,BNDRY=PAGE                             
         LTR   RF,RF                                                            
         BNZ   BOOTERR2                                                         
         B     BOOT38                                                           
*                                                                               
BOOT36   GETMAIN VC,LA=COREMIN,A=COREADR,BNDRY=PAGE,SP=132                      
         LTR   RF,RF                                                            
         BNZ   BOOTERR2                                                         
*                                                                               
BOOT38   CLC   CORELEN,COREMAX     CHECK THAT WE GOT WHAT WE WANTED             
         BNE   BOOTERR2                                                         
                                                                                
         L     RF,COREADR                                                       
         ST    RF,BOOTLOCO         SAVE LOW ADDR OF CORE ACQUIRED               
         A     RF,CORELEN          ADD LENGTH OF CORE ACQUIRED                  
         BCTR  RF,0                                                             
         ST    RF,BOOTHICX         SAVE HIGH ADDR OF CORE ACQUIRED              
         B     BOOT40                                                           
         EJECT                                                                  
***********************************************************************         
* LOAD IN FACPAK AND PASS CONTROL TO IT                               *         
***********************************************************************         
         SPACE 1                                                                
BOOT40   L     R0,BOOTLOCO                                                      
         GOTO1 VLOADER,PARM,FACPAK,(R0),(C'A',(R0))                             
         ICM   RF,15,4(R1)         GET LOAD ADDRESS OF FACPAK MODULE            
         BZ    BOOTERR3                                                         
         ST    RF,BOOTLOCO                                                      
         L     RE,0(R1)            GET LENGTH OF FACPAK MODULE                  
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         ST    RE,BOOTHICO         SET HIGH CORE OF FACPAK MODULE               
*                                                                               
         CLI   FAPROT,C'Y'         TEST IF FACPAK STORAGE PROTECTION            
         BE    BOOTPROT                                                         
*                                                                               
         LA    R1,BOOTPARM                                                      
         BASR  RE,RF               PASS CONTROL TO FACPAK MODULE                
         B     BOOTEOJ                                                          
*                                                                               
BOOTPROT EQU   *                   BOOTUP WITH FACPAK STORAGE PROT              
         BAS   RE,SETPCS           SET UP PC SERVICE ENTRY TABLE                
*                                                                               
         LA    R1,BOOTPARM                                                      
         L     RE,SERV1PC                                                       
         PC    0(RE)                                                            
*        BASR  RE,RF               PASS CONTROL TO FACPAK MODULE                
                                                                                
         BAS   RE,CLRPCS           CLEAR PC SERVICE ENTRY TABLE                 
         B     BOOTEOJ                                                          
         EJECT                                                                  
***********************************************************************         
* ERROR CONDITIONS                                                    *         
***********************************************************************         
         SPACE 1                                                                
BOOTERR1 LA    R0,ERROR1           INVALID PARAMETER CARD                       
         B     BOOTERRX                                                         
*                                                                               
BOOTERR2 DC    H'0'                NOT ENOUGH CORE AVAILABLE                    
         B     BOOTERRX                                                         
*                                                                               
BOOTERR3 LA    R0,ERROR3           FACPAK PHASE NOT FOUND                       
         B     BOOTERRX                                                         
*                                                                               
BOOTERR4 LA    R0,ERROR4           NO *START CARD FOR THIS FACPAK               
         B     BOOTERRX                                                         
*                                                                               
BOOTERR5 LA    R0,ERROR5           ERROR OPENING ASIDHELP FILE                  
         B     BOOTERRX                                                         
*                                                                               
BOOTERRX GOTO1 VLOGIO,PARM,X'FF000001',(40,(R0))                                
         B     BOOTEOJ                                                          
*                                                                               
ERROR1   DC    CL40'INVALID PARAMETER CARD'                                     
ERROR2   DC    CL40'SNOT ENOUGH CORE AVAILABLE'                                 
ERROR3   DC    CL40'FACPAK PHASE NOT FOUND'                                     
ERROR4   DC    CL40'NO *START CARD FOR JOBNAME FOUND'                           
ERROR5   DC    CL40'ERROR OPENING ASIDHELP FILE'                                
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
*                                                                               
BOOTEOJ  XBASE ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RETURN UPSI VALUE                                        *         
***********************************************************************         
         SPACE 1                                                                
GETUPSI  LA    RF,GETUPSIT                                                      
         CLI   0(R1),C'1'                                                       
         BNE   *+10                                                             
         OC    BOOTUPSI(1),0(RF)                                                
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         BCT   R0,GETUPSI+4                                                     
         BR    RE                                                               
*                                                                               
GETUPSIT DC    X'8040201008040201'                                              
         EJECT                                                                  
***********************************************************************         
* SET UP PC SERVICE LINKAGE AND ENTRY TABLES                          *         
***********************************************************************         
         SPACE 1                                                                
SETPCS   NTR1  ,                                                                
         MODESET KEY=NZERO,MODE=SUP                                             
*                                  INITIALISE LINKAGE TABLE                     
         LHI   R2,1                                                             
         ST    R2,LXCOUNT          REQUEST 1 LX                                 
GETLX    LXRES LXLIST=LXL,RELATED=(FREELX,CONET)                                
*                                                                               
*                                  INITIALISE ENTRY TABLE                       
         L     R2,BOOTLOCO                                                      
         ETDEF TYPE=SET,ETEADR=ETD1,ROUTINE=(2),SSWITCH=NO,RAMODE=24,  X        
               STATE=PROBLEM,AKM=(8,9),EKM=(9)                                  
*                                                                               
CET1     ETCRE ENTRIES=ETDESC,RELATED=(CONET,DISET1,DESET1)                     
         ST    R0,TKVALUE          SAVE RETURNED TOKEN                          
*                                                                               
*                                  CONSTRUCT PC NUMBERS                         
         L     R2,LXVALUE          LX=PC# WITH EX OF 0                          
         LA    R2,0(,R2)           CONTRUCT EX=0 PC#                            
         ST    R2,SERV1PC                                                       
*                                  CONNECT ENTRY TABLE TO LINKAGE TABLE         
         LA    R2,1                                                             
         ST    R2,TKCOUNT          SET COUNT OF ETS                             
CONET    ETCON TKLIST=TKL,LXLIST=LXL,RELATED=(GETLX,CET1)                       
*                                                                               
         MODESET KEY=NZERO,MODE=PROB                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CLEAR PC SERVICE LINKAGE AND ENTRY TABLES                           *         
***********************************************************************         
         SPACE 1                                                                
CLRPCS   NTR1  ,                                                                
         MODESET KEY=NZERO,MODE=SUP                                             
*                                  DISCONNECT ENTRY/LINKAGE TABLE               
DISET1   ETDIS TKLIST=TKL,RELATED=CET1                                          
*                                  DESTROY ENTRY TABLE                          
DESET1   ETDES TOKEN=TKVALUE,RELATED=CET1                                       
*                                  FREE LX FOR REUSE                            
FREELX   LXFRE LXLIST=LXL,RELATED=GETLX                                         
*                                                                               
         SLR   R2,R2               ZERO VALUE                                   
RESETAX  AXSET AX=(2)              RESET AX TO ZERO                             
*                                                                               
         MODESET KEY=NZERO,MODE=PROB                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ENTRY POINT DEFINITIONS                                             *         
***********************************************************************         
         SPACE 1                                                                
ETDESC   ETDEF TYPE=INITIAL                                                     
ETD1     ETDEF TYPE=ENTRY,ROUTINE=0,SSWITCH=NO,RAMODE=24,              X        
               STATE=PROBLEM,AKM=(8,9),EKM=(9)                                  
         ETDEF TYPE=FINAL                                                       
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
DUB      DC    D'0'                                                             
*                                                                               
COREADR  DC    F'0'                ADR CORE ACQUIRED                            
CORELEN  DC    F'0'                LEN CORE ACQUIRED                            
COREMIN  DC    F'00400000'         MIN CORE REQUIRED                            
COREMAX  DC    F'16000000'         MAX CORE REQUIRED                            
CORESPR  DC    F'00200000'         SPR CORE REQUIRED                            
*                                                                               
VCARDS   DC    V(CARDS)                                                         
VLOADER  DC    V(LOADER)                                                        
VLOGIO   DC    V(LOGIO)                                                         
*                                                                               
ASIDFLD  DC    F'0'                                                             
*                                                                               
PARM     DS    2D                                                               
CARD     DS    CL80                                                             
BFLAGS   DS    XL1                 INTERNAL FLAGS                               
BFMULTI  EQU   X'80'               PARAMETERS ARE FOR MULTIPLE FACPAKS          
BFSTART  EQU   X'40'               STARTED READING CARDS                        
MVSNAMEL DS    H                                                                
MVSNAME  DS    CL8                                                              
*                                                                               
SERVBLK  EQU   *                                                                
LXL      DS    0F                  LX LIST                                      
LXCOUNT  DS    F                   NUMBER OF LXS REQUESTED                      
LXVALUE  DS    F                   LX RETURNED BY LXRES                         
AXL      DS    0F                  AX LIST                                      
AXCOUNT  DS    F                   NUMBER OF AXS REQUESTED                      
AXVALUE  DS    F                   AX RETURNED BY AXES                          
TKL      DS    0F                  TOKEN LIST                                   
TKCOUNT  DS    F                   NUMBER OF ETS CREATED                        
TKVALUE  DS    F                   TOKEN RETURNED BY ETCRE                      
PCTAB    DS    0F                  TABLE OF PC NUMBERS                          
SERV1PC  DS    F                   PC NUMBER FOR SERVICE 1                      
SERV2PC  DS    F                   PC NUMBER FOR SERVICE 2                      
*                                                                               
ASIDHELP DCB   DDNAME=ASIDHELP,MACRF=(PM),RECFM=FB,LRECL=80,           +        
               BLKSIZE=400,DSORG=PS                                             
*                                                                               
BOOTPARM DS    0D             ***  PARAM LIST TO FACPAK PHASE                   
BOOTCMRG DS    F                   ADDR DOS COMRG/MVS PARM LIST                 
BOOTUPSI DS    F                   UPSI BYTE                                    
BOOTLOCO DS    F                   LOW  CORE OF FACPAK PHASE                    
BOOTHICO DS    F                   HIGH CORE OF FACPAK PHASE                    
BOOTHICX DS    F                   HIGH CORE OF DOS PARTN/MVS ADRSPC            
*&&UK                                                                           
FACPAK   DC    CL8'FACPAK2'        MODULE NAME FACPAK                           
FASTART  DC    CL8'FASTART'        MODULE NAME FASTART                          
FACPAKP  DC    CL8' '              MODULE NAME SUBSIDIARY FACPAK                
FAPARMS  DC    CL8'FAPARMS'        MODULE NAME RUN PARAMETERS                   
FATASKS  DC    XL1'00'             OVERRIDE TASKS IN FAPARMS                    
FAPROT   DC    XL1'00'             FACPAK STORAGE PROTECTION FLAG               
FATOR    DC    CL1'Y'              FACPAK IS A TOR                              
FAAORID  DC    CL1' '              AOR ID                                       
         DC    CL4' '              N/D                                          
MSGLIB   DC    CL8' '              ANSWER LODE PROGRAM LIB                      
MSGRST   DS    CL8' '              ANSWER IS THIS A RESTART                     
MSGDAY   DS    CL8' '              ANSWER DAY VALUE                             
MSGSYSO  DS    CL8' '              ANSWER SYSTEM OVERRIDE                       
MSGLINO  DS    CL8' '              ANSWER LINE OVERRIDE                         
*&&                                                                             
*&&US                                                                           
FACPAK   DC    CL8'FACAPY0'        MODULE NAME FACPAK                           
FASTART  DC    CL8'FASTART'        MODULE NAME FASTART                          
FACPAKP  DC    CL8' '              MODUE NAME SUBSIDIARY FACPAK                 
FAPARMS  DC    CL8' '              MODULE NAME RUN PARAMETERS                   
FATASKS  DC    XL1'00'             OVERRIDE TASKS IN FAPARMS                    
FAPROT   DC    XL1'00'             FACPAK STORAGE PROTECTION FLAG               
FATOR    DC    CL1'Y'              FACPAK IS A TOR                              
FAAORID  DC    CL1' '              AOR ID                                       
         DC    CL4' '              N/D                                          
MSGLIB   DC    CL8' '              ANSWER LODE PROGRAM LIB                      
MSGRST   DS    CL8' '              ANSWER IS THIS A RESTART                     
MSGDAY   DS    CL8' '              ANSWER DAY VALUE                             
MSGSYSO  DS    CL8' '              ANSWER SYSTEM OVERRIDE                       
MSGLINO  DS    CL8' '              ANSWER LINE OVERRIDE                         
*&&                                                                             
         EJECT                                                                  
         DS    0D                                                               
         DC    CL8'BOOTWORK'                                                    
WORK     DC    50D'0'                                                           
         EJECT                                                                  
***********************************************************************         
* INCLUDED DSECTS                                                     *         
***********************************************************************         
         SPACE 1                                                                
*FAASIDD                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAASIDD                                                        
         PRINT ON                                                               
*FAPIGFACD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAPIGFACD                                                      
         PRINT ON                                                               
*FAUTL                                                                          
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003FABOOTA   12/20/00'                                      
         END                                                                    
