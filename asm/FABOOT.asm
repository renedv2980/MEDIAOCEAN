*          DATA SET FABOOT     AT LEVEL 030 AS OF 06/08/11                      
*PHASE FACPAKA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
         TITLE 'FABOOT - FACILITIES BOOT STRAP'                                 
         PRINT NOGEN                                                            
FABOOT   CSECT                                                                  
         NBASE 0,**FABOOT,WORK=WORK                                             
         ST    R1,BOOTCMRG                                                      
*                                                                               
BOOT02   L     R1,BOOTCMRG         GET MVS PARM LIST FOR UPSI VALUE             
         L     R1,0(,R1)                                                        
         LH    R2,0(,R1)           R2=L'PARM DATA                               
         LTR   R2,R2                                                            
         BZ    BOOT04                                                           
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         BAS   RE,GETUPSI                                                       
*                                                                               
BOOT04   LA    R2,PARM             EXTRACT MVS JOBNAME                          
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,PARM                                                          
         MVC   MVSNAME,0(R2)                                                    
*                                                                               
         EXTRACT ASIDFLD,'S',FIELDS=(ASID)                                      
*                                                                               
*&&UK                                                                           
         TM    BOOTUPSI,X'80'      UPSI 1 FOR CARD DECK INPUT                   
         BZ    BOOT40                                                           
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* READ AND PROCESS INPUT PARAMETERS                                   *         
***********************************************************************         
         LA    R0,L'MVSNAME        FIND LENGTH OF JOBNAME                       
         XR    R4,R4                                                            
         LA    RF,MVSNAME                                                       
*                                                                               
BOOT06   CLI   0(RF),C' '                                                       
         BNH   BOOT08                                                           
         LA    R4,1(R4)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,BOOT06                                                        
*                                                                               
BOOT08   GOTO1 =V(CARDS),PARM,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    BOOTERR4                                                         
*                                                                               
         LA    RF,BOOT20           FIRST TIME BRANCH                            
         TM    BFLAGS,BFSTART      BEEN IN BEFORE?                              
         BZ    *+8                 NO                                           
         LA    RF,BOOT08                                                        
         OI    BFLAGS,BFSTART                                                   
*                                                                               
         CLC   =C'*START',CARD     MULTIPLE FACPAK BOOK START?                  
         BNER  RF                  NO                                           
         OI    BFLAGS,BFMULTI      PARAMETERS FOR MULTIPLE FACPAKS              
*                                                                               
         LA    RF,CARD+L'CARD-1    RF = END OF CARD                             
         LA    R2,CARD+6           GO PAST *START TO FIRST NON-SPACE            
         CLI   0(R2),C' '                                                       
         BH    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         LR    R3,R2               SAVE START OF THIS FACPAK NAME               
*                                                                               
BOOT10   CR    R2,RF               END OF CARD?                                 
         BH    BOOTERR1                                                         
         CLI   0(R2),C' '          END OF FACPAK NAME LIST?                     
         BE    BOOT12                                                           
         CLI   0(R2),C','          END OF LIST ENTRY                            
         BE    BOOT12                                                           
         LA    R2,1(R2)                                                         
         B     BOOT10                                                           
*                                                                               
BOOT12   LR    RE,R2               GET LENGTH OF JOBNAME                        
         SR    RE,R3                                                            
         CR    RE,R4               SAME LENGTH?                                 
         BNE   BOOT14              NO                                           
         BCTR  RE,0                                                             
         EX    RE,*+8              MATCH AGAINST THIS JOBNAME                   
         BE    BOOT16                                                           
         CLC   MVSNAME(0),0(R3)                                                 
*                                                                               
BOOT14   CLI   0(R2),C' '          END OF LIST ENTRIES                          
         BE    BOOT08              YES - NEXT CARD                              
         LA    R2,1(R2)            GO PAST THE C','                             
         LR    R3,R2               SET START OF NEXT FACPAK NAME                
         B     BOOT10                                                           
         EJECT                                                                  
***********************************************************************         
* Scan through cards and set values                                             
***********************************************************************         
BOOT16   DS    0H                                                               
         GOTO1 =V(CARDS),PARM,CARD,=C'RE00'                                     
         CLC   CARD(2),=C'/*'                                                   
         BE    BOOT40                                                           
         TM    BFLAGS,BFMULTI     MULTIPLE FACPAK BOOK?                         
         BZ    BOOT20             NO                                            
*                                                                               
         CLC   =C'*START',CARD    IGNORE ANY OTHER *START CARDS                 
         BE    BOOT16                                                           
         CLC   =C'*END',CARD      C'*END' MEANS WE'RE THROUGH                   
         BE    BOOT40                                                           
*                                                                               
BOOT20   CLI   CARD,C'*'          IGNORE COMMENTS/COMMANDS                      
         BE    BOOT16                                                           
         CLC   CARD(7),=C'FACPAK='                                              
         BE    BOOT22                                                           
         CLC   CARD(8),=C'FASTART='                                             
         BE    BOOT24                                                           
         CLC   CARD(8),=C'FAPARMS='                                             
         BE    BOOT26                                                           
         CLC   CARD(6),=C'TASKS='                                               
         BE    BOOT28                                                           
         CLC   CARD(5),=C'UPSI='                                                
         BE    BOOT30                                                           
         CLC   CARD(8),=C'PROTECT='                                             
         BE    BOOT32                                                           
         CLC   CARD(4),=C'AOR='                                                 
         BE    BOOT34                                                           
         CLC   CARD(4),=C'WLM='                                                 
         BE    BOOT36                                                           
         CLC   CARD(5),=C'LP2=Y'                                                
         BE    BOOT38                                                           
*                                                                               
         CLC   CARD(6),=C'FANET='  NO LONGER USED                               
         BE    BOOT16                                                           
         CLC   CARD(7),=C'FAPHLS=' NO LONGER USED                               
         BE    BOOT16                                                           
         B     BOOTERR1                                                         
*                                                                               
BOOT22   MVC   FACPAK,CARD+7       OVERRIDE FACPAK                              
         B     BOOT16                                                           
*                                                                               
BOOT24   MVC   FASTART,CARD+8      OVERRIDE FASTART                             
         B     BOOT16                                                           
*                                                                               
BOOT26   MVC   FAPARMS,CARD+8      OVERRIDE FAPARMS                             
         B     BOOT16                                                           
*                                                                               
BOOT28   MVC   PARM(2),CARD+6      OVERRIDE NUMBER OF TASKS                     
         CLI   PARM+1,C' '                                                      
         BNE   *+14                                                             
         MVC   PARM+1(1),PARM                                                   
         MVI   PARM,C'0'                                                        
         CLC   PARM(2),=C'00'      ZERO MEANS AS DEFINED IN FAPARMS             
         BL    BOOTERR1                                                         
         CLC   PARM(2),=C'35'      MAXIMUM NUM OF TASKS                         
         BH    BOOTERR1                                                         
         PACK  PARM+8(8),PARM(2)                                                
         CVB   RF,PARM+8                                                        
         STC   RF,FATASKS                                                       
         B     BOOT16                                                           
*                                                                               
BOOT30   MVI   BOOTUPSI,0          OVERRIDE UPSI VALUE                          
         LA    R1,CARD+5                                                        
         LA    R2,8                                                             
         BAS   RE,GETUPSI                                                       
         B     BOOT16                                                           
*                                                                               
BOOT32   CLI   CARD+8,C'Y'         SET FACPAK STORAGE PROTECTION ON             
         BNE   BOOT16                                                           
         MVI   FAPROT,C'Y'                                                      
         B     BOOT16                                                           
*                                                                               
BOOT34   MVI   FATOR,C'N'                                                       
         MVC   FAAORID,CARD+4                                                   
         B     BOOT16                                                           
*                                                                               
BOOT36   CLI   CARD+4,C'Y'         SET FACPAK CONNECTED TO WLM                  
         BNE   BOOT16                                                           
         MVI   FAWLM,C'Y'                                                       
         B     BOOT16                                                           
*                                                                               
BOOT38   MVI   FALP2,C'Y'                                                       
         B     BOOT16                                                           
         EJECT                                                                  
***********************************************************************         
* FIND LOW/HIGH CORE AVAILABLE FOR FACPAK                             *         
***********************************************************************         
BOOT40   DS    0H                                                               
*&&US                                                                           
         OPEN  (ASIDHELP,OUTPUT)                                                
         LTR   RF,RF                                                            
         BNZ   BOOT41                                                           
*                                                                               
         XC    CARD,CARD                                                        
         MVC   CARD+00(8),MVSNAME                                               
         MVC   CARD+08(2),ASIDFLD+2                                             
         MVC   CARD+10(1),FATOR                                                 
         PUT   ASIDHELP,CARD                                                    
         CLOSE ASIDHELP                                                         
*&&                                                                             
*                                                                               
BOOT41   CLI   FAPROT,C'Y'         TEST IF FACPAK STORAGE PROTECTION            
         BE    BOOT42                                                           
*                                                                               
         GETMAIN VC,LA=COREMIN,A=COREADR,BNDRY=PAGE                             
         B     BOOT44                                                           
*                                                                               
BOOT42   GETMAIN VC,LA=COREMIN,A=COREADR,BNDRY=PAGE,SP=132                      
*                                                                               
BOOT44   LTR   RF,RF               GETMAIN ERROR?                               
         BNZ   BOOTERR2                                                         
*                                                                               
         CLI   FAPROT,C'Y'         TEST IF FACPAK STORAGE PROTECTION            
         BE    BOOT46                                                           
*                                                                               
         FREEMAIN VC,A=COREADR     GET AND FREE MAX CORE AVAILABLE              
         B     BOOT48                                                           
*                                                                               
BOOT46   FREEMAIN VC,A=COREADR,SP=132  GET AND FREE MAX CORE AVAILABLE          
*                                                                               
BOOT48   LTR   RF,RF                                                            
         BNZ   BOOTERR2                                                         
*                                                                               
         L     R1,CORELEN          GET AMOUNT OF CORE ACQUIRED                  
         S     R1,CORESPR          MINUS SPARE CORE REQUIRED                    
         BNP   BOOTERR2                                                         
*                                                                               
         LA    R1,2047(R1)         SET MODULUS 2K                               
         SRL   R1,11                                                            
         SLL   R1,11                                                            
         ST    R1,COREMAX          SET NEW MAX CORE REQUIRED                    
         XC    COREMIN,COREMIN                                                  
         CLI   FAPROT,C'Y'         TEST IF FACPAK STORAGE PROTECTION            
         BE    BOOT50                                                           
*                                                                               
         GETMAIN VC,LA=COREMIN,A=COREADR,BNDRY=PAGE                             
         B     BOOT52                                                           
*                                                                               
BOOT50   GETMAIN VC,LA=COREMIN,A=COREADR,BNDRY=PAGE,SP=132                      
*                                                                               
BOOT52   LTR   RF,RF                                                            
         BNZ   BOOTERR2                                                         
         CLC   CORELEN,COREMAX     CHECK THAT WE GOT WHAT WE WANTED             
         BNE   BOOTERR2                                                         
         L     RF,COREADR                                                       
         ST    RF,BOOTLOCO         SAVE LOW ADDR OF CORE ACQUIRED               
         A     RF,CORELEN          ADD LENGTH OF CORE ACQUIRED                  
         BCTR  RF,0                                                             
         ST    RF,BOOTHICX         SAVE HIGH ADDR OF CORE ACQUIRED              
         B     BOOT60                                                           
*                                                                               
COREADR  DC    F'0'                ADR CORE ACQUIRED                            
CORELEN  DC    F'0'                LEN CORE ACQUIRED                            
COREMIN  DC    F'00400000'         MIN CORE REQUIRED                            
COREMAX  DC    F'16000000'         MAX CORE REQUIRED                            
CORESPR  DC    F'00200000'         SPR CORE REQUIRED                            
         EJECT                                                                  
***********************************************************************         
* Load FACPAKx and start to run                                                 
***********************************************************************         
BOOT60   L     R0,BOOTLOCO                                                      
         GOTO1 =V(LOADER),PARM,FACPAK,(R0),(C'A',(R0))                          
         L     RF,4(R1)                                                         
         LTR   RF,RF               GET LOAD ADDRESS OF FACPAK MODULE            
         BZ    BOOTERR3                                                         
         ST    RF,BOOTLOCO                                                      
         L     RE,0(R1)            GET LENGTH OF FACPAK MODULE                  
         AR    RE,RF                                                            
         BCTR  RE,0                                                             
         ST    RE,BOOTHICO         SET HIGH CORE OF FACPAK MODULE               
         CLI   FAPROT,C'Y'         TEST IF FACPAK STORAGE PROTECTION            
         BE    BOOTPROT                                                         
         LA    R1,BOOTPARM                                                      
         BASR  RE,RF               PASS CONTROL TO FACPAK MODULE                
         B     BOOTEOJ                                                          
***********************************************************************         
* BOOT WITH PROTECTION KEYS                                                     
***********************************************************************         
BOOTPROT BAS   RE,SETPCS           SET UP PC SERVICE ENTRY TABLE                
*                                                                               
         LA    R1,BOOTPARM                                                      
         L     RE,SERV1PC                                                       
         PC    0(RE)                                                            
*        BASR  RE,RF               PASS CONTROL TO FACPAK MODULE                
         BAS   RE,CLRPCS           CLEAR PC SERVICE ENTRY TABLE                 
         B     BOOTEOJ                                                          
                                                                                
BOOTERR1 LA    R0,ERROR1           INVALID PARAMETER CARD                       
         B     BOOTERRX                                                         
                                                                                
BOOTERR2 LA    R0,ERROR2           NOT ENOUGH CORE AVAILABLE                    
         B     BOOTERRX                                                         
                                                                                
BOOTERR3 LA    R0,ERROR3           FACPAK PHASE NOT FOUND                       
         B     BOOTERRX                                                         
                                                                                
BOOTERR4 LA    R0,ERROR4           NO *START CARD FOR THIS FACPAK               
         B     BOOTERRX                                                         
                                                                                
BOOTERRX GOTO1 =V(LOGIO),PARM,1,(40,(R0))                                       
*                                                                               
BOOTEOJ  XBASE                                                                  
***********************************************************************         
* SET UPSI VALUES                                                               
***********************************************************************         
GETUPSI  LA    RF,GETUPSIT                                                      
         CLI   0(R1),C'1'                                                       
         BNE   *+10                                                             
         OC    BOOTUPSI(1),0(RF)                                                
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R2,GETUPSI+4                                                     
         BR    RE                                                               
GETUPSIT DC    X'8040201008040201'                                              
         EJECT                                                                  
***********************************************************************         
* SET UP PC SERVICE LINKAGE AND ENTRY TABLES                                    
***********************************************************************         
         DS    0D                                                               
SETPCS   NTR1                                                                   
*                                                                               
         MODESET KEY=NZERO,MODE=SUP                                             
*                                  INITIALISE LINKAGE TABLE                     
         LA    R2,1                                                             
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
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* CLEAR PC SERVICE LINKAGE AND ENTRY TABLES                                     
***********************************************************************         
         DS    0D                                                               
CLRPCS   NTR1                                                                   
*                                                                               
         MODESET KEY=NZERO,MODE=SUP                                             
*                                  DISCONNECT ENTRY/LINKAGE TABLE               
DISET1   ETDIS TKLIST=TKL,RELATED=CET1                                          
*                                  DESTROY ENTRY TABLE                          
DESET1   ETDES TOKEN=TKVALUE,RELATED=CET1                                       
*                                  FREE LX FOR REUSE                            
FREELX   LXFRE LXLIST=LXL,RELATED=GETLX                                         
*                                                                               
         SR    R2,R2               ZERO VALUE                                   
RESETAX  AXSET AX=(2)              RESET AX TO ZERO                             
*                                                                               
         MODESET KEY=NZERO,MODE=PROB                                            
*                                                                               
         XIT1                                                                   
         DS    0D                                                               
ETDESC   ETDEF TYPE=INITIAL                                                     
ETD1     ETDEF TYPE=ENTRY,ROUTINE=0,SSWITCH=NO,RAMODE=24,              X        
               STATE=PROBLEM,AKM=(8,9),EKM=(9)                                  
         ETDEF TYPE=FINAL                                                       
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND VARIABLES                                                       
***********************************************************************         
ASIDFLD  DC    D'0'                                                             
*                                                                               
ASIDHELP DCB   DDNAME=ASIDHELP,MACRF=(PM),RECFM=FB,LRECL=80,           +        
               BLKSIZE=400,DSORG=PS                                             
*                                                                               
         DS    0D                                                               
BOOTPARM DS    0F                  PARAM LIST TO FACPAK PHASE                   
*                                                                               
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
FAWLM    DC    CL1'N'              WLM ACTIVE                                   
FALP2    DC    CL1'N'              LP2 FLAG                                     
         DC    CL2' '              N/D                                          
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
FAWLM    DC    CL1'N'              WLM ACTIVE                                   
FALP2    DC    CL1'N'              LP2 FLAG                                     
         DC    CL2' '              N/D                                          
MSGLIB   DC    CL8' '              ANSWER LODE PROGRAM LIB                      
MSGRST   DS    CL8' '              ANSWER IS THIS A RESTART                     
MSGDAY   DS    CL8' '              ANSWER DAY VALUE                             
MSGSYSO  DS    CL8' '              ANSWER SYSTEM OVERRIDE                       
MSGLINO  DS    CL8' '              ANSWER LINE OVERRIDE                         
*&&                                                                             
         EJECT                                                                  
ERROR1   DC    CL40'*FACPAK* INVALID PARAMETER CARD'                            
ERROR2   DC    CL40'*FACPAK* NOT ENOUGH CORE AVAILABLE'                         
ERROR3   DC    CL40'*FACPAK* FACPAK PHASE NOT FOUND'                            
ERROR4   DC    CL40'*FACPAK* NO *START CARD FOR JOBNAME'                        
*                                                                               
BFLAGS   DS    XL1                 INTERNAL FLAGS                               
BFMULTI  EQU   X'80'               PARAMETERS ARE FOR MULTIPLE FACPAKS          
BFSTART  EQU   X'40'               STARTED READING CARDS                        
MVSNAME  DS    CL8                                                              
PARM     DS    2D                                                               
CARD     DS    CL80                                                             
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
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'BOOTWORK'                                                    
WORK     DC    50D'0'                                                           
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030FABOOT    06/08/11'                                      
         END                                                                    
