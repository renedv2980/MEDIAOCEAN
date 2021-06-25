*          DATA SET SPSFM06    AT LEVEL 052 AS OF 12/09/19                      
*PHASE T21706A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21706  -- CLIENT MAINTENANCE                        *         
*                T21707  -- CLIENT LIST                               *         
*                                                                     *         
*  COMMENTS:     MAINTAINS CLIENTS RECORDS                            *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM76 (MAINT) & SCSFM77 (LIST)              *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-40080  12/02/19 SUPPORT NEW BUYING AGENCY IDENTIFIER      *         
***********************************************************************         
         TITLE 'T21706 - CLIENT MAINTENANCE'                                    
T21706   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1706**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         USING OFFICED,OFCBLK                                                   
*                                                                               
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
XIT      XIT1                                                                   
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
*                                                                               
         OI    CLTMEDH+6,X'80'     TRANSMIT ALL KEY FIELDS                      
         OI    CLTCLTH+6,X'80'     TO ENFORCE ALL CAPS DISPLAY                  
*                                                                               
         LA    R2,CLTMEDH          MEDIA                                        
         MVC   SVMED,8(R2)         SAVE MEDIA IN CASE OF '*' OR 'L'             
         CLI   ACTNUM,ACTADD       ADDING NEW CLIENT RECORD/S?                  
         BNE   VK00                NOPE, MUST VALIDATE MEDIA                    
         CLI   8(R2),C'*'          ADD MEDIAS T,R,AND X?                        
         BE    *+12                YES                                          
         CLI   8(R2),C'L'          JUST MEDIA T AND R?                          
         BNE   VK00                NO                                           
         MVI   8(R2),C'T'          WE ALWAYS ADD MEDIA T                        
*                                                                               
VK00     GOTO1 VALIMED                                                          
         MVC   CLTMEDN(L'MEDNM),MEDNM                                           
         OI    CLTMEDNH+6,X'80'                                                 
*                                                                               
*        CLI   ACTNUM,ACTDIS                                                    
*        BE    VK05                                                             
*        CLI   SVAPROF+7,C'C'       IF CANANDIAN AGENCY                         
*        BNE   VK05                                                             
*        CLI   QMED,C'C'           MEDIA C AND N ONLY FOR DISPLAY               
*        BE    ERRINV                                                           
*        CLI   QMED,C'N'                                                        
*        BE    ERRINV                                                           
*                                                                               
VK05     L     R6,AIO                                                           
         USING AGYHDRD,R6          SAVING SOME IMPORTANT AGENCY INFO            
*                                                                               
         CLI   ACTNUM,ACTADD       ADDING NEW CLIENT RECORD/S?                  
         BNE   VK06                                                             
         CLI   AGYPAHEX,X'00'      MAKE SURE AGY HEX IS NOT ZERO                
         BE    SETUPERX            IF YES -- DISALLOW                           
         MVC   BYTE,BAGYMD         ALSO THAT A/M HOB IS NOT ZERO                
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'00'                                                       
         BE    SETUPERX            IF YES -- DISALLOW                           
*                                                                               
VK06     MVC   FLAG1,AGYFLAG1                                                   
         MVC   ACCOFC,AGYOFC2                                                   
         MVC   SVCTAGY,AGYCTAGY                                                 
*                                                                               
         MVI   ELCODE,X'03'        ACC AGY LIST ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ACCAGY,2(R6)        SAVE ACC AGENCY LIST                         
         DROP  R6                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       CREATING A NEW CLIENT                        
         BE    VK10                SKIP VALICLT                                 
*                                                                               
         LA    R2,CLTCLTH          CLIENT                                       
         GOTO1 VALICLT                                                          
         B     VK40                SKIP MANUAL CHECK                            
*                                                                               
VK10     DS    0H                                                               
         LA    R2,CLTCLTH          CLIENT                                       
         GOTO1 ANY                 REQUIRED                                     
*                                                                               
*** MAKING SURE CLIENT CODE DOESN'T BEGIN WITH *,$,+                            
         CLI   WORK,C'*'                                                        
         BE    ERRCLI                                                           
         CLI   WORK,C'$'                                                        
         BE    ERRCLI                                                           
         CLI   WORK,C'+'                                                        
         BE    ERRCLI                                                           
***                                                                             
         MVC   QCLT(3),WORK                                                     
         CLI   5(R2),3             2 <= LENGTH <= 3                             
         BH    ERRCLI                                                           
         CLI   5(R2),2                                                          
         BL    ERRCLI                                                           
*                                                                               
         CLI   5(R2),3             CERTAIN CASES CANNOT BE USED                 
         BE    VK10A               AS CLIENTS.                                  
         CLI   5(R2),2                                                          
         BE    VK10B                                                            
*                                                                               
VK10A    CLC   8(3,R2),=C'ALL'                                                  
         BE    ERRCLI                                                           
         CLC   8(3,R2),=C'DDS'                                                  
         BE    ERRCLI                                                           
         B     VK20                                                             
*                                                                               
VK10B    DS    0H                                                               
         CLI   ACTNUM,ACTADD       CLIENTS BELOW CANT BE ADDED                  
         BNE   VK20                                                             
*                                                                               
         CLC   8(2,R2),=C'CL'                                                   
         BE    ERRCLI                                                           
         CLC   8(2,R2),=C'PG'                                                   
         BE    ERRCLI                                                           
         CLC   8(2,R2),=C'BM'                                                   
         BE    ERRCLI                                                           
         CLC   8(2,R2),=C'NO'                                                   
         BE    ERRCLI                                                           
*                                                                               
VK20     GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   ERRCLI                                                           
*                                                                               
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
*                                                                               
         OC    T217FFD+6(2),T217FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VK40                                                             
         CLI   T217FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    VK30                YES                                          
         CLI   T217FFD+6,C'+'      TEST MKT LOCKOUT                             
         BE    VK30                YES                                          
         CLI   T217FFD+6,C'$'      TEST OFFICE LIST                             
         BE    VK30                YES                                          
*                                                                               
VK25     CLC   T217FFD+6(2),BCLT                                                
         BNE   ERRSEC                                                           
VK30     CLI   T217FFD+6,C'$'                                                   
         BE    VK40                                                             
         CLI   T217FFD+6,C'*'                                                   
         BNE   ERRSEC                                                           
         B     VK40                                                             
*                                                                               
VK40     CLC   SVCTAGY,=C'  '       DOING CODE COORDINATION?                    
         BNH   VK50                                                             
         BRAS  RE,CHKCTCLT                                                      
*                                                                               
VK50     XC    KEY,KEY              BUILD THE KEY                               
         LA    R6,KEY                                                           
         USING CLTRECD,R6                                                       
         MVI   CKEYTYPE,X'00'                                                   
         MVC   CKEYAM,BAGYMD        AGENCY/MEDIA CODE                           
         MVC   CKEYCLT,BCLT         BINARY CLIENT CODE                          
*                                                                               
VKX      MVC   SVCLTKEY(L'KEY),KEY  BACK UP THE KEY                             
         MVC   CLTLASTC,CLTCLT+2                                                
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0X                                                               
* call init. routine for cost2 conv. date                                       
         CLI   SVAPROF+7,C'C'       IF CANANDIAN AGENCY                         
         BNE   VR01                                                             
         CLI   QMED,C'C'           MEDIA C AND N ONLY FOR DISPLAY               
         BE    ERRINV                                                           
         CLI   QMED,C'N'                                                        
         BE    ERRINV                                                           
VR01     BRAS  RE,VALCOS2                                                       
*                                                                               
         OI    GENSTAT2,RETEQSEL                                                
*                                                                               
         MVC   SVCLTKEY(L'KEY),KEY BACKING UP THE KEY                           
         MVI   QXIT,0              CLEAR THE QUICK EXIT FLAG.                   
*                                                                               
         LA    R4,TABLES           TABLE OF VR AND DR ROUTINE ADDRESS           
         LA    R4,OPTSTART(R4)                                                  
         USING OPTTABD,R4                                                       
VR10     LH    R2,SCRDISD          POINT TO THE DISPLACEMENT                    
         CH    R2,=X'FFFF'         SEE IF AT END OF TABLE                       
         BE    VR20                                                             
         AR    R2,RA               POINT R2 TO THE HEADER OF ENTRY              
         LH    R3,RECDISD                                                       
         A     R3,AIO              POINT R3 TO THE TARGET IN AIO                
         L     RF,AVALD            VALIDATE FIELD ON SCREEN                     
         A     RF,RELO                                                          
         BASR  RE,RF               CALL THE VALIDATION ROUTINE                  
         CLI   QXIT,1              HAS THE QUICK EXIT FLAG BEEN SET?            
*                                  QXIT IS SET AT THE DELETE SUBROUTINE         
         BE    DR                  YES, SKIP THE REST OF VALIDATION             
         LA    R4,OPTTABQ(R4)      BUMP THE TABLE POINTER                       
         B     VR10                                                             
*                                                                               
VR20     BRAS  RE,CROSSCHK         CHECK THE INPUTS FOR ALL CO-RELATION         
*                                                                               
VRX      CLI   ACTNUM,ACTADD       ADDING A RECORD?                             
         BNE   VRX05               NO                                           
         BRAS  RE,INITCL2          INIT SOME CL2 FIELDS                         
         BRAS  RE,CHKRX            IF WE NEED TO ADD MED R/X VAL FIRST          
*                                                                               
VRX05    MVC   KEY(L'SVCLTKEY),SVCLTKEY                                         
         MVC   AIO,AIO1            RESTORE THE KEY                              
         L     R6,AIO                                                           
         CLI   ACTNUM,ACTADD                                                    
         BNE   VRX10                                                            
         MVC   0(13,R6),SVCLTKEY                                                
         MVC   13(2,R6),=H'1280'   PUT IN THE RECORD SIZE FOR CLT REC           
         BAS   RE,ADREC            ADDING A NEW RECORD                          
         MVC   SVCLTKEY,KEY        SAVE THE KEY AND THE DISK ADDRESS            
*                                  FOR CANADIAN CLIENT, RECORD NEED TO          
         BRAS  RE,SPCN             BE ADDED FOR MEDIA C AND N ALSO              
         BRAS  RE,SPRX             MIGHT NEED TO ADD MEDIAS R AND X             
*                                                                               
         B     VRX20               SKIP CHANGE                                  
*                                                                               
VRX10    DS    0H                  CHANGING AN EXISTING RECORD                  
         MVC   AIO,AIO1                                                         
         BRAS  RE,PTREC            STORE THE RECORD                             
*                                  FOR CANADIAN CLIENT, RECORD NEED TO          
         BRAS  RE,SPCN             BE STORED FOR MEDIA C AND N ALSO             
*                                                                               
VRX20    DS    0H                                                               
         BRAS  RE,OFCPTR           IF OFFICE NUMBER HAS CHANGED, OFFICE         
*                                  PASSIVE POINTED HAS TO BE RESET              
***********************************************************************         
*  ADDING REQUEST RECORD FOR TRANSACTION                              *         
***********************************************************************         
REQREC   MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         XC    0(150,R6),0(R6)                                                  
         MVI   10(R6),41                                                        
         MVI   14(R6),106                                                       
         LA    R6,26(R6)                                                        
         MVI   0(R6),X'40'                                                      
         MVC   1(79,R6),0(R6)                                                   
         MVC   0(2,R6),=C'41'                                                   
         MVC   2(2,R6),14(RA)                                                   
***      MVC   4(1,R6),CLTMED                                                   
***      MVC   5(3,R6),CLTCLT                                                   
         MVC   4(1,R6),QMED                                                     
         MVC   5(3,R6),QCLT                                                     
         OC    5(3,R6),SPACES                                                   
         MVC   68(7,R6),=C'CONTROL'                                             
         MVI   61(R6),C'C'                                                      
         MVI   63(R6),C'A'                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    *+8                                                              
         MVI   63(R6),C'C'                                                      
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',0(R6),0(R6)                   
*                                                                               
         MVC   AIO,AIO1                                                         
         B     DR                   REDISPLAY CHANGED RECORD                    
         EJECT                                                                  
         SPACE 2                                                                
***********************************************************************         
VALNAME  NTR1                       *VALIDATE CLIENT NAME*                      
         L     R6,AIO               R6 POINTS TO THE ORIGINAL RECORD            
         USING CLTRECD,R6                                                       
         MVC   AIO,AIO2             SET IO FOR THIS SUBROUTINE TO BE 2          
         GOTO1 ANY                  R2 IS POINTING AT FIELD                     
         MVC   CNAME,WORK                                                       
*                                                                               
         CLC   =C'DELETE',CNAME     USER TRYING TO DELETE?                      
         BNE   NAMEX                                                            
*                                                                               
         CLI   ACTNUM,ACTADD        CAN'T DELETE ON ADD                         
         BE    ERRDADD                                                          
*                                                                               
         OC    CLIST(100),CLIST     CANT DELETE WITH PRODS LEFT                 
         BNZ   ERRPRDX                                                          
*                                                                               
         CLI   SVAPROF+7,C'C'       IF CANANDIAN AGENCY                         
         BNE   NAME10                                                           
         CLI   CLTMED,C'T'          AND MEDIA T                                 
         BNE   NAME10                                                           
         BRAS  RE,CKCANDN           THEN CHECK ALSO MEDIA C AND N               
         BNE   ERRPRDX              PRODUCT STILL EXIST                         
*                                                                               
NAME10   BRAS  RE,CK4GRP            CHECK FOR CLIENT EXISTANCE IN GRP           
         MVC   HALF2(1),SVCLTKEY+1                                              
         BRAS  RE,DODEF                                                         
         CLI   SVAPROF+7,C'C'                                                   
         BNE   NAME20                                                           
         CLI   CLTMED,C'T'                                                      
         BNE   NAME20                                                           
         MVC   HALF2(1),HALF                                                    
         CLI   HALF2,0                                                          
         BE    *+12                                                             
         BRAS  RE,DODEF                                                         
         BRAS  RE,MRKCLT                                                        
*                                                                               
         MVC   HALF2(1),HALF+1                                                  
         CLI   HALF2,0                                                          
         BE    *+12                                                             
         BRAS  RE,DODEF                                                         
         BRAS  RE,MRKCLT                                                        
*                                                                               
         BRAS  RE,DELNET                                                        
*                                                                               
NAME20   DS    0H                                                               
         MVC   AIO,AIO1            RESTORE THE AIO                              
         MVC   KEY,SVCLTKEY                                                     
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVI   KEY+13,X'DD'                                                     
         GOTO1 WRITE                                                            
         L     RE,AIO              THE ORIGINAL CLIENT RECORD                   
         MVI   15(RE),X'C0'        REC+15                                       
         BRAS  RE,PTREC                                                         
         MVC   CNAME(20),=C'** CLIENT DELETED **'                               
         MVI   QXIT,1              QUICK EXIT AFTER DELETING A RECORD           
NAMEX    MVC   AIO,AIO1            RESET IO AREAS BACK TO DEFAULT               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
VALOFF   NTR1                       *VALIDATE OFFICE NUMBER*                    
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         MVI   SVCOFFC,0                                                        
         CLI   ACTNUM,ACTADD                                                    
         BE    *+10                                                             
         MVC   SVCOFFC,COFFICE      SAVE OLD OFFICE NUMBER                      
         MVI   COFFICE,0                                                        
         CLI   SVAPROF+13,C'Y'      IS OFFICE NUMBER REQUIRED?                  
         BE    OFF10                                                            
         CLI   5(R2),0                                                          
         BE    OFFX                                                             
*                                                                               
OFF10    GOTO1 ANY                                                              
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T217FFD+6                                                
         MVC   OFCLMT,T217FFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,WORK                                                     
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         MVC   COFFICE(1),OFCOFC   SAVE OFF 1 BYTE INTERNAL OFFICE CODE         
         TM    OFCINDS,OFCINOLA    USING 2 CHAR OFFICES?                        
         BNZ   OFF12                NO                                          
         TM    OFCINDS,OFCIOINV    INVALID OFFICE?                              
         BNZ   ERROFC               YES                                         
         CLI   0(R1),0                                                          
         BNE   ERRINV                                                           
         B     OFFX                                                             
*                                                                               
OFF12    TM    FLAG1,X'10'          OFF=HEX OPTION IN USE?                      
         BO    OFF20                YES                                         
         CLI   8(R2),C'A'                                                       
         BL    ERRINV                                                           
OFF20    CLI   WORK,C'='            INVALID OFFICE NUMBERS                      
         BE    ERRINV                                                           
         CLI   WORK,C','                                                        
         BE    ERRINV                                                           
         CLI   WORK,C'-'                                                        
         BE    ERRINV                                                           
         MVC   COFFICE,WORK                                                     
*                                                                               
OFFX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
VALTOFF  NTR1                  *VALIDATE MEDIA, TRAFFIC OFFICE NUMBER*          
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         MVI   CTRAFOFC,0                                                       
         CLI   5(R2),0                                                          
         BE    TOFFX                                                            
TOFF10   GOTO1 ANY                                                              
         TM    FLAG1,X'10'          OFF=HEX OPTION IN USE?                      
         BO    TOFF20                YES                                        
         CLI   8(R2),C'A'                                                       
         BL    ERRINV                                                           
TOFF20   CLI   WORK,C'='            INVALID OFFICE NUMBERS                      
         BE    ERRINV                                                           
         CLI   WORK,C','                                                        
         BE    ERRINV                                                           
         CLI   WORK,C'-'                                                        
         BE    ERRINV                                                           
         MVC   CTRAFOFC,WORK                                                    
TOFFX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
VALBAID  NTR1                                                                   
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    *+12                ALWAYS VALIDATE ON ADD                       
         TM    4(R2),X'20'         HAS FIELD CHANGED                            
         BO    VBAIDX              NO, THEN EXIT                                
*                                                                               
         L     R6,AIO              R6 = A(CLIENT RECORD)                        
         USING CLTRECD,R6          CLIENT RECORD DSECT                          
         XC    CBUYAGIS,CBUYAGIS   CLEAR BUYING AGY ID                          
         DROP  R6                  DROP CLIENT RECORD USING                     
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VBAIDX              NO - DONE                                    
         CLI   5(R2),3             INPUT LENGTH OF 3?                           
         BNE   ERRAGID1            NO - ERROR                                   
*                                                                               
         XC    KEY1,KEY1           CLEAR CONTROL FILE KEY                       
         LA    R6,KEY1             R6 = WORK                                    
         USING BAGRECD,R6          BUYING AGENCY RECORD DSECT                   
         MVI   BAGKMIN,BAGKMINQ    MINOR SYSTEM 'T' (FOR TRAFFIC)               
         MVI   BAGKTYP,BAGKTYPQ    BUYING AGENCY RECORD                         
         MVC   BAGKAGY,AGENCY      AGENCY ALPHA                                 
         MVC   BAGKBAGY,8(R2)      BUYING AGENCY IDENTIFIER                     
         MVC   KEY2,KEY1           SAVE OFF KEY                                 
         DROP  R6                  DROP BUYING AGENCY RECORD USING              
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY1,KEY1                 
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - NO ERRORS TOLERATED                    
*                                                                               
         CLC   KEY2(32),KEY1       KEY = KEYSAVE?                               
         BNE   ERRAGID2            NO - THIS DOESN'T EXIST - ERROR              
*                                                                               
         L     R6,AIO              R6 = A(CLIENT RECORD)                        
         USING CLTRECD,R6          CLIENT RECORD DSECT                          
         MVC   CBUYAGIS,8(R2)      MOVE BUYING AGY ID TO CLIENT RECORD          
         DROP  R6                  DROP CLIENT RECORD USING                     
*                                                                               
VBAIDX   B     XIT                 DONE                                         
***********************************************************************         
VALACC   NTR1                      *VALIDATE AGENCY CODE*                       
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    *+12                ALWAYS VALIDATE ON ADD                       
         TM    4(R2),X'20'         HAS FIELD CHANGED                            
         BO    VAXX                NO, THEN EXIT                                
*                                                                               
         L     RF,ACOMFACS         GET SYS NUM TO SWITCH BACK                   
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   SYSSW,FASYS                                                      
         DROP  R1                                                               
*                                                                               
         XC    POWCODE,POWCODE                                                  
         XC    ACCOFF,ACCOFF                                                    
         MVI   COMPCD,0                                                         
*                                                                               
         L     R4,AIO                                                           
         USING CLTRECD,R4                                                       
         LA    R3,CLTAOFC          WHAT DID THEY ENTER                          
         SR    R1,R1              RESET INPUT LENGTH COUNTER                    
         CLI   CLTAOFCH+5,0                                                     
         BE    VA10                                                             
         LA    R1,1                                                             
         LA    R3,1(R3)                                                         
         CLI   CLTAOFCH+5,1                                                     
         BE    VA10                                                             
         LA    R1,2                                                             
         LA    R3,2(R3)                                                         
         CLI   CLTAOFCH+5,2                                                     
         BE    VA10                                                             
*                                                                               
         LA    R3,CLTAOFC          WHAT DID THEY ENTER                          
         LA    RE,24                                                            
         LA    R1,0                                                             
VAL      CLI   0(R3),C','                                                       
         BE    VA10                                                             
         CLI   0(R3),C'/'                                                       
         BE    VA10                                                             
         CLI   0(R3),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,VAL                                                           
*                                                                               
VA10     STC   R1,OFFLEN           SAVE OFFICE LENGTH                           
         MVC   ACCOFF(2),CLTAOFC                                                
         CLI   OFFLEN,1                                                         
         BNE   *+8                                                              
         MVI   ACCOFF+1,C' '                                                    
         CLI   1(R3),C' '          IS THERE AN OVERRIDE AGENCY                  
         BNH   *+10                                                             
         MVC   POWCODE,1(R3)       YES - SAVE POWER CODE                        
*                                                                               
         CLI   OFFLEN,2                                                         
         BH    ERROFC              NO OFFICE LENGTH > 2                         
         CLI   OFFLEN,0                                                         
         BNE   VA20                                                             
         CLI   ACCOFC,C'Y'         NOTHING ENTERED - 2 REQD                     
         BE    ERR2OFC                                                          
         XC    CACCAGY,CACCAGY     CLEAR THE POWER CODE                         
         MVC   CACCOFC,COFFICE     DEFAULT TO SPOT OFFICE                       
         MVI   CACCOFC+1,C' '                                                   
         B     VAX                                                              
*                                                                               
VA20     CLI   OFFLEN,2            IF THEY ENTERED 2 CHAR OFF                   
         BNE   VA25                                                             
         CLI   ACCOFC,C'Y'         BUT 2 CHAR NOT REQUIRED                      
         BNE   ERR1OFC             =ERROR                                       
*                                                                               
VA25     CLI   OFFLEN,1            IF THEY ENTERED 1 CHAR OFF                   
         BNE   VA30                                                             
         CLI   ACCOFC,C'Y'         BUT 2 CHAR REQUIRED                          
         BE    ERR2OFC                                                          
*                                                                               
VA30     DS    0H                                                               
         OC    POWCODE,POWCODE     OVERRIDE AGY?                                
         BNZ   VA40                                                             
         CLI   ACCOFC,C'Y'         2 CHAR REQUIRED                              
         BNE   VA90                IF NOT THEN SKIP SWITCH                      
         L     RF,ACOMFACS         SWITCH TO ACC SYSTEM                         
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'ACC',0                                              
         B     VA70                                                             
*                                                                               
VA40     DS    0H                  VALIDATE AGAINST AGY HDR CODE LIST           
         LA    RE,ACCAGY                                                        
         LA    R1,8                                                             
VA40L    CLC   0(2,RE),POWCODE     MATCH?                                       
         BE    VA50                                                             
         CLI   0(RE),C' '                                                       
         BNH   ERRAAGY                                                          
         LA    RE,2(RE)                                                         
         BCT   R1,VA40L                                                         
         B     ERRAAGY                                                          
*                                                                               
VA50     MVC   DATADISP,=H'28'     FIND SE NUMBER FOR SPECIFIED                 
         XC    CTKEY,CTKEY         ACC AGY CODE                                 
         LA    R6,CTKEY                                                         
         USING CT5REC,R6                                                        
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE '5'                              
         MVC   CT5KALPH,POWCODE                                                 
         L     R5,AIO2                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',CTKEY,(R5)                
         CLI   8(R1),0             ERRORS?                                      
         BNE   ERRAAGY                                                          
         DROP  R6                                                               
*                                                                               
         L     R6,AIO2                                                          
         USING CTSYSD,R6                                                        
         MVI   ELCODE,X'21'        GET SE NUM FOR ACC FILE                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VA60NX   BAS   RE,NEXTEL                                                        
         BNE   ERRAAGY             ERROR IF NOT FOUND                           
         CLI   CTSYSEL,X'21'       STILL X'21' EL                               
         BNE   ERRAAGY             ERROR IF NOT FOUND                           
         CLI   CTSYSNUM,X'06'      ACC??                                        
         BNE   VA60NX                                                           
*                                                                               
         MVC   COMPCD(1),CTSYSAGB  AGY BINARY CD                                
         XC    DMCB(8),DMCB        YES                                          
         MVC   DMCB(1),CTSYSSE     SE NUM                                       
         L     RF,ACOMFACS         SWITCH TO THAT ACC SYSTEM                    
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
*                                                                               
VA70     CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    ERRSYS                                                           
         CLI   4(R1),1             ANY OTHER ERRORS?                            
         BE    ERRSWS                                                           
         CLI   COMPCD,0              TEST SAVED SF CODE BEFORE                  
         BNE   *+10                  YES - BETTER NOT DO IT AGAIN !             
         MVC   COMPCD,0(R1)          SAVE RETURNED AGENCY BINARY CODE           
*                                                                               
         MVC   MYACCKEY,SPACES       READ ACC COMPANY REC                       
         MVC   MYACCKEY(1),COMPCD                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYACCKEY,(R6)                
         CLI   8(R1),0                                                          
         BNE   ERRCMP                                                           
         AH    R6,=Y(AC$ACCORFST)  FIRST ELEM (IN OLD FILE FORMAT)              
VA80     CLI   0(R6),AC$CPYELQ     X'10' COMPANY ELEM                           
         BE    VA85                                                             
         LLC   R0,0(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   VA80                                                             
         B     ERRCMP                                                           
         USING AC$CPYELD,R6                                                     
VA85     TM    AC$CPYSTAT4,AC$CPYSOFF2 2 CHAR REQ'D                             
         BO    VA88                YES = VALIDATE OFFICE                        
         CLI   OFFLEN,1            MUST BE ONE                                  
         BNE   ERR1OFC                                                          
         B     VA90                OK                                           
         DROP  R6                                                               
*                                                                               
         USING AC$OFFRECD,R6                                                    
VA88     CLI   OFFLEN,2            MUST BE TWO                                  
         BNE   ERR2OFC                                                          
         LA    R6,MYACCKEY         NEW OFFICE -- LOOK FOR OFFICE REC            
         MVC   MYACCKEY,SPACES                                                  
         MVI   AC$OFFKTYP,AC$OFFKTYPQ X'01'                                     
         MVC   AC$OFFKCPY,COMPCD                                                
         MVC   AC$OFFKOFF(2),ACCOFF                                             
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',MYACCKEY,(R6)                
         CLI   8(R1),0                                                          
         BNE   ERROFC                                                           
         TM    AC$OFFRSTAT,AC$OFFSLIST   OFFICE LIST?                           
         BO    ERROFC                                                           
*                                                                               
VA90     DS    0H                  OFFICE CODE IS GOOD                          
         MVC   CACCOFC,ACCOFF      SAVE OFFICE CODE                             
         MVC   CACCAGY,POWCODE     SAVE AGY CODE                                
*                                                                               
VAX      DS    0H                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SYSSW       ORIGINAL SYS                                 
         L     RF,ACOMFACS         SWITCH BACK                                  
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0             ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATADISP,=H'24'     FOR SPOT                                     
VAXX     B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE INTERFACE CODE                                                
***********************************************************************         
VALIFC   NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R2,CLTIFCDH         NO INPUT, SKIP FIELD                         
         CLI   5(R2),0                                                          
         BE    IFC10                                                            
         MVC   CCLTIFC,8(R2)       STORE INPUT IF PRESENT                       
         B     IFCX                                                             
IFC10    DS    0H                                                               
         CLC   =C'H9',AGENCY                                                    
         BE    ERRMIS                                                           
IFCX     B     XIT                                                              
***********************************************************************         
VALRATE  NTR1                       *VALIDATE RATING SERVICE*                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,RATETABU                                                      
         LA    RE,2                                                             
RTLOOP1A CLC   8(3,R2),0(R1)        SEE IF INPUT WAS AN US SERVICE              
         BNE   RTLOOP1                                                          
         MVC   CPROF+3(1),3(R1)                                                 
         B     VRATE05A                                                         
RTLOOP1  LA    R1,4(R1)                                                         
         BCT   RE,RTLOOP1A                                                      
*                                                                               
VRATE01  LA    R1,RATETABC                                                      
         LA    RE,2                                                             
RTLOOP2A CLC   8(3,R2),0(R1)        SEE IF INPUT WAS A CANADIAN SERVICE         
         BNE   RTLOOP2                                                          
         MVC   CPROF+3(1),3(R1)                                                 
         B     VRATE05A                                                         
RTLOOP2  LA    R1,4(R1)                                                         
         BCT   RE,RTLOOP2A                                                      
         B     ERRINV               NEITHER, THEN ERROR                         
*                                                                               
VRATE05A CLI   CLTMED,C'R'          FOR MEDIA R                                 
         BNE   VRATE10              RATING SERVICE=1 ONLY                       
         MVI   CPROF+3,C'1'                                                     
VRATE10  CLI   CLTMED,C'T'          FOR MEDIA T                                 
         BNE   VRATE20              AND "NOT" CANADIAN AGY                      
         CLI   SVAPROF+7,C'C'       RATING SERVICE=0 ONLY                       
         BE    VRATE20                                                          
         MVI   CPROF+3,C'0'                                                     
*                                                                               
VRATE20  DS    0H                   CROSS-CHECK WITH AGYHDR                     
         CLI   CPROF+3,C'0'                                                     
         BNE   CKARB                                                            
         CLI   SVAPROF,C'0'         AGYPROF+0=RATING SERVICE                    
         BE    VRATEX                                                           
         CLI   SVAPROF,C'2'                                                     
         BE    VRATEX                                                           
*                                                                               
PROFERR  LA    R2,CLTOP4H                                                       
         B     ERRINV                                                           
*                                                                               
CKARB    CLI   SVAPROF,C'1'                                                     
         BE    VRATEX                                                           
         CLI   SVAPROF,C'2'                                                     
         BNE   PROFERR                                                          
*                                                                               
VRATEX   B     XIT                                                              
         DROP  R6                                                               
***********************************************************************         
*********** NO MORE BRD BUYING ON CLIENT ADD - 9/19/01 ****************         
***********************************************************************         
VALPROF1 NTR1                      *VALIDATE BRAND/POL TRNDS*                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    *+12                YES                                          
         TM    4(R2),X'20'         TEST VALIDATED                               
         BO    PROF1X                                                           
                                                                                
         MVI   CPOLONLY,C'Y'       ASSUME POL BUY ONLY                          
*                                                                               
         CLC   8(2,R2),=C'BP'      MATCH AGAINST VALID INPUTS                   
         BNE   *+12                                                             
         MVI   CPROF+0,C'1'                                                     
         B     PROF1X                                                           
*                                                                               
         CLC   8(2,R2),=C'TP'                                                   
         BNE   *+12                                                             
         MVI   CPROF+0,C'0'                                                     
         B     PROF1X                                                           
*                                                                               
         CLC   8(2,R2),=C'BR'                                                   
         BE    ERRNOBRD            NO MORE ADDS OF BRD BUYING                   
***                                                                             
* DO NOT WANT TO DEFAULT TO BRD IF FIELD IS EMPTY OR NOT VALID OPTION!          
***                                                                             
         MVI   CPROF+0,C'1'                                                     
*                                                                               
PROF1X   OC    CPWPCT,CPWPCT       PW CLT - ALWAYS BRAND POL                    
         BZ    *+14                                                             
         CLC   8(2,R2),=C'BP'                                                   
         BNE   ERRINV                                                           
         OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
***********************************************************************         
VALMST   NTR1                      *VALIDATE MARKET/STA TRND*                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
VMST10   CLC   8(2,R2),=C'U5'      MATCH AGAINST VALID INPUTS                   
         BNE   VMST20                                                           
         MVI   CPROF+2,C'0'                                                     
         B     VMSTX                                                            
VMST20   CLC   8(3,R2),=C'U3M'                                                  
         BNE   VMST25                                                           
         MVI   CPROF+2,C'2'                                                     
         B     VMSTX                                                            
VMST25   CLC   8(3,R2),=C'U3S'                                                  
         BNE   VMST30                                                           
         MVI   CPROF+2,C'3'                                                     
         B     VMSTX                                                            
VMST30   CLC   8(2,R2),=C'NO'                                                   
         BNE   VMST35                                                           
         MVI   CPROF+2,X'00'                                                    
         B     VMSTX                                                            
VMST35   B     ERRINV              NO MATCH, INVALID INPUT                      
VMSTX    XIT                                                                    
***********************************************************************         
VALPROF7 NTR1                      *VALIDATE PRT CLT CODE AS AAN*               
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         CLI   ACTEQU,ACTADD       AND ACTON ADD                                
         BNE   PROF705                                                          
         CLI   CLTLASTC,C'0'                                                    
         BE    PROF703                                                          
         CLI   CLTLASTC,C'6'                                                    
         BL    PROF705                                                          
         CLI   CLTLASTC,C'9'                                                    
         BH    PROF705                                                          
PROF703  DS    0H                                                               
         MVI   8(R2),C'Y'          IF LAST CHARACTER X: C'6'<=X<=C'0'           
         B     PROF7X              FORCE 'Y' IN THIS FIELD                      
*                                                                               
PROF705  DS    0H                                                               
         LA    R1,PROFTAB7                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
PROF710  CLC   8(1,R2),0(R1)                                                    
         BE    PROF7X                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,PROF710                                                       
         B     ERRINV                                                           
PROF7X   MVC   CPROF+6(1),8(R2)                                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALPROF8 NTR1                      *VALIDATE PRINT EST SERIES NM*               
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,PROFTAB8                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
PROF810  CLC   8(1,R2),0(R1)                                                    
         BE    PROF8X                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,PROF810                                                       
         B     ERRINV                                                           
PROF8X   MVC   CPROF+7(1),8(R2)                                                 
         B     XIT                                                              
*=====================================================================*         
* ABEA AND MHER VISITED HERE ON AUG11/00.                                       
* NORMALLY, CPP GUIDE IS REQUIRED FOR GOALS INPUT.                              
* 0 (N) MEANS DON'T OVERRIDE THE NORMAL REQUIREMENT                             
* 1 (Y) MEANS NO CPP GUIDE IS REQUIRED                                          
* IT WAS WRONG FOR QUITE A WHILE !                                              
*=====================================================================*         
VALPROF9 NTR1                      *VALIDATE GOALS CPP OVERRIDE*                
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,PROFTAB9         TABLE IS IN THE FORMAT OF                    
         LLC   R5,0(R1)            (Y 0 N 1)                                    
         LA    R1,1(R1)            IF INPUT=Y, 1 GETS STORED                    
PROF910  CLC   8(1,R2),0(R1)       IF INPUT=N, 0 GETS STORED                    
         BE    PROF9X                                                           
         LA    R1,2(R1)            BUMP POINTER BY 2 BYTES                      
         BCT   R5,PROF910                                                       
         B     ERRINV                                                           
PROF9X   MVC   CPROF+8(1),1(R1)    !!! 0/1 IS STORED: NOT Y/N !!!               
         B     XIT                                                              
***********************************************************************         
VALPROFA NTR1                      *VALIDATE PROGRAM ADJ. CONTROL*              
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,PROFTABA                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
PROFA10  CLC   8(1,R2),0(R1)       IS THERE A MATCH?                            
         BE    PROFAX                                                           
         LA    R1,2(R1)            POINT TO THE NEXT VALID ENTRY                
         BCT   R5,PROFA10                                                       
         B     ERRINV                                                           
PROFAX   MVC   CPROF+9(1),1(R1)    MOVE IN (0->0) (1->A) (2->N)                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALPROFB NTR1                      *VALIDATE POL TIMESHEET DEMOS*               
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,PROFTABB                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
PROFB10  CLC   8(1,R2),0(R1)                                                    
         BE    PROFBX                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,PROFB10                                                       
         B     ERRINV                                                           
PROFBX   MVC   CPROF+10(1),8(R2)                                                
         B     XIT                                                              
***********************************************************************         
VALPROFC NTR1                      *VALIDATE FROCE EST SERIES REQ*              
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,PROFTABC                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
PROFC10  CLC   8(1,R2),0(R1)                                                    
         BE    PROFCX                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,PROFC10                                                       
         B     ERRINV                                                           
PROFCX   MVC   CPROF+11(1),8(R2)                                                
         B     XIT                                                              
***********************************************************************         
VALPROFD NTR1                      *VALIDATE PRD REQ FOR TRUE POL*              
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,PROFTABD                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
PROFD10  CLC   8(1,R2),0(R1)                                                    
         BE    PROFDX                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,PROFD10                                                       
         B     ERRINV                                                           
PROFDX   MVC   CPROF+12(1),8(R2)                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALPROFE NTR1                      *VALIDATE EXCLUSION GROUP CODE*              
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,ALPHANUM                                                      
PROFE10  CLC   8(1,R2),0(R1)                                                    
         BE    PROFEX                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'00'                                                      
         BE    ERRINV                                                           
         B     PROFE10                                                          
PROFEX   MVC   CPROF+13(1),8(R2)                                                
         B     XIT                                                              
***********************************************************************         
VALPROFF NTR1                      *VALIDATE CLIENT RATE CONTROL*               
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,PROFTABF                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
PROFF10  CLC   8(1,R2),0(R1)                                                    
         BE    PROFFX                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,PROFF10                                                       
         B     ERRINV                                                           
PROFFX   MVC   CPROF+14(1),8(R2)                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALXTRA1 NTR1                      *VALIDATE CANADIAN DEMO OPTION*              
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,XTRATAB1                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
XTRA110  CLC   8(1,R2),0(R1)                                                    
         BE    XTRA1X                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,XTRA110                                                       
         B     ERRINV                                                           
XTRA1X   MVC   CEXTRA+0(1),8(R2)                                                
         B     XIT                                                              
***********************************************************************         
VALXTRA3 NTR1                      *VALIDATE BUY ID REQUIRED*                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,XTRATAB3+1       +1 TO SAVE THE LA INSTRUCTION!               
         LLC   R5,XTRATAB3                                                      
*                                                                               
XTRA310  CLC   8(1,R2),0(R1)                                                    
         BE    XTRA3X                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,XTRA310                                                       
         B     ERRINV                                                           
XTRA3X   MVC   CEXTRA+2(1),8(R2)                                                
*                                                                               
         CLI   CEXTRA+2,C'N'          IF BUY ID NOT REQUIRED                    
         BNE   XIT                                                              
         MVC   CLTTTLE,SPACES         CLEAR ID TITLE ON SCREEN AND              
         MVI   CLTTTLEH+5,0                                                     
         XC    CTITLE,CTITLE          IN THE RECORD                             
*                                                                               
         B     XIT                                                              
***********************************************************************         
VALXTRA4 NTR1                      *VALIDATE ESTIMATE FILTERS REQ*              
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,XTRATAB4                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
XTRA410  CLC   8(1,R2),0(R1)                                                    
         BE    XTRA4X                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,XTRA410                                                       
         B     ERRINV                                                           
XTRA4X   MVC   CEXTRA+3(1),8(R2)                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALXTRA6 NTR1                      *VALIDATE U.S SPILL*                         
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,XTRATAB6                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
XTRA610  CLC   8(1,R2),0(R1)                                                    
         BE    XTRA6X                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,XTRA610                                                       
         B     ERRINV                                                           
XTRA6X   MVC   CEXTRA+5(1),8(R2)                                                
         B     XIT                                                              
***********************************************************************         
VALXTRA7 NTR1                      *VALIDATE 'EST=NO' EST NAME*                 
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,XTRATAB7                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
XTRA710  CLC   8(1,R2),0(R1)                                                    
         BE    XTRA7X                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,XTRA710                                                       
         B     ERRINV                                                           
XTRA7X   MVC   CEXTRA+6(1),8(R2)                                                
         B     XIT                                                              
***********************************************************************         
VALXTRA9 NTR1                      *VALIDATE GOAL REQD FOR BUY*                 
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,XTRATAB9                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
XTRA910  CLC   8(1,R2),0(R1)                                                    
         BE    XTRA9X                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,XTRA910                                                       
         B     ERRINV                                                           
XTRA9X   MVC   CEXTRA+8(1),8(R2)                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALCTRY  NTR1                      *VALIDATE COUNTRY*                           
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,CTRYTAB                                                       
         LA    R5,2                                                             
CTRY10   CLC   8(3,R2),0(R1)                                                    
         BE    CTRYX                                                            
         LA    R1,4(R1)                                                         
         BCT   R5,CTRY10                                                        
         B     ERRINV                                                           
CTRYX    MVC   CEXTRA+9(1),3(R1)                                                
         B     XIT                                                              
***********************************************************************         
VALXTRAB NTR1                      *VALIDATE OUT-OF-WEEK CLIENT*                
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,XTRATABB                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
XTRAB10  CLC   8(1,R2),0(R1)                                                    
         BE    XTRABX                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,XTRAB10                                                       
         B     ERRINV                                                           
XTRABX   MVC   CEXTRA+10(1),8(R2)                                               
         B     XIT                                                              
***********************************************************************         
VALXTRAC NTR1                      *VALIDATE GST CODE*                          
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,XTRATABC+1                                                    
         LLC   R5,XTRATABC                                                      
*        LA    R1,1(R1)                                                         
XTRAC10  CLC   8(1,R2),0(R1)                                                    
         BE    XTRACX                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,XTRAC10                                                       
         CLI   8(R2),0                                                          
         BNE   ERRINV                                                           
XTRACX   MVC   CEXTRA+11(1),8(R2)                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALXTRAD NTR1                      *VALIDATE SPECIAL DEMO ADJ.*                 
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,XTRATABD                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
XTRAD10  CLC   8(1,R2),0(R1)                                                    
         BE    XTRADX                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,XTRAD10                                                       
         B     ERRINV                                                           
XTRADX   MVC   CEXTRA+12(1),8(R2)                                               
         B     XIT                                                              
***********************************************************************         
VALXTRAE NTR1                      *VALIDATE PRD REQD FOR ADDS SEND*            
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         LA    R1,XTRATABE                                                      
         LLC   R5,0(R1)                                                         
         LA    R1,1(R1)                                                         
XTRAE10  CLC   8(1,R2),0(R1)                                                    
         BE    XTRAEX                                                           
         LA    R1,1(R1)                                                         
         BCT   R5,XTRAE10                                                       
         B     ERRINV                                                           
XTRAEX   MVC   CEXTRA+13(1),8(R2)                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALDLY   NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         MVI   CDAILY,0                                                         
         CLI   5(R2),0                                                          
         BE    DLYX2                                                            
         CLI   8(R2),C'N'                                                       
         BE    DLYX2                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERRINV                                                           
DLYX1    MVC   CDAILY,8(R2)                                                     
DLYX2    B     XIT                                                              
***********************************************************************         
VALPRD   NTR1                      *NO-OP*                                      
         B     XIT                                                              
***********************************************************************         
VALTTL   NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    TTL10                                                            
         MVC   CTITLE,CLTTTLE                                                   
         OC    CTITLE,SPACES                                                    
         B     TTLX                                                             
*                                                                               
TTL10    CLI   CEXTRA+2,C'N'       IF BUY-ID REQUIRED=Y, THEN MISSING           
         BNE   ERRMIS              MESSAGE SENT ON BLANK FIELD                  
TTLX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALSAPI  NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         CLI   SAPAGY,C'Y'         TEST SAP AGY                                 
         BNE   SAPX                                                             
*                                                                               
         GOTO1 ANY                                                              
         MVC   CSAPCODE,WORK                                                    
*                                                                               
SAPX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALMED   NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
                                                                                
         TM    FLAG1,X'40'         IF SPECIAL MEDIA NAMES                       
         BNO   MEDX                                                             
         XC    CMEDNAME,CMEDNAME                                                
         CLI   5(R2),0                                                          
         BE    MEDX                                                             
         CLI   8(R2),C'='          VALID INPUT MUST BEGIN WITH =                
         BNE   ERRINV                                                           
         MVC   CMEDNAME,9(R2)      ONLY STORE THE STRING BEYOND =               
MEDX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
VALPST   NTR1                                                                   
         L     R6,AIO              A(CLIENT RECORD)                             
         USING CLTRECD,R6          CLIENT RECORD DSECT                          
         LA    R4,CPST             PST IN CLIENT RECORD                         
         LA    R5,L'CPST-1         LENGTH OF PST FIELD -1 FOR EX                
         CR    R4,R3               POINTING TO PST IN CLIENT RECORD?            
         BE    *+8                 YES - PROCESSING PST FIELD                   
         LA    R5,L'CMPST-1        LENGTH OF MAIN PST FIELD -1 FOR EX           
         EX    R5,*+8              EXECUTE                                      
         B     *+10                BRANCH SO ASSEMBLER DOESN'T COMPLAIN         
         XC    0(0,R3),0(R3)       CLEAR PST/MAIN PST IN RECORD                 
         CLI   5(R2),0             ANY PST INPUT?                               
         BE    VALPSTX             NO - EXIT                                    
*                                                                               
         MVI   BYTE,PSTVALQ        ACTION = VALIDATE                            
         BRAS  RE,PSTVAL           CALL PSTVAL                                  
*                                                                               
VALPSTX  B     XIT                 EXIT                                         
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                          ADD A RECORD                               *         
***********************************************************************         
ADREC    NTR1                                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         MVC   SVCLTKEY(13),0(R6)                                               
         MVC   BYTE,CKEYAM         CONVERT BAGYMD INTO 1 CHAR MEDIA             
         NI    BYTE,X'0F'          TURN OFF AGENCY PORTION                      
         LA    R5,MEDTAB                                                        
*                                                                               
DK10     CLC   BYTE,1(R5)                                                       
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)     NEXT ITEM IN MEDIA TABLE                     
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   DK10                                                             
*                                                                               
*PRINT END OF MEDIA TABLE ERROR.                                                
*                                                                               
DK20     MVC   CLTMED(1),0(R5)                                                  
         OI    CLTMEDH+6,X'80'                                                  
         MVI   CLTMEDH+5,X'01'     FORCE THE EXECUTION OF VALIMED               
*                                  CONVERT BINARY CLT CODE TO 3 CHAR            
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),CLTCLT                             
         OI    CLTCLTH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         LA    R2,CLTMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         MVC   CLTMEDN(L'MEDNM),MEDNM                                           
         OI    CLTMEDNH+6,X'80'                                                 
*                                                                               
         L     R6,AIO                                                           
         USING AGYHDRD,R6          SAVING SOME IMPORTANT AGENCY INFO            
*                                                                               
         MVC   FLAG1,AGYFLAG1                                                   
         MVC   ACCOFC,AGYOFC2                                                   
         MVC   SVCTAGY,AGYCTAGY                                                 
*                                                                               
         MVI   ELCODE,X'03'        ACC AGY LIST ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ACCAGY,2(R6)        SAVE ACC AGENCY LIST                         
*                                                                               
         MVI   CLTCLTH+5,3         SET LEN                                      
         CLI   CLTCLT+2,C' '                                                    
         BH    *+8                                                              
         MVI   CLTCLTH+5,2         SET LEN                                      
         LA    R2,CLTCLTH          CALL VALICLT TO CHECK SECURITY               
         GOTO1 VALICLT                                                          
*                                                                               
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE                            
         BNE   DK30                                                             
*                                                                               
         CLI   SVAPROF+7,C'C'       IF CANANDIAN AGENCY                         
         BNE   DK24                                                             
         CLI   QMED,C'C'           MEDIUM C AND N ONLY FOR DISPLAY              
         BE    ERRINV                                                           
         CLI   QMED,C'N'                                                        
         BE    ERRINV                                                           
*                                                                               
DK24     CLI   T217FFD+1,C'*'      TEST DDS TERM                                
         BE    DK30                                                             
         TM    T217FFD+12,X'40'                                                 
         BO    ERRSEC2             ON = NO CHANGE                               
*                                                                               
DK30     MVC   KEY(13),SVCLTKEY                                                 
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0X                                                               
*                                                                               
         MVC   KEY,SVCLTKEY        RESTORE THE KEY                              
         LA    R4,TABLES           TABLE OF VR AND DR ROUTINE ADDRESS           
         LA    R4,OPTSTART(R4)                                                  
         USING OPTTABD,R4                                                       
DR10     LH    R2,SCRDISD                                                       
         CH    R2,=X'FFFF'         SEE IF AT END OF TABLE                       
         BE    DRX                                                              
         AR    R2,RA               POINT R2 TO THE HEADER OF ENTRY              
         LH    R3,RECDISD                                                       
         A     R3,AIO              POINT R3 TO THE TARGET IN AIO                
         L     RF,ADISD            DISPLAY FIELD ON SCREEN                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         LA    R4,OPTTABQ(R4)      BUMP THE TABLE POINTER                       
         B     DR10                                                             
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
DISNAME  NTR1                      *DISPLAY CLIENT NAME*                        
         MVC   8(L'CNAME,R2),0(R3)                                              
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
DISACC   NTR1                      *DISPLAY AGENCY CODE*                        
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         MVC   8(2,R2),0(R3)                                                    
         OC    CACCAGY,CACCAGY                                                  
         BNZ   DISACC10                                                         
         MVC   CLTAOFC+2(3),SPACES                                              
         B     DISACCX                                                          
*DISACC10 MVC   CLTAOFC+2(1),=C'/'    NEED MORE BYTES!!!                        
DISACC10 MVI   CLTAOFC+2,C'/'         AHHHHH...THATS MUCH BETTER!               
         MVC   CLTAOFC+3(2),CACCAGY                                             
DISACCX  OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         DROP  R6                                                               
***********************************************************************         
DISIFC   NTR1                      *DISPLAY INTERFACE CODE*                     
         MVC   8(8,R2),0(R3)                                                    
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
DISONUM  NTR1                      *DISPLAY OFFICE CODE AND NAME*               
         XC    OFCBLK,OFCBLK                                                    
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,0(R3)                                                     
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(X'01',ACOMFACS),           X        
               (C'S',CLTONAM)                                                   
         MVC   8(2,R2),OFCOFC2                                                  
         OI    6(R2),X'80'                                                      
         OI    CLTONAMH+6,X'80'                                                 
         B     XIT                                                              
***********************************************************************         
DISCPP   NTR1                                                                   
         LA    R1,PROFTAB9         POINT TO THE ENTRY TABLE FOR CPP.            
         LLC   R5,0(R1)            GET THE COUNT OF ENTRIES                     
         LA    R1,1(R1)            POINT TO THE POSSIBLE DISPLAY ENTRY          
DISCPP10 CLC   0(1,R3),1(R1)       MATCH WITH POSSIBLE RECORD ENTRIES           
         BE    DISCPPX                                                          
         LA    R1,2(R1)                                                         
         BCT   R5,DISCPP10                                                      
DISCPPX  MVC   8(1,R2),0(R1)                                                    
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
DISADJ   NTR1                                                                   
         LA    R1,PROFTABA         POINT TO THE ENTRY TABLE FOR ADJ.            
         LLC   R5,0(R1)            GET THE COUNT OF ENTRIES                     
         LA    R1,1(R1)            POINT TO THE POSSIBLE DISPLAY ENTRY          
DISADJ10 CLC   0(1,R3),1(R1)       MATCH WITH POSSIBLE RECORD ENTRIES           
         BE    DISADJX                                                          
         LA    R1,2(R1)                                                         
         BCT   R5,DISADJ10                                                      
DISADJX  MVC   8(1,R2),0(R1)                                                    
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
DISYN    NTR1                      *DISPLAY YES OR NO FOR FIELDS*               
         CLI   0(R3),C'Y'                                                       
         BE    DISYES                                                           
         CLI   0(R3),C'1'                                                       
         BE    DISYES                                                           
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
DISYES   MVI   8(R2),C'Y'                                                       
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
DISSAME  NTR1                      *DISPLAY STRAIGHT FROM THE RECORD*           
         MVC   8(1,R2),0(R3)                                                    
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
DISBUY   NTR1                      *DISPLAY BUYING TYPE*                        
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         CLI   CPOLONLY,C'N'                                                    
         BE    *+12                                                             
         CLI   CPOLONLY,X'00'                                                   
         BNE   DISBUY10                                                         
         DROP  R6                                                               
*                                  R3 -> BRAND POL T/A                          
         CLI   0(R3),C'0'          BRD?                                         
         BE    *+14                                                             
         MVC   8(4,R2),=C'BPOL'                                                 
         B     DISBUYX                                                          
*                                                                               
         MVC   8(4,R2),=C'BRD '                                                 
         B     DISBUYX                                                          
*                                  R3 -> BRAND POL T/A                          
DISBUY10 CLI   0(R3),C'0'          TPOL?                                        
         BNE   DISBUY20            NO - BPOL                                    
         MVC   8(4,R2),=C'TPOL'                                                 
         B     DISBUYX                                                          
*                                                                               
DISBUY20 MVC   8(4,R2),=C'BPOL'                                                 
*                                                                               
DISBUYX  OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         B     XIT                                                              
***********************************************************************         
DISCTRY  NTR1                      *DISPLAY COUNTRY*                            
         CLI   0(R3),C'C'                                                       
         BNE   DISCT10                                                          
         MVC   8(3,R2),=C'CAN'                                                  
         B     DISCTX                                                           
DISCT10  MVC   8(3,R2),=C'USA'                                                  
DISCTX   OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
DISRATE  NTR1                      *DISPLAY RATING SERVICES*                    
         CLI   0(R3),C'0'                                                       
         BNE   RATE20                                                           
         CLI   SVAPROF+7,C'C'                                                   
         BNE   RATE10                                                           
         MVC   8(3,R2),=C'CSI'                                                  
         B     RATEX                                                            
RATE10   MVC   8(3,R2),=C'NSI'                                                  
         B     RATEX                                                            
RATE20   CLI   SVAPROF+7,C'C'                                                   
         BNE   RATE30                                                           
         MVC   8(3,R2),=C'BBM'                                                  
         B     RATEX                                                            
RATE30   MVC   8(3,R2),=C'ARB'                                                  
RATEX    OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
DISDLY   NTR1                      *DISPLAY DAILY EST*                          
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'                                                      
         CLI   0(R3),0                                                          
         BE    XITDLY                                                           
         MVC   8(1,R2),0(R3)                                                    
         OI    6(R2),X'80'                                                      
XITDLY   B     XIT                                                              
***********************************************************************         
DISTTL   NTR1                      *DISPLAY TITLE*                              
         MVC   8(10,R2),0(R3)                                                   
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
DISSAPI  NTR1                      *DISPLAY SAP CODE*                           
         XC    8(10,R2),8(R2)                                                   
         CLI   SAPAGY,C'Y'         TEST PROTECTED                               
         BNE   DISSAPX                                                          
         MVC   8(10,R2),0(R3)                                                   
DISSAPX  OI    6(R2),X'80'                                                      
         B     XIT                                                              
***********************************************************************         
DISMED   NTR1                      *DISPLAY MEDIA TITLE*                        
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         MVC   8(11,R2),SPACES                                                  
         TM    FLAG1,X'40'         SPECIAL MEDIA NAME?                          
         BNO   DISMEDX                                                          
         CLC   CMEDNAME,SPACES                                                  
         BNH   DISMEDX                                                          
         MVI   8(R2),C'='          START WITH '=' SO CAN IGNORE GARBAGE         
         MVC   CLTMDNM+1(10),CMEDNAME                                           
DISMEDX  OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
DISMST   NTR1                      *DISPLAY MKT/STA TRND*                       
         OI    6(R2),X'80'                                                      
         CLI   0(R3),C'0'                                                       
         BE    *+12                                                             
         CLI   0(R3),C'1'                                                       
         BNE   MST20                                                            
         MVC   8(3,R2),=C'U5 '                                                  
         B     MSTX                                                             
MST20    CLI   0(R3),C'2'                                                       
         BNE   MST30                                                            
         MVC   8(3,R2),=C'U3M'                                                  
         B     MSTX                                                             
MST30    CLI   0(R3),C'3'                                                       
         BNE   MST35                                                            
         MVC   8(3,R2),=C'U3S'                                                  
         B     MSTX                                                             
MST35    CLI   0(R3),X'00'                                                      
         BNE   MSTX                                                             
         MVC   8(3,R2),=C'NO '                                                  
MSTX     B     XIT                                                              
***********************************************************************         
DISBAID  NTR1                      *DISPLAY BUYING AGY ID*                      
*                                                                               
         L     R6,AIO              A(CLIENT RECORD)                             
         USING CLTRECD,R6          CLIENT RECORD DSECT                          
         MVC   8(3,R2),SPACES      INIT BUYING AGID ON SCREEN TO SPACES         
         CLC   CBUYAGIS,SPACES     HAVE ANYTHNG ON THE RECORD TO SHOW?          
         BNH   *+10                NO                                           
         MVC   8(3,R2),CBUYAGIS    YES - MOVE FROM REC TO SCREEN                
         OI    6(R2),X'80'         TRANSMIT                                     
         B     XIT                                                              
*                                                                               
***********************************************************************         
DISCLTSQ NTR1                      *DISPLAY SPCL TRFC CLT/SEQ*                  
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         MVC   8(6,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         OC    CMCLTCOD,CMCLTCOD        TEST SPCL TRAFFIC CLIENT                
         BZ    DISCLTX                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A15'   GET CLUNPK ADDRESS                      
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'              CHECK FOR ERROR                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                  LOAD A(CLUNPK)                          
         GOTO1 (RF),(R1),(C'Y',CMCLTCOD),8(R2)                                  
*                                                                               
         LA    R4,10(R2)                                                        
         CLI   0(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   1(R4),C'='                                                       
*                                       UNPACK CLT CODE INTO FLD                
         GOTO1 HEXOUT,DMCB,CMCLTUNQ,2(R4),1,=C'TOG'                             
*                                                                               
DISCLTX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
DISPST   NTR1                      *DISPLAY PST CODE*                           
*                                                                               
         L     R6,AIO              A(CLIENT RECORD)                             
         USING CLTRECD,R6          CLIENT RECORD DSECT                          
         LA    R4,CPST             PST IN CLIENT RECORD                         
         LA    R5,L'CLTPST-1       PST LENGTH ON SCREEN -1 FOR EX               
         LA    R1,L'CPST-1         PST LENGTH IN RECORD -1 FOR EX               
         CR    R4,R3               POINTING TO PST IN CLIENT RECORD?            
         BE    *+12                YES - PROCESSING PST FIELD                   
         LA    R5,L'CLTMPST-1      MAIN PST LENGTH ON SCREEN -1 FOR EX          
         LA    R1,L'CMPST-1        MAIN PST LENGTH IN RECORD -1 FOR EX          
         EX    R5,*+8              EXECUTE                                      
         B     *+10                BRANCH SO ASSEMBLER DOESN'T COMPLAIN         
         MVC   8(0,R2),SPACES      CLEAR PST/MAIN PST FIELD ON SCREEN           
         OI    6(R2),X'80'         TRANSMIT                                     
         EX    R1,*+8              EXECUTE                                      
         B     *+10                BRANCH SO ASSEMBLER DOESN'T COMPLAIN         
         OC    0(0,R3),0(R3)       ANYTHING TO DISPLAY IN RECORD?               
         BZ    DISPSTX             NO - EXIT                                    
         CR    R4,R3               POINTING TO PST IN CLIENT RECORD?            
         BE    DISPST10            YES                                          
         LA    R4,WORK             POINT TO NEW PST STRING                      
         XC    WORK(10),WORK       BUILD 10 CHAR PST STRING                     
         LLC   R1,0(R3)            POSTION IN STRING                            
         BCTR  R1,0                -1 FOR INDEXING                              
         AR    R1,R4               PUT CHARACTER HERE                           
         MVC   0(1,R1),1(R3)       MOVE THE CHARACTER INTO POSTION              
         LR    R3,R4               R3 NOW POINTS TO WORK                        
*                                                                               
DISPST10 MVI   BYTE,PSTFMTQ        ACTION = FORMAT (DISPLAY)                    
         BRAS  RE,PSTVAL           CALL PSTVAL                                  
*                                                                               
DISPSTX  J     XIT                 EXIT                                         
         DROP  R6                                                               
*                                                                               
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         J     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRCLI   MVI   ERROR,INVCLI                                                     
         B     VSFMERR                                                          
ERRSEC   MVI   ERROR,SECLOCK                                                    
         B     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRCGRP  MVC   ERRNUM,=AL2(CLTINGRP)                                            
         B     SPERREX                                                          
ERRPRDX  MVC   ERRNUM,=AL2(PRDEXIST)                                            
         B     SPERREX                                                          
ERRDADD  MVC   ERRNUM,=AL2(NODELADD)                                            
         B     SPERREX                                                          
ERRCORD  MVC   ERRNUM,=AL2(NOCOORD)                                             
         B     SPERREX                                                          
ERRTCP   MVC   ERRNUM,=AL2(NOTCLPD)                                             
         B     SPERREX                                                          
ERRTCLT  MVC   ERRNUM,=AL2(INVTCLT)                                             
         B     SPERREX                                                          
ERRTCLQ  MVC   ERRNUM,=AL2(INVTCLTQ)                                            
         B     SPERREX                                                          
ERRCTCLQ MVC   ERRNUM,=AL2(CHATCLT)                                             
         B     SPERREX                                                          
ERRCTPRD MVC   ERRNUM,=AL2(CHATPRD)                                             
         B     SPERREX                                                          
ERROFC   MVC   ERRNUM,=AL2(INVOFC)                                              
         B     SPERREX                                                          
ERR1OFC  MVC   ERRNUM,=AL2(OFCONE)                                              
         B     SPERREX                                                          
ERR2OFC  MVC   ERRNUM,=AL2(OFCTWO)                                              
         B     SPERREX                                                          
ERRAAGY  MVC   ERRNUM,=AL2(INVAAGY)                                             
         B     SPERREX                                                          
ERRSYS   MVC   ERRNUM,=AL2(SYSNOPEN)                                            
         B     SPERREX                                                          
ERRSWS   MVC   ERRNUM,=AL2(SYSWS)                                               
         B     SPERREX                                                          
ERRCMP   MVC   ERRNUM,=AL2(NOACCCMP)                                            
         B     SPERREX                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
ERRNOBRD MVC   ERRNUM,=AL2(NOBRD)                                               
         B     SPERREX                                                          
ERRMEDRX MVC   ERRNUM,=AL2(MEDRX)                                               
         B     SPERREX                                                          
SETUPERX MVC   ERRNUM,=AL2(SETUPERR)                                            
         B     SPERREX                                                          
ERRAGID1 MVC   ERRNUM,=AL2(BAGYIDE1)                                            
         B     SPERREX                                                          
ERRAGID2 MVC   ERRNUM,=AL2(BAGYIDE2)                                            
         B     SPERREX                                                          
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
NOTAUTH  EQU   175                 NOT AUTHORIZED FOR THIS FUNCTION             
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
CLTINGRP EQU   387                 CLIENT STILL EXIST IN GROUP                  
PRDEXIST EQU   526                 PRODUCT STILL EXIST FOR THIS CLIENT          
NODELADD EQU   525                 CAN'T DELETE ON ADD                          
NOCOORD  EQU   1075                AGENCY DOES NOT AUTHORIZE COORD.             
NOTCLPD  EQU   517                 NO TRAFFIC CLT/SEQ & PRD ALLOWED             
INVTCLT  EQU   518                 INVALID TRAFFIC CLT CODE                     
INVTCLTQ EQU   519                 INVALID TRAFFIC CLT SEQ NUMBER               
CHATCLT  EQU   520                 CAN'T CHANGE TRAF CLT/SEQ                    
CHATPRD  EQU   543                 CAN'T CHANGE TRAF PROD CODE                  
INVOFC   EQU   544                 INVALID OFFICE CODE #                        
OFCONE   EQU   1068                ONE CHAR OFFICE CODE REQUIRED                
OFCTWO   EQU   1069                TWO CHAR OFFICE CODE REQUIRED                
INVAAGY  EQU   545                 INVALID ACCOUNT AGENCY CODE                  
SYSNOPEN EQU   1071                ACC SYSTEM NOT OPEN                          
SYSWS    EQU   1072                CAN'T SWITCH TO ACC SYSTEM                   
NOACCCMP EQU   1073                UNABLE TO READ ACC COMPANY REC               
NOBRD    EQU   835                 NO MORE BRD BUYING USE BPOL/TPOL             
MEDRX    EQU   1201                MED R AND/OR X CANT EXIST FOR CLT            
SETUPERR EQU   1338                FILE SETUP INCORRECT - CONTACT DDS           
BAGYIDE1 EQU   1470                BUYING AGENCY ID MUST BE 3 CHARS             
BAGYIDE2 EQU   1471                BUYING AGENCY ID DOES NOT EXIST              
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TABLES, CONSTANTS                                            *         
***********************************************************************         
TABLES   DS    0H                                                               
RATETABU DC    CL3'NSI',CL1'0'     RATING SERVICE TABLE FOR USA                 
         DC    CL3'ARB',CL1'1'                                                  
RATETABC DC    CL3'CSI',CL1'0'     RATING SERVICE TABLE FOR CAN                 
         DC    CL3'BBM',CL1'1'                                                  
         DC    X'FF'                                                            
*                                                                               
CTRYTAB  DC    CL3'USA',CL1'U'                                                  
         DC    CL3'CAN',CL1'C'                                                  
***********************************************************************         
*        TABLE FOR MEDIA AND ELEMENT CODES                            *         
***********************************************************************         
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*             VALIDATION TABLE FOR PROFILE & EXTRA PROFILES           *         
***********************************************************************         
*      1 DC    AL1(03),C'BPOLTPOLBRD '          BRAND/POL TRNDS                 
*      2 DC    AL1(03),AL1(02),X'FF'            LOCK BOX NUM                    
*      3 !!    TABLE DEFINED AS MSTTAB          MKT/STA TURNAROUND              
*      4 DC    AL1(04),AL1(04),C'01'            RATING SERVICE                  
*      5 DC    AL1(07),C'0123459'               BILL FORMULA CNTRL              
*      6 DC    AL1(05),AL1(06),C'012'           BILL ESTIMATE CNTRL             
PROFTAB7 DC    AL1(02),C'YN'                    PRINT CLT CODE=AAN              
PROFTAB8 DC    AL1(11),C'0123456789*'           PRINT EST SERIES NM             
PROFTAB9 DC    AL1(02),C'N0Y1'                  GOALS CPP OVERRIDE              
PROFTABA DC    AL1(03),C'00A1N2'                PROGRAM ADJ CNTRL               
PROFTABB DC    AL1(03),C'012'                   POL TIMESHEET DEMOS             
PROFTABC DC    AL1(02),C'YN'                    FORCE EST SERIES REQ            
PROFTABD DC    AL1(03),C'0YN'                   PRD REQ FOR TRUE POL            
*      E DC    ALPHA-NUMERIC VALUES             EXCL GROUP CODE                 
PROFTABF DC    AL1(12),C'0123456789Y*'          CLIENT RATE CNTRL               
*                                                                     *         
***********************************************************************         
*                                                                     *         
XTRATAB1 DC    AL1(03),C'0UC'                   CANADIAN DEMO OPTION            
*        DC    AL1(04),AL1(17),C'01'            CANADIAN NET TAX                
XTRATAB3 DC    AL1(27)                          BUY ID REQ                      
         DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ*'                                   
XTRATAB4 DC    AL1(02),C'NY'                    EST FILTERS REQ                 
*        DC    AL1(04),AL1(20),C'0E'            CAMPAIGNS                       
XTRATAB6 DC    AL1(02),C'NY'                    US SPILL                        
XTRATAB7 DC    AL1(02),C'NY'                    EST=NO EST NAME                 
*        DC    AL1(04),AL1(23),C'NY'            MKGDS IN MISSED MTH             
XTRATAB9 DC    AL1(03),C'NYP'                   GOAL REQD FOR BUY               
*        !!    NEW TABLE AT CTRYTAB             COUNTRY                         
XTRATABB DC    AL1(02),C'NY'                    OUT-OF-WEEK                     
XTRATABC DC    AL1(04),C'SUXZ'                  GST CODE                        
XTRATABD DC    AL1(02),C'NY'                    SPECIAL DEMO ADJ                
XTRATABE DC    AL1(02),C'NY'                    PRD REQ FOR ADDS SEND           
         DC    X'0000'                                                          
         SPACE 2                                                                
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
         EJECT                                                                  
***********************************************************************         
*        PFKEYS TABLES                                                *         
***********************************************************************         
PFTABLE DS    0H                                                                
*        PRODUCT MAINT DISPLAY                                                  
         DC   AL1(PF02X-*,02,PFTCPROG,(PF02X-PF02)/KEYLNQ,0)                    
         DC   CL3'PM '                 MAINT                                    
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
PF02     DC   AL1(KEYTYTWA,L'CLTMED-1),AL2(CLTMED-T217FFD)                      
         DC   AL1(KEYTYTWA,L'CLTCLT-1),AL2(CLTCLT-T217FFD)                      
PF02X    EQU  *                                                                 
*                                                                               
*        PRODUCT LIST                                                           
         DC   AL1(PF03X-*,03,PFTCPROG,(PF03X-PF03)/KEYLNQ,0)                    
         DC   CL3'PL '                 MAINT                                    
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'LIST'                ACTION                                   
PF03     DC   AL1(KEYTYTWA,L'CLTMED-1),AL2(CLTMED-T217FFD)                      
         DC   AL1(KEYTYTWA,L'CLTCLT-1),AL2(CLTCLT-T217FFD)                      
PF03X    EQU  *                                                                 
*                                                                               
*        CLIENT2 MAINT DISPLAY                                                  
         DC   AL1(PF05X-*,05,PFTCPROG,(PF05X-PF05)/KEYLNQ,0)                    
         DC   CL3'C2'                  MAINT                                    
         DC   CL8'CL2'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
PF05     DC   AL1(KEYTYTWA,L'CLTMED-1),AL2(CLTMED-T217FFD)                      
         DC   AL1(KEYTYTWA,L'CLTCLT-1),AL2(CLTCLT-T217FFD)                      
PF05X    EQU  *                                                                 
*                                                                               
*        CLIENT LIST                                                            
         DC   AL1(PF06X-*,06,PFTCPROG,(PF06X-PF06)/KEYLNQ,0)                    
         DC   CL3'CL'                  MAINT                                    
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'LIST'                ACTION                                   
PF06     DC   AL1(KEYTYTWA,L'CLTMED-1),AL2(CLTMED-T217FFD)                      
         DC   AL1(KEYTYTWA,L'CLTCLT-1),AL2(CLTCLT-T217FFD)                      
PF06X    EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
OPTNTAB  DS    0F                                                               
OPTSTART EQU   *-TABLES                                                         
         DC    AL2(CLTCLTNH-T217FFD),AL2(CNAME-CKEY)     (NAME)                 
         DC    A(VALNAME),A(DISNAME)                                            
         DC    AL2(CLTONUMH-T217FFD),AL2(COFFICE-CKEY)   (OFFICE #)             
         DC    A(VALOFF),A(DISONUM)                                             
         DC    AL2(CLTTONH-T217FFD),AL2(CTRAFOFC-CKEY)  (traff. off. #)         
         DC    A(VALTOFF),A(DISSAME)                                            
         DC    AL2(CLTAOFCH-T217FFD),AL2(CACCOFC-CKEY)   (AGY CODE)             
         DC    A(VALACC),A(DISACC)                                              
         DC    AL2(CLTIFCDH-T217FFD),AL2(CCLTIFC-CKEY)   (INTERFACE)            
         DC    A(VALIFC),A(DISIFC)                                              
         DC    AL2(CLTOP1H-T217FFD),AL2(CPROF+0-CKEY)    (Buying type)          
         DC    A(VALPROF1),A(DISBUY)                                            
         DC    AL2(CLTOP3H-T217FFD),AL2(CPROF+2-CKEY)    (MKT/STA TRND)         
         DC    A(VALMST),A(DISMST)                                              
         DC    AL2(CLTOP8H-T217FFD),AL2(CPROF+7-CKEY)    (PRT EST NM)           
         DC    A(VALPROF8),A(DISSAME)                                           
         DC    AL2(CLTOP4H-T217FFD),AL2(CPROF+3-CKEY)    (RATING SRVC)          
         DC    A(VALRATE),A(DISRATE)                                            
         DC    AL2(CLTOP11H-T217FFD),AL2(CPROF+10-CKEY)  (POL TIMESHT)          
         DC    A(VALPROFB),A(DISSAME)                                           
         DC    AL2(CLTOP7H-T217FFD),AL2(CPROF+6-CKEY)    (PRT AAN)              
         DC    A(VALPROF7),A(DISYN)                                             
         DC    AL2(CLTOP9H-T217FFD),AL2(CPROF+8-CKEY)    (GOALS OVRIDE)         
         DC    A(VALPROF9),A(DISCPP)                                            
         DC    AL2(CLTOP10H-T217FFD),AL2(CPROF+9-CKEY)   (ADJ CONTROL)          
         DC    A(VALPROFA),A(DISADJ)                                            
         DC    AL2(CLTOP12H-T217FFD),AL2(CPROF+11-CKEY)  (FORCE EST)            
         DC    A(VALPROFC),A(DISYN)                                             
         DC    AL2(CLTOP13H-T217FFD),AL2(CPROF+12-CKEY)  (PRD REQ)              
         DC    A(VALPROFD),A(DISYN)                                             
         DC    AL2(CLTOP15H-T217FFD),AL2(CPROF+14-CKEY)  (CL RATE CTRL)         
         DC    A(VALPROFF),A(DISSAME)                                           
         DC    AL2(CLTEX1H-T217FFD),AL2(CEXTRA+0-CKEY)   (CAN DEMO OPT)         
         DC    A(VALXTRA1),A(DISSAME)                                           
         DC    AL2(CLTEX3H-T217FFD),AL2(CEXTRA+2-CKEY)   (BUY ID REQ)           
         DC    A(VALXTRA3),A(DISSAME)                                           
         DC    AL2(CLTEX4H-T217FFD),AL2(CEXTRA+3-CKEY)   (EST FLT REQ)          
         DC    A(VALXTRA4),A(DISYN)                                             
         DC    AL2(CLTEX6H-T217FFD),AL2(CEXTRA+5-CKEY)   (US SPILL)             
         DC    A(VALXTRA6),A(DISSAME)                                           
         DC    AL2(CLTEX7H-T217FFD),AL2(CEXTRA+6-CKEY)   ('EST=NO')             
         DC    A(VALXTRA7),A(DISYN)                                             
         DC    AL2(CLTEX9H-T217FFD),AL2(CEXTRA+8-CKEY)   (GOAL REQD)            
         DC    A(VALXTRA9),A(DISSAME)                                           
         DC    AL2(CLTEX10H-T217FFD),AL2(CEXTRA+9-CKEY)  (COUNTRY)              
         DC    A(VALCTRY),A(DISCTRY)                                            
         DC    AL2(CLTEX12H-T217FFD),AL2(CEXTRA+11-CKEY) (GST CODE)             
         DC    A(VALXTRAC),A(DISSAME)                                           
         DC    AL2(CLTEX13H-T217FFD),AL2(CEXTRA+12-CKEY) (SPC DEMO)             
         DC    A(VALXTRAD),A(DISYN)                                             
         DC    AL2(CLTDLYH-T217FFD),AL2(CDAILY-CKEY)     (DAILY EST)            
         DC    A(VALDLY),A(DISDLY)                                              
         DC    AL2(CLTOP14H-T217FFD),AL2(CPROF+13-CKEY)  (EXCLU GRP)            
         DC    A(VALPROFE),A(DISSAME)                                           
         DC    AL2(CLTBAIDH-T217FFD),AL2(CBUYAGIS-CKEY)  (BUYING AGYID)         
         DC    A(VALBAID),A(DISBAID)                                            
         DC    AL2(CLTEX11H-T217FFD),AL2(CEXTRA+10-CKEY) (OUT-OF-WEEK)          
         DC    A(VALXTRAB),A(DISYN)                                             
         DC    AL2(CLTEX14H-T217FFD),AL2(CEXTRA+13-CKEY) (PRD REQ)              
         DC    A(VALXTRAE),A(DISYN)                                             
         DC    AL2(CLTTTLEH-T217FFD),AL2(CTITLE-CKEY)    (ID TITLE)             
         DC    A(VALTTL),A(DISTTL)                                              
         DC    AL2(CLTSAPIH-T217FFD),AL2(CSAPCODE-CKEY)   SAP INTF Cd           
         DC    A(VALSAPI),A(DISSAPI)                                            
         DC    AL2(CLTMDNMH-T217FFD),AL2(CMEDNAME-CKEY)  (MEDIA TITLE)          
         DC    A(VALMED),A(DISMED)                                              
         DC    AL2(CLTPSTH-T217FFD),AL2(CPST-CKEY)                              
         DC    A(VALPST),A(DISPST)                                              
         DC    AL2(CLTMPSTH-T217FFD),AL2(CMPST-CKEY)                            
         DC    A(VALPST),A(DISPST)                                              
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,CONACTH          POINT TO ACTION                              
         CLI   T217FFD+1,C'*'      TEST DDS TERM                                
         BE    SETUP01                                                          
         TM    T217FFD+12,X'40'                                                 
         BNO   SETUP01             NOT ON = ALL OK                              
         CLI   ACTNUM,ACTCHA                                                    
         BE    ERRSEC2             CHANGE NOT ALLOWED                           
         CLI   ACTNUM,ACTADD                                                    
         BE    ERRSEC2             ADD NOT ALLOWED                              
*                                                                               
SETUP01  MVI   USEIO,C'N'          INITIALIZE WITH GENCON DOING DMCALLS         
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         MVI   IOOPT,C'Y'          NO REC UPDATES BY GENCON                     
*                                                                               
         OI    CLTREH+1,X'0C'      HIDE PF12=RETURN FIELD                       
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    *+8                 NO                                           
         NI    CLTREH+1,X'FF'-X'04' LIGHT UP PF12=RETURN FIELD                  
         OI    CLTREH+6,X'80'      TRANSMIT THE RESULT                          
*                                                                               
         XC    CLTPFKY+52(12),CLTPFKY+52  CLEAR PF6=CLT/LIST FIELD              
         OI    CLTPFKYH+6,X'80'     TRANSMIT THE RESULT                         
         CLI   ACTNUM,ACTSEL       ACTION SEL?                                  
         BE    SETUP05             YES                                          
         MVC   CLTPFKY+52(12),=CL12'PF6=Clt/List'  PF6=CLT/LIST FIELD           
         B     SETUP08                                                          
*                                                                               
SETUP05  CLI   PFKEY,6                                                          
         BNE   *+12                                                             
         MVI   PFKEY,X'FF'         PF6 IS INVALID IN THIS CASE                  
         B     SETUP10                                                          
*                                                                               
SETUP08  OC    PFKEY,PFKEY                                                      
         BNZ   SETUP10                                                          
         B     SETUPX                                                           
*                                                                               
SETUP10  GOTO1 INITPFKY,DMCB,PFTABLE      MAINT PF TABLE                        
*                                                                               
SETUPX   J     XIT                                                              
         EJECT                                                                  
         LTORG                                                        *         
*                                                                               
*                                                                               
***********************************************************************         
*              MANY FIELDS CO-EXISTS WITH OUTHERS, NEEDS TO CROSS-CHECK         
***********************************************************************         
CROSSCHK NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         CLI   CPROF+0,C'0'        CPROF+0 != 0 -> BRAND/POL = Y                
         BE    CROSS10             AND                                          
         CLI   CPROF+2,C'1'        CPROF+2 <= 1 -> MKT/STA = U5                 
         BH    CROSS10             THEN                                         
         MVI   CPROF+0,C'2'        2 HAS TO BE IN RECORD FOR CPROF+0            
*                                                                               
CROSS10  CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BE    CROSS20                                                          
         LA    R2,CLTEX1H                                                       
         CLI   CEXTRA+0,C'0'       CAN DEMO OPTION MUST BE 0                    
         BNE   CERRINV                                                          
*        LA    R2,CLTEX2H                                                       
*        CLI   CEXTRA+1,C'0'       CAN NETWORK TAX MUST BE 0                    
*        BNE   ERRINV                                                           
         LA    R2,CLTEX10H                                                      
         CLI   CEXTRA+9,C'C'       COUNTRY MUST BE USA                          
         BE    CERRINV                                                          
         MVI   CEXTRA+9,C'U'                                                    
         B     CROSS30                                                          
*                                                                               
CROSS20  CLI   CEXTRA+9,C'0'       CANADIAN AGENCY,DEF CNTRY IS CAN             
         BNE   *+8                                                              
         MVI   CEXTRA+9,C'C'                                                    
         CLI   CEXTRA+9,C'C'                                                    
         BE    CROSS30                                                          
         LA    R2,CLTEX10H                                                      
         CLC   AGENCY,=C'HD'       ONLY HDTO CAN HAVE USA CLIENT                
         BNE   CERRINV                                                          
*                                                                               
CROSS30  DS    0H                                                               
         CLI   CLTMED,C'R'         FOR MEDIA R                                  
         BNE   CROSSX                                                           
         CLI   ACTEQU,ACTADD       AND ACTON ADD                                
         BNE   CROSSX                                                           
         LA    R2,CLTEX11H                                                      
         CLI   CEXTRA+10,C'Y'      DON'T ALLOW OUT-OF-WEEK ROTATOR              
         BE    CERRINV                                                          
*                                                                               
CROSSX   J     XIT                                                              
*                                                                               
CERRINV  MVI   ERROR,INVALID                                                    
         J     VSFMERR                                                          
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*                          PUTREC                                     *         
***********************************************************************         
PTREC    NTR1  BASE=*,LABEL=*                                                   
         MVI   USEIO,C'Y'          DON'T LET GENCON DO PUTREC                   
         MVC   KEY,SVCLTKEY        NOW WE HAVE THE CORRECT DISK ADDRESS         
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         J     XIT                                                              
***********************************************************************         
*    MARKS CLIENT RECORD                                              *         
***********************************************************************         
MRKCLT   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),HALF2     AGENCY/MEDIA                                  
         MVC   KEY+2(2),SVCLTKEY+2 CLIENT                                       
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BNE   MRKCLTX                                                          
         GOTO1 GETREC                                                           
         MVI   KEY+13,X'DD'        SET DIR DELETED                              
         GOTO1 WRITE                                                            
         L     RE,AIO                                                           
         MVI   15(RE),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MRKCLTX  J     XIT                                                              
***********************************************************************         
*    DELETE CANADIAN CLIENT SPECIFIC NETWORK DEFINITION RECORDS       *         
***********************************************************************         
DELNET   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING NDEFRECD,R2                                                      
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,AGENCY     AGY                                          
         GOTO1 HIGH                                                             
         B     DN10                                                             
*                                                                               
DN5      GOTO1 SEQ                                                              
*                                                                               
DN10     CLC   KEYSAVE(4),KEY      STILL THIS AGY                               
         BNE   DNX                                                              
         CLC   NDEFKCLT,BCLT       THIS CLT                                     
         BNE   DN5                 NO GET NEXT                                  
         MVI   KEY+13,X'DD'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         MVI   15(RE),X'C0'                                                     
         BRAS  RE,PTREC            DELETE RECORD                                
         B     DN5                                                              
*                                                                               
DNX      J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*  CAN'T DELETE A CLIENT IF THE CLIENT STILL EXIST WITHIN A CLIENT GRP*         
***********************************************************************         
CK4GRP   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         L     R6,AIO1                                                          
         USING CLTRECD,R6                                                       
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING GRPRECD,R3                                                       
         MVI   GRPKTYP,GRPKTYPQ                                                 
         MVI   GRPKSTYP,GRPKCTYQ                                                
         MVC   GRPKAGMD,CKEYAM                                                  
         GOTO1 HIGH                                                             
         B     CK10                                                             
CK05     GOTO1 SEQ                                                              
*                                                                               
CK10     CLC   KEYSAVE(3),KEY                                                   
         BNE   CKX                                                              
         LA    R3,KEYSAVE                                                       
*                                                                               
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         LA    R6,24(RE)                                                        
         MVI   ELCODE,X'30'        VALUE ELEMENT                                
CK15     BAS   RE,NEXTEL                                                        
         BNE   CK05                                                             
         CLC   2(3,R6),QCLT                                                     
         BNE   CK15                                                             
         LA    R2,CLTCLTNH                                                      
*                                                                               
         B     ERRCGRP             CAN'T DELETE WITH CLIENT IN GRP              
*                                                                               
CKX      J     XIT                 CLT ISN'T IN ANY GRP -> RETURN               
         SPACE 2                                                                
*====================================================================*          
*    CHECK MEDIA C AND N RECORDS TOO                                 *          
*    SETS HALF(1)= AGY/MED FOR C AND HALF+1(1)= AGY/MED FOR N        *          
*====================================================================*          
CKCANDN  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
*        MVC   HALF,=H'0'         WHY GENERATE A LITERAL??? AKT 5/04            
         XC    HALF,HALF                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,6              AGENCY RECORD                                 
         MVC   KEY+1(2),AGENCY    AGENCY ALPHA                                  
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'               MUST BE ABLE TO FIND AGENCY RECORD            
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         LA    R6,24(RE)          REC+24                                        
         MVI   ELCODE,2                                                         
CKCN10   BAS   RE,NEXTEL                                                        
         BNE   CKCN30                                                           
*                                                                               
         CLI   2(R6),C'C'                                                       
         BNE   CKCN20                                                           
         MVC   HALF(1),3(R6)      SAVE BINARY AGY/MED FOR MED 'C'               
         B     CKCN10                                                           
CKCN20   CLI   2(R6),C'N'                                                       
         BNE   CKCN10                                                           
         MVC   HALF+1(1),3(R6)    SAVE BINARY AGY/MED FOR MED C'N'              
         B     CKCN10                                                           
*                                                                               
CKCN30   CLI   HALF,0              ARE THERE ANY C MEDIA                        
         BE    CKCN40              NO, THEN EXIT                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),HALF                                                    
         MVC   KEY+2(2),SVCLTKEY+2                                              
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CKCN40                                                           
         GOTO1 GETREC                                                           
         L     R6,AIO              THE CLIENT RECORD FOR MEDIA C                
         USING CLTRECD,R6                                                       
         OC    CLIST(100),CLIST    CHECK IF ANY PRODUCT STILL EXIST             
         BNZ   CKCNNO              ** ERROR - PRODUCTS EXIST                    
*                                                                               
CKCN40   CLI   HALF+1,0            ARE THERE ANY N MEDIA                        
         BE    CKCNYES             NO, THEN EXIT                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),HALF+1                                                  
         MVC   KEY+2(2),SVCLTKEY+2                                              
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CKCNYES                                                          
         GOTO1 GETREC                                                           
         L     R6,AIO             THE CLIENT RECORD FOR MEDIA N                 
         USING CLTRECD,R6                                                       
         OC    CLIST(100),CLIST                                                 
         BNZ   CKCNNO             ** ERROR - PRODUCTS EXIST                     
*                                                                               
CKCNYES  CR    RB,RB              SET CONDITION CODES                           
         B     CKCNX                                                            
CKCNNO   LTR   RB,RB                                                            
CKCNX    MVC   AIO,AIO1           RESTORE OUTPUT IO                             
         J     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*    CHECK FOR DIV/DST DEF RECS                                       *         
***********************************************************************         
DODEF    NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),HALF2         AGY/MED                                   
         MVC   KEY+3(2),SVCLTKEY+2    CLT                                       
         GOTO1 HIGH                                                             
         B     DODEF10                                                          
DODEF5   GOTO1 SEQ                                                              
DODEF10  CLC   KEYSAVE(5),KEY                                                   
         BNE   DODEF15                                                          
         MVI   KEY+13,X'DD'                                                     
         GOTO1 WRITE                                                            
         B     DODEF5                                                           
*                                                                               
DODEF15  XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),HALF2        AGENCY/MEDIA                               
         MVC   KEY+3(2),SVCLTKEY+2   CLIENT                                     
         GOTO1 HIGH                                                             
         B     DODEF25                                                          
DODEF20  GOTO1 SEQ                                                              
DODEF25  CLC   KEY(5),KEYSAVE                                                   
         BNE   DODEF40                                                          
         CLI   KEY+8,C'G'                                                       
         BL    DODEF35                                                          
*                                  SEE IF NEED TO ADD TO SCHEME TABLE           
         LA    R5,ELEM                                                          
DODEF28  CLI   0(R5),0                                                          
         BE    DODEF30                                                          
         CLC   0(1,R5),KEY+8                                                    
         BE    DODEF35             FOUND                                        
         LA    R5,1(R5)                                                         
         B     DODEF28                                                          
*                                                                               
DODEF30  MVC   0(1,R5),KEY+8       SAVE LIST OF SCHEMES                         
DODEF35  MVI   KEY+13,X'DD'                                                     
         GOTO1 WRITE                                                            
         B     DODEF20                                                          
*                                                                               
DODEF40  XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D03'         MKTGRPS ASSGNS                           
         MVC   KEY+2(1),HALF2          AGY/MED                                  
         MVC   KEY+3(2),SVCLTKEY+2     CLIENT                                   
         GOTO1 HIGH                                                             
         B     DODEF50                                                          
DODEF45  GOTO1 SEQ                                                              
DODEF50  CLC   KEY(11),KEYSAVE                                                  
         BNE   DODEF60                                                          
         MVI   KEY+13,X'DD'                                                     
         GOTO1 WRITE                                                            
         B     DODEF45                                                          
*                                                                               
DODEF60  LA    R5,ELEM                                                          
DODEF65  CLI   0(R5),0             END OF SCHEME LIST                           
         BE    DODEFX                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),HALF2          AGY/MED                                  
         MVC   KEY+3(2),SVCLTKEY+2     CLIENT                                   
         MVC   KEY+8(1),0(R5)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST FIND SCHEME                             
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         LA    R6,24(RE)           REC+24                                       
         MVI   ELCODE,X'02'                                                     
DODEF70  BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST FIND EXCEPTION ELEM                     
         CLC   2(3,R6),QCLT                                                     
         BNE   DODEF70                                                          
         GOTO1 RECUP,DMCB,(0,RE),0(R6),0                                        
         GOTO1 PUTREC                                                           
         LA    R5,1(R5)            NEXT SCHEME                                  
         B     DODEF65                                                          
DODEFX   J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CHKCTCLT: CHECKING FOR SPECIAL ZENITH CLIENTS.               *         
***********************************************************************         
CHKCTCLT NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING ZENRECD,R4                                                       
         MVI   ZENKCODE,ZENKCODQ                                                
         MVI   ZENKTYP,ZENCLTQ                                                  
         MVC   ZENKAGY,SVCTAGY                                                  
         MVC   ZENKCLT,QCLT                                                     
         BAS   RE,CHKCT                                                         
*        POINT TO FIRST UNP FIELD (CLIENT NAME, HOPEFULLY)                      
CHK10    BAS   RE,NEXTUF                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO              POINT TO RECORD                              
         MVI   5(R2),20            FORCE LENGTH                                 
         OI    6(R2),X'80'         FORCE XMT                                    
         LA    R4,ZENFIRST                                                      
         USING ZENELEM,R4                                                       
         MVC   8(20,R2),ZENCNAME   MOVE ZENITH NAME                             
         B     CHKX                                                             
         DROP  R4                                                               
*                                                                               
CHKX     MVC   AIO,AIO1                                                         
         J     XIT                                                              
*                                                                               
CHKCT    CLI   ACTNUM,ACTADD        TEST ACTION = ADD                           
         BNE   CHKX                 NO - FORGET IT                              
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,0(R6)                    
         CLC   0(25,R4),0(R6)                                                   
         BE    CHK10               USED TO BE BER RE!?  -SEAN                   
*                                                                               
         B     ERRCORD                                                          
*                                                                               
FNDUF    TM    1(R2),X'20'         TEST PROTECTED                               
         BCR   8,RE                NO = EXIT WITH CC EQ                         
NEXTUF   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
SPCN     NTR1  BASE=*,LABEL=*                                                   
         CLI   SVAPROF+7,C'C'      CANADIAN CLIENT                              
         BNE   SPCNX                                                            
         CLI   CLTMED,C'T'         AND MEDIA T                                  
         BNE   SPCNX                                                            
*                                                                               
         LA    R2,2                LOOP COUNTER                                 
         MVC   AIO,AIO1            POINT TO THE RECORD                          
         L     R6,AIO                                                           
         NI    1(R6),X'F0'                                                      
         OI    1(R6),X'03'         FOR MEDIA N                                  
*                                                                               
SPCN10   MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES THE RECORD EXIST?                       
         BE    SPCNCHA             YES, THEN CHANGE IT                          
*                                                                               
         MVC   KEY,KEYSAVE         ELSE, ADD IT                                 
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     SPCN20                                                           
*                                                                               
SPCNCHA  MVC   AIO,AIO3                                                         
         L     R1,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   0(13,R6),0(R1)      COPY THE RECORD, BUT DIFFERENT KEY           
         MVC   AIO,AIO1            WRITE THE RECORD TO FILE                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SPCN20   L     R6,AIO                                                           
         NI    1(R6),X'F0'                                                      
         OI    1(R6),X'08'         FOR MEDIA C                                  
         BCT   R2,SPCN10                                                        
*                                                                               
SPCNX    MVC   0(13,R6),SVCLTKEY                                                
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* IF USER PUT A * OR L IN THE MEDIA FIELD THEN THAT CLIENT SHOULD     *         
* NOT EXIST FOR MEDIAS R OR X                                         *         
***********************************************************************         
CHKRX    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVMED,C'*'          WANT TO ADD MEDIAS R AND X?                  
         BE    *+12                YES                                          
         CLI   SVMED,C'L'          ADD MEDIA R?                                 
         BNE   CKRXX               NOPE, DONE                                   
         CLI   CLTMED,C'T'         MEDIA T?                                     
         BNE   CKRXX               NO                                           
*                                                                               
         LA    R2,CLTMEDH          IN CASE OF ERROR                             
         LA    R4,2                ADD MEDIA R AND X                            
         LA    R6,KEY                                                           
         MVC   0(13,R6),SVCLTKEY                                                
         NI    1(R6),X'F0'                                                      
         OI    1(R6),X'02'         FOR MEDIA R                                  
*                                                                               
CKRX10   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES THE RECORD EXIST?                       
         BE    ERRMEDRX            YES, ERROR                                   
*                                                                               
         MVC   0(13,R6),SVCLTKEY                                                
         NI    1(R6),X'F0'                                                      
         OI    1(R6),X'04'         FOR MEDIA X                                  
         BCT   R4,CKRX10                                                        
*                                                                               
CKRXX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* IF USER PUT A * IN MEDIA FIELD THEN WE SHOULD ADD MEDIAS R AND X    *         
* FOR THAT CLIENT.  IF USER PUT AN L IN MEDIA FIELD THEN WE SHOULD    *         
* ADD MEDIA R FOR THAT CLIENT.                                        *         
* NOTE: NON-CANADIAN (THEY HAVE TO DO IT THE HARD WAY)                *         
***********************************************************************         
SPRX     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,2                ADD MEDIA R AND X                            
         CLI   SVMED,C'*'          ADD MEDIAS R AND X?                          
         BE    SPRX05              YES                                          
         CLI   SVMED,C'L'          ADD MEDIA R?                                 
         BNE   SPRXX               NOPE, DONE                                   
         LA    R4,1                JUST ADD MEDIA R                             
SPRX05   CLI   CLTMED,C'T'         MEDIA T?                                     
         BNE   SPRXX               NO                                           
*                                                                               
         LA    R2,CLTMEDH          IN CASE OF ERROR                             
         MVC   AIO,AIO1            POINT TO THE RECORD                          
         L     R6,AIO                                                           
         NI    1(R6),X'F0'                                                      
         OI    1(R6),X'02'         FOR MEDIA R                                  
*                                                                               
SPRX10   MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES THE RECORD EXIST?                       
         BE    ERRMEDRX            YES, ERROR                                   
*                                                                               
         MVC   KEY,KEYSAVE         ELSE, ADD IT                                 
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         NI    1(R6),X'F0'                                                      
         OI    1(R6),X'04'         FOR MEDIA X                                  
         BCT   R4,SPRX10                                                        
*                                                                               
SPRXX    MVC   0(13,R6),SVCLTKEY                                                
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INIT SOME CLIENT2 FILEDS AS PER MEL                                 *         
***********************************************************************         
INITCL2  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AIO,AIO1            POINT TO THE RECORD                          
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         MVI   CPROF+1,C'0'        Lock Box Number                              
         MVI   CPROF+4,C'0'        Bill Formula Control                         
         MVI   CPROF+5,C'0'        Bill Estimate Control                        
         MVI   CEXTRA+1,C'0'       Canadian Network Tax                         
         MVI   CEXTRA+4,C'0'       Campaigns                                    
         MVI   CEXTRA+7,C'N'       MKGDS in Missed Month                        
         MVI   CCPPRS,C'N'         CPPRS                                        
         MVC   CRFPGRP,SPACES      T/A RFP Group                                
         DROP  R6                                                               
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD OR DELETE OF PASSIVE OFFICE POINTERS                                      
***********************************************************************         
OFCPTR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
OFCPTR0  CLC   SVCOFFC,COFFICE     TEST OFFICE CHANGED                          
         BE    OFCPTRX             NO                                           
********                                                                        
* DELETE OLD OFFICE (IF ANY)                                                    
********                                                                        
         CLI   SVCOFFC,C' '                                                     
         BNH   OFCPTR10                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D80'                                                  
         MVC   KEY+2(1),SVCLTKEY+1    A/M                                       
         MVC   KEY+9(1),SVCOFFC                                                 
         MVC   KEY+11(2),SVCLTKEY+2   CLT                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR2                                                          
         OI    KEY+13,X'80'      MARK RECORD                                    
         GOTO1 WRITE                                                            
*                                                                               
OFCPTR2  CLI   SVAPROF+7,C'C'    CANADIAN AGENCY                                
         BNE   OFCPTR10                                                         
         CLI   CLTMED,C'T'       TV ONLY                                        
         BNE   OFCPTR10                                                         
********                                                                        
* IF IT'S A CANADIAN CLIENT, NEED TO DELETE/ADD FOR MEDIA N (X'03')             
********                                                                        
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR4                                                          
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
********                                                                        
* IF IT'S A CANADIAN CLIENT, NEED TO DELETE/ADD FOR MEDIA C (X'08')             
********                                                                        
OFCPTR4  NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OFCPTR10                                                         
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE                                                            
********                                                                        
* NOW ADD NEW (OR UNDELETE)                                                     
********                                                                        
OFCPTR10 CLI   COFFICE,C' '        TEST NEW OFFICE PRESENT                      
         BNH   OFCPTRX                                                          
         MVC   KEY(20),SVCLTKEY                                                 
         GOTO1 =A(ADDOFC),RR=RELO                                               
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   OFCPTRX                                                          
         CLI   QMED,C'T'                                                        
         BNE   OFCPTRX                                                          
********                                                                        
* DO MEDIA N                                                                    
********                                                                        
         MVC   KEY(20),SVCLTKEY                                                 
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                READ THE DIRECTORY POINTER                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(ADDOFC),RR=RELO                                               
********                                                                        
* DO MEDIA C                                                                    
********                                                                        
         MVC   KEY(20),SVCLTKEY                                                 
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                READ THE DIRECTORY POINTER                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(ADDOFC),RR=RELO                                               
*                                                                               
OFCPTRX  NI    DMINBTS,X'F7'                                                    
         J     XIT                                                              
***********************************************************************         
ADDOFC   NTR1  BASE=*,LABEL=*                                                   
*        MVC   KEY,SVCLTKEY                                                     
*        GOTO1 HIGH                                                             
         MVC   WORK(20),KEY        SAVE CLTHDR KEY AND DISK ADDRESS             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D80'                                                  
         MVC   KEY+2(1),WORK+1     A/M                                          
         MVC   KEY+9(1),COFFICE                                                 
         MVC   KEY+11(2),WORK+2    CLT                                          
         MVC   KEY+14(4),WORK+14   SET DISK ADDRESS                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ADDOFC10                                                         
         NI    KEY+13,X'7F'                                                     
         GOTO1 WRITE                                                            
         B     ADDOFCX                                                          
*                                                                               
ADDOFC10 MVC   KEY,KEYSAVE     RESTORE KEY                                      
         GOTO1 ADD                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ADDOFCX  DS    0H                                                               
         DROP  R6                                                               
         J     XIT                                                              
VALCOS2  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AIO                                                           
         USING CLTHDR,R2                                                        
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VC2XIT                                                           
*                                                                               
         CLC   =C'WI',AGENCY                                                    
         BE    VC210                                                            
         CLC   =C'WT',AGENCY                                                    
         BE    VC210                                                            
         CLC   =C'WJ',AGENCY                                                    
         BE    VC210                                                            
*                                                                               
         XC    CC2CONV,CC2CONV                                                  
         B     VC2XIT                                                           
*                                                                               
VC210    DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         MVC   SVDATE,FADATEB                                                   
         DROP  R1                                                               
         GOTO1 DATCON,DMCB,(3,SVDATE),(2,CC2CONV)                               
         OI    COPT3,COP3CONV                                                   
VC2XIT   DS    0H                                                               
         J     XIT                                                              
         DROP  R2                                                               
***********************************************************************         
*        CALL PSTVAL TO CHANGE/DISPLAY PST/MAIN PST                   *         
*        INPUT : R2 = SCREEN HEADER                                   *         
*              : R3 = PST IN RECORD                                   *         
*              : R5 = LENGTH OF PST ON SCREEN OR IN THE RECORD        *         
***********************************************************************         
PSTVAL   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,ELEM             R4 = ELEMENT                                 
         USING PSTBLKD,R4          PST BLOCK DSECT                              
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVC   PSTACT,BYTE         ACTION = VALIDATE OR DISPLAY                 
         ST    R3,PSTADIN          INPUT ADDRESS IS PST IN RECORD               
         CLI   BYTE,PSTFMTQ        ACTION = FORMAT (DISPLAY)?                   
         BE    *+8                 YES                                          
         ST    R2,PSTADIN          NO - INPUT ADDRESS IS PST ON SCREEN          
         XC    PSTOUT,PSTOUT       CLEAR PST OUTPUT BLOCK                       
         LA    R1,PSTOUT           PST OUTPUT BLOCK                             
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
*                                                                               
         MVI   DMCB+7,QPSTVAL      CALL PSTVAL TO DISPLAY PST CODES             
         XC    DMCB(7),DMCB        CLEAR DMCB                                   
         MVC   DMCB+4(3),=X'D9000A'                                             
         GOTO1 CALLOV,DMCB         CALL OVERLAY FOR A(PSTVAL)                   
         CLI   4(R1),X'FF'         ANY ERRORS?                                  
         BNE   *+6                 NO                                           
         DC    H'0'                YES - DEATH                                  
         L     RF,DMCB             A(PSTVAL)                                    
         GOTO1 (RF),DMCB,(R4)      CALL PSTVAL                                  
*                                                                               
         LA    R1,8(R2)            MOVE TO SCREEN                               
         CLI   BYTE,PSTFMTQ        ACTION = FORMAT (DISPLAY)?                   
         BE    PSTVAL20            YES - DON'T CHECK FOR ERRORS                 
*                                                                               
         LR    R1,R3               MOVE TO RECORD                               
         CLI   PSTERR,0            ANY ERRORS RETURNED FROM PSTVAL?             
         JNE   ERRINV              YES - GIVE ERROR INVALID                     
*                                                                               
         CHI   R5,L'CMPST-1        ADD/CHANGE MAIN PST?                         
         BNE   PSTVAL20            NO                                           
*                                                                               
         LA    R1,PSTOUT           RETURNED FROM PSTVAL                         
         LA    R2,10               MAX 10 ENTRIES                               
*                                                                               
PSTVAL15 CLI   0(R1),0             HAVE PST ENTRY?                              
         BNE   PSTVAL16            YES                                          
         LA    R1,1(R1)            NO - BUMP POINTER                            
         BCT   R2,PSTVAL15         LOOP BACK AND CHECK NEXT ONE                 
         DC    H'0'                PSTVAL SHOULD HAVE RETURNED AN ERROR         
*                                                                               
PSTVAL16 MVC   1(1,R3),0(R1)       MOVE PST                                     
         LA    R2,PSTOUT           PST ENTRY                                    
         SR    R1,R2               GET INDEXED POSITION                         
         LA    R1,1(R1)            ADD ONE (POSTION 1 = 1 IN THIS CASE)         
         STC   R1,0(R3)            SAVE THE POSITION IN THE RECORD              
         J     XIT                 AND EXIT                                     
*                                                                               
PSTVAL20 EX    R5,*+8              DISPLAY = L'SCREEN | ADD/CHA = L'REC         
         B     *+10                BRANCH SO ASSEMBLER DOESN'T COMPLAIN         
         MVC   0(0,R1),PSTOUT      MOVE PST/MAIN PST TO SCREEN/RECORD           
         J     XIT                 EXIT BACK TO CALLER                          
         DROP  R4                                                               
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
OPTTABD  DSECT                                                                  
SCRDISD  DS    AL2                  DISPLACEMENT INTO THE SCREEN                
RECDISD  DS    AL2                  DISPLACEMENT INTO THE RECORD                
AVALD    DS    A                    ADDRESS OF VALIDATION ROUTINES              
ADISD    DS    A                    ADDRESS OF DISPLAY ROUTINES                 
OPTTABQ  EQU   *-OPTTABD                                                        
*                                                                               
SCANTABD DSECT                                                                  
SCANTF1L DS    CL1                 1ST FIELD LENGTH                             
SCANTF2L DS    CL1                 2ND FIELD LENGTH                             
SCANTF1V DS    CL1                 1ST FIELD BITWISE VALIDATION INFO            
SCANTF2V DS    CL1                 2ND FIELD BITWISE VALIDATION INFO            
SCANTF1B DS    CL4                 1ST FIELD BINARY VALUE                       
SCANTF2B DS    CL4                 2ND FIELD BINARY VALUE                       
SCANTF1  DS    CL10                1ST FIELD INPUT STRING                       
SCANTF2  DS    CL10                2ND FIELD INPUT STRING                       
SCANTABQ EQU   *-SCANTABD                                                       
*                                                                               
PRDD     DSECT                                                                  
PRDPRD   DS    0CL4                                                             
PRDAC    DS    CL3                 ALPHA PRODUCT CODE                           
PRDBSN   DS    XL1                 BINARY PRODUCT SEQUENCE NUMBER               
PRDQ     EQU   *-PRDD                                                           
*                                                                               
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ADVHDRD  DSECT                                                                  
       ++INCLUDE SPGENADV                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
HALF2    DS    H                                                                
QXIT     DS    X                   QUICK EXIT FLAG TO SKIP VALREC               
*                                                                               
FLAG1    DS    XL1                 AGYFLAG1                                     
ACCOFC   DS    XL1                 AGYOFC2                                      
ACCAGY   DS    XL24                AGYACCAG                                     
*                                                                               
SVCOFFC  DS    C                   BACKUP OFFICE NUMBER                         
SVCLTKEY DS    CL48                BACK UP KEY                                  
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
*                                                                               
PSTOUT   DS    CL64                PSTVAL OUTPUT FIELD                          
APSTVAL  DS    A                   A(PSTVAL)                                    
*                                                                               
SCANTAB  DS    CL32                SCANNER INPUT TABLE                          
*                                                                               
KEY1     DS    CL48                                                             
KEY2     DS    CL48                                                             
*                                                                               
SYSSW    DS    XL1                 SE NUM FOR SWITCH                            
POWCODE  DS    CL2                 ACCOUNT AGENCY OVERRIDE                      
ACCOFF   DS    CL2                 ACCOUNT OFFICE CODE                          
OFFLEN   DS    XL1                 ACCOUNT OFFICE LENGTH                        
COMPCD   DS    XL1                 AGENCY BINARY CODE                           
GTFACTB  DS    CL88                                                             
MYACCKEY DS    CL42                ACCOUNT REC KEY                              
CTKEY    DS    CL28                                                             
SVDATE   DS    XL3                                                              
CLTLASTC DS    X                                                                
*                                                                               
MYCLTCOD DS    CL3                 TMP CLT CODE(2) & PROD SEQ#(1)               
SVMED    DS    CL1                 SAVED CLIENT CODE                            
OFCBLK   DS    XL(OFCLENQ)                                                      
         EJECT                                                                  
***********************************************************************         
         PRINT  OFF                                                             
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM76D          MAINTENACE SCREEN                            
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE FAFACTS           FOR FACTSD IN VALACC                         
       ++INCLUDE CTGENFILE         FOR CTSYSD IN VALACC                         
*PREFIX=AC$                                                                     
       ++INCLUDE ACGENFILE         FOR CPYELD & OFFRECD IN VALACC               
*PREFIX=                                                                        
       ++INCLUDE DDOFFICED         FOR OFFICED                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY          AGENCY PROFILES                              
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
       ++INCLUDE DDPSTBLK          FOR PSTVAL                                   
       ++INCLUDE DDCOREQUS         FOR PSTVAL                                   
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SPGENGRP          FOR DELETING CLIENTS                         
       ++INCLUDE SPGENNDEF         FOR DELETING CLIENTS                         
       ++INCLUDE GEGENBAG          BUYING AGENCY IDENTIFIER IN C/SEC            
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052SPSFM06   12/09/19'                                      
         END                                                                    
