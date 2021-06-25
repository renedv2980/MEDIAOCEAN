*          DATA SET DMSNAPPER  AT LEVEL 006 AS OF 02/26/03                      
*PHASE DMSNAPA                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE LOGIO                                                                  
         TITLE 'DMSTAMPER - SET CPUID/DSPACE IN REC ZERO OF 1ST TRACK'          
         PRINT NOGEN                                                            
STAMPER  CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,DMSNAP,WORK=A(SNAPWORK)                                        
*                                                                               
INIT     L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(40),=C'XXX - SET DATASPACE FLAG FOR SNAP COPY  '           
         MVC   MID1(15),=C'PARAMETER CARDS'                                     
         MVC   MID2(15),=C'---------------'                                     
*                                                                               
INIT1    L     R1,X'10'            GET CPU ID                                   
         L     R1,X'C4'(R1)        R1=A(SMCA) FROM CVT                          
         MVC   CPUID,X'10'(R1)     CPU IS C'XXXX' FROM SMCASID                  
         MVC   TITLE(3),CPUID                                                   
         EJECT                                                                  
INPUT    GOTO1 =V(CARDS),P1,C,=C'RE00'                                          
         CLC   C(2),=C'/*'                                                      
         BE    SNAPPERX                                                         
*                                                                               
INPUT1   CLC   C(10),SPACES        ALLOW BLANK AND ASTERISK CARDS               
         BE    *+12                                                             
         CLI   C,C'*'                                                           
         BNE   INPUT2                                                           
         MVC   P(40),C             PRINT COMMENT CARDS                          
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
*                                                                               
INPUT2   CLC   =C'DDSIO=',C        OVERRIDE DDSIO                               
         BNE   INPUT2X                                                          
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),C+6                                                      
         MVC   P(30),C                                                          
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
INPUT2X  EQU   *                                                                
*                                                                               
INPUT4   CLC   =C'DSPACE=',C       SET DATA SPACE ID                            
         BNE   INPUT4X                                                          
         MVC   P(30),C                                                          
         LA    R1,C+7                                                           
         BAS   RE,VALDSP           VALIDATE DATA SPACE ID CHR                   
         BNE   INPUT4E                                                          
         CLI   DSPID,C' '          ONLY ONE DATA SPACE ID ALLOWED               
         BNE   INPUT4E                                                          
         MVC   DSPID,1(RF)         SAVE INPUT VALUE FROM TABLE                  
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
INPUT4E  MVC   P+30(32),=CL32'**ERROR** INVALID DATA SPACE ID'                  
         GOTO1 =V(LOGIO),DUB,1,(62,P)                                           
         GOTO1 =V(PRINTER)                                                      
         B     SNAPPERX                                                         
INPUT4X  EQU   *                                                                
*                                                                               
INPUT5   CLC   =C'SNAP=',C        SNAP=Y/N TO TURN OFF/ON                       
         BNE   INPUT5X                                                          
         MVC   P(30),C                                                          
         CLI   C+5,C'Y'                                                         
         BE    *+12                                                             
         CLI   C+5,C'N'                                                         
         BNE   INPUT5E                                                          
         CLI   SNAP,C' '          ONLY ONE SNAP= CARD ALLOWED                   
         BNE   INPUT5E                                                          
         MVC   SNAP,C+5                                                         
         MVC   P(30),C                                                          
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
INPUT5E  MVC   P+30(32),=CL32'**ERROR** INVALID SNAP= CARD'                     
         GOTO1 =V(LOGIO),DUB,1,(62,P)                                           
         GOTO1 =V(PRINTER)                                                      
         B     SNAPPERX                                                         
INPUT5X  EQU   *                                                                
*                                                                               
INPUT6   CLC   =C'FORCE=',C        FORCE=Y TO OVERRIDE                          
         BE    INPUT6A                                                          
         CLC   =C'OVERRIDE',C      OVERRIDE SAME AS FORCE=Y                     
         BNE   INPUT6X                                                          
         MVI   FORCE,C'Y'                                                       
         B     INPUT6B                                                          
INPUT6A  MVC   P(30),C                                                          
         CLI   C+6,C'Y'                                                         
         BE    *+12                                                             
         CLI   C+6,C'N'                                                         
         BNE   INPUT6E                                                          
         MVC   FORCE,C+6                                                        
INPUT6B  MVC   P(30),C                                                          
         GOTO1 =V(PRINTER)                                                      
         OI    PRINTF,X'01'                                                     
         B     INPUT                                                            
INPUT6E  MVC   P+30(32),=CL32'**ERROR** INVALID FORCE= CARD'                    
         GOTO1 =V(LOGIO),DUB,1,(62,P)                                           
         GOTO1 =V(PRINTER)                                                      
         B     SNAPPERX                                                         
INPUT6X  EQU   *                                                                
*                                                                               
INPUTA   CLI   MID1,C'I'           TEST FIRST FILE NAME CARD                    
         BE    MAIN                                                             
         MVC   MID1(25),=C'INPUT     SYS   FILE     '                           
         MVC   MID2(25),=C'--------- ----- -------- '                           
         TM    PRINTF,X'01'                                                     
         BZ    INPUTB                                                           
         ZAP   LINE,=P'99'         NEW PAGE IF PRINTED CONTROL CARDS            
*                                                                               
INPUTB   CLI   DSPID,C' '          MUST HAVE DSPID= INPUT                       
         BE    INPUTBE                                                          
         BAS   RE,SETDSP           SET DATASPACE ID INTO SSB                    
         B     INPUTBX                                                          
INPUTBE  MVC   P+30(32),=CL32'**ERROR** MISSING DSPACE= CARD'                   
         GOTO1 =V(LOGIO),DUB,1,(32,P+30)                                        
         GOTO1 =V(PRINTER)                                                      
         B     SNAPPERX                                                         
INPUTBX  EQU   *                                                                
*                                                                               
INPUTC   CLI   SNAP,C' '           MUST HAVE SNAP= INPUT                        
         BNE   INPUTCX                                                          
INPUTCE  MVC   P+30(32),=CL32'**ERROR** MISSING SNAP= CARD'                     
         GOTO1 =V(LOGIO),DUB,1,(32,P+30)                                        
         GOTO1 =V(PRINTER)                                                      
         B     SNAPPERX                                                         
INPUTCX  EQU   *                                                                
*                                                                               
         B     MAIN                PROCESS FILENAME OR SYS=XXXX CARD            
*                                                                               
SNAPPERX XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*FILE CARD HAS FILE NAME OR SYSTEM CARD HAS SYS=XXXXX WITH SYSTEM NAME*         
***********************************************************************         
         SPACE 1                                                                
MAIN     GOTO1 =V(DMDDNAME),DMCB,=C'DDNAME',C                                   
         CLI   8(R1),0                                                          
         BE    MAIN1                                                            
         MVC   P(9),C                                                           
         MVC   P+30(32),=CL32'**ERROR** INVALID FILE/SYSTEM'                    
         GOTO1 =V(LOGIO),DUB,1,(62,P)                                           
         GOTO1 =V(PRINTER)                                                      
         B     MAINX                                                            
*                                                                               
MAIN1    L     RE,DMCB+8           GET A(FILE INFO DATA)                        
         MVC   RETINFO(RETINFOX-RETINFO),0(RE)                                  
         CLC   C(4),=C'SYS='       TEST IF SYSTEM NAME INPUT                    
         BE    MAIN4                                                            
         L     R4,FILADTF          R4=A(DTF)                                    
         TM    DTFFLAG-DTF(R4),DTFSNAP                                          
         BO    MAIN2                                                            
         CLI   FORCE,C'Y'                                                       
         BE    MAIN2                                                            
         MVC   P+30(32),=CL32'**ERROR** FILE IS NOT SNAPPABLE'                  
         GOTO1 =V(LOGIO),DUB,1,(62,P)                                           
         GOTO1 =V(PRINTER)                                                      
         B     MAINX                                                            
*                                                                               
MAIN2    BAS   RE,SNAPPY           SINGLE FILE                                  
         MVC   P+00(9),C                                                        
         MVC   P+10(5),SENAME                                                   
         MVC   P+16(8),DDNAME                                                   
         MVC   P+25(1),SNAP                                                     
         MVC   P+27(1),RESULT                                                   
         GOTO1 =V(PRINTER)                                                      
         B     MAINX                                                            
*                                                                               
MAIN4    L     R3,SYSADR           POINT TO SYSLIST FOR SYSTEM                  
         SR    R5,R5                                                            
         IC    R5,3(R3)            GET NUMBER OF FILES                          
         LA    R3,4(R3)                                                         
MAIN5    L     R4,4(R3)            R4=A(DTF)                                    
         LA    R4,0(R4)                                                         
         TM    DTFFLAG-DTF(R4),DTFSNAP                                          
         B     MAIN7               *NOP* SHOULD BE BO                           
MAIN6    LA    R3,8(R3)            BUMP TO NEXT FILE                            
         BCT   R5,MAIN5                                                         
         B     MAINX                                                            
*                                                                               
MAIN7    MVC   FILNUM,3(R3)        SET FILE NUMBER                              
         MVC   FILFLAG1(2),0(R3)   SET FILE FLAGS                               
         ST    R4,FILADTF          SET A(DTF)                                   
         MVC   DDNAME,22(R4)       SET DDNAME                                   
         CLI   SENUM,X'0A'                                                      
         BE    MAIN8                                                            
         CLI   FILNUM,X'A0'        IGNORE CONTROL FILES IN SYSTEM               
         BL    MAIN8                                                            
         CLI   FILNUM,X'AF'                                                     
         BNH   MAIN6                                                            
*                                                                               
MAIN8    BAS   RE,SNAPPY                                                        
         MVC   P+00(9),C                                                        
         MVC   P+10(5),SENAME                                                   
         MVC   P+16(8),DDNAME                                                   
         MVC   P+25(1),SNAP                                                     
         MVC   P+27(1),RESULT                                                   
         GOTO1 =V(PRINTER)                                                      
         B     MAIN6                                                            
*                                                                               
MAINX    TM    ERRORF,X'80'        TEST CANCEL FLAG                             
         BO    SNAPPERX                                                         
         B     INPUT               BACK FOR NEXT INPUT CARD                     
         EJECT                                                                  
***********************************************************************         
*SUBROUTINES TO VALIDATE AND BIND WITH DATA SPACE                     *         
***********************************************************************         
         SPACE 1                                                                
VALDSP   LA    RF,DSPTAB           POINT TO TABLE OF DSPACE IDS                 
         SR    R0,R0                                                            
VALDSP1  CLI   0(RF),X'FF'         TEST END OF TABLE                            
         BNE   *+12                                                             
         LA    R0,1                                                             
         B     VALDSPX                                                          
VALDSP2  CLC   0(1,R1),0(RF)       IDENTIFY BY LAST CHR                         
         BE    VALDSPX                                                          
         LA    RF,L'DSPTAB(RF)                                                  
         B     VALDSP1                                                          
VALDSPX  LTR   R0,R0               EXIT WITH CC EQL IF VALID                    
         BR    RE                                                               
         SPACE 2                                                                
SETDSP   NTR1                      GET DATASPACE ALET AND PUT IN SSB            
         L     RE,=A(SSB)                                                       
         USING SSBD,RE                                                          
         MVC   SSODSPAC,DSPID      SET DATA SPACE ID CHR                        
*                                                                               
         LA    R0,21               SET UP FRED'S SVC FOR DATA SPACE             
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK(28),WORK                                                    
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),=C'DMG DATAMGRX'                                      
         MVC   WORK+7(1),DSPID                                                  
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OC    WORK+24(4),WORK+24                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,=A(SSB)          SET ALET IN SSB                              
         USING SSBD,RE                                                          
         MVC   SSOALET,WORK+24                                                  
         DROP  RE                                                               
*                                                                               
SETDSPX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*ROUTINE TO FIND FILE DATA IN DATASPACE AND TURN ON/OFF SNAP BIT      *         
***********************************************************************         
         SPACE 1                                                                
SNAPPY   NTR1                                                                   
         SAC   512                 SET ACCESS REGISTER MODE                     
         LAM   R0,RF,ARZERO                                                     
         MVI   RESULT,C'0'         SET RETURN CODE                              
         L     RE,=A(SSB)                                                       
         USING SSBD,RE                                                          
         OC    SSOALET,SSOALET     EXIT IF NO ALET                              
         BZ    SNAPPYX                                                          
         LAM   R2,R2,SSOALET                                                    
         SR    R2,R2                                                            
         DROP  RE                                                               
*                                                                               
SNAPPY1  SR    RF,RF               LOCATE DATASPACE INFO FOR SENUM              
         IC    RF,SENUM                                                         
         SLL   RF,6                                                             
         AR    R2,RF               R2=A(SE ENTRY HEADER IN DSPACE)              
         USING DMSPACED,R2                                                      
         MVC   WRSCHDR,0(R2)       COPY RESOURCE HEADER INTO W/S                
         MVI   RESULT,C'1'                                                      
         OC    DSPTFRST+1(3),DSPTFRST+1                                         
         BZ    SNAPPYX                                                          
         L     R2,DSPTFRST                                                      
         N     R2,=X'00FFFFFF'     GET DISPLACEMENT TO SE ENTRY                 
         USING DMSYSHDR,R2                                                      
         MVC   WSYSHDR,0(R2)       COPY SYSTEM HEADER INTO W/S                  
*                                                                               
SNAPPY2  MVI   RESULT,C'2'         TEST THE SE NUM MATCHES                      
         CLC   DSYSENUM+1(1),SENUM                                              
         BNE   SNAPPYX                                                          
         MVI   RESULT,C'3'                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DSYFILES       GET NUMBER OF FILES                          
         LA    R2,DSYDATA                                                       
         USING DSFILHDR,R2                                                      
SNAPPY3  CLC   DSFILNUM,FILNUM     FIND THE CORRECT FILE ENTRY                  
         BE    SNAPPY4                                                          
         LA    R2,32(,R2)                                                       
         BCT   R0,SNAPPY3                                                       
         B     SNAPPYX                                                          
*                                                                               
SNAPPY4  MVC   WFILHDR,0(R2)       COPY THE FILE HEADER INTO W/S                
         MVI   RESULT,C'N'         SET FILE WAS NOT SNAPPED                     
         TM    DSFILFLG,X'80'                                                   
         BZ    *+8                                                              
         MVI   RESULT,C'Y'         SET FILE WAS SNAPPED                         
*                                                                               
SNAPPY5  NI    DSFILFLG,255-X'80'  TURN OFF FILE IS SNAPPED                     
         CLI   SNAP,C'Y'                                                        
         BNE   *+8                                                              
         OI    DSFILFLG,X'80'      SET FILE IS SNAPPED                          
*                                                                               
SNAPPYX  CLI   RESULT,C'0'         EXIT WITH CC LOW IF OK                       
         SAC   0                                                                
         XIT1                                                                   
         EJECT                                                                  
*DSPACE TABLE CONTAINS ONE CHR VALUE THAT IS IN REC0+7(1)                       
*                                                                               
*&&US                                                                           
DSPTAB   DS    0CL2                                                             
         DC    C'A',C'A'           ADV                                          
         DC    C'R',C'R'           REP                                          
         DC    C'T',C'T'           TEST                                         
DSPTABX  DC    X'FFFF'                                                          
*&&                                                                             
*&&UK                                                                           
DSPTAB   DS    0CL2                                                             
         DC    C'A',C'A'           ADV                                          
         DC    C'T',C'T'           TEST                                         
DSPTABX  DC    X'FFFF'                                                          
*&&                                                                             
         EJECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
FILEDD   DC    CL8' '                                                           
*                                                                               
UNKNOWN  DC    CL4'????'                                                        
NONE     DC    CL4'0000'                                                        
NOCHANGE DC    CL4'....'                                                        
*                                                                               
CPUID    DC    CL4' '              CPU ID                                       
*                                                                               
DSPID    DC    C' '                SET BY DSPACE=                               
SNAP     DC    C' '                SET BY SNAP=                                 
FORCE    DC    C'N'                SET BY FORCE=                                
PRINTF   DC    X'00'                                                            
ERRORF   DC    X'00'                                                            
RESULT   DC    X'00'                                                            
RESERR   DC    X'00'                                                            
*                                                                               
C        DC    CL80' '                                                          
MSG      DC    CL64' '                                                          
WORK     DC    CL64' '                                                          
*                                                                               
RETINFO  DS    0F                  START OF DMDDNAME RETURNED INFO              
*                                                                               
FILEINFO DS    0XL16               RETURNED FILE INFO                           
         DS    X                   RESERVED                                     
SENUM    DS    X                   SE NUMBER                                    
SENAME   DS    CL5                 SE SHORT NAME                                
SEFLAG1  DS    X                   SE FLAGS                                     
FILNUM   DS    X                   FILE NUMBER                                  
FILFLAG1 DS    X                   FILE FLAGS                                   
FILFLAG2 DS    X                   FILE FLAGS                                   
         DS    X                   N/D                                          
FILADTF  DS    AL4                 FILE A(DTF)                                  
*                                                                               
DSNINFO  DS    0XL4                                                             
DDNDSP   DS    X                   DISP TO DDNAME SIGNIFICANT CHR(S)            
DSNDSP   DS    X                   DISP TO DSN SIGNIFICANT CHR(S)               
         DS    XL2                 N/D                                          
*                                                                               
DDNAME   DS    CL8                 FILE DD NAME                                 
DDSDSN   DS    CL20                DATA SET NAME FROM DDS DYNDD TABLE           
*                                                                               
SYSADR   DS    AL4                 A(SYSFLES ENTRY FOR SYSTEM)                  
SYSNUM   DS    XL1                 LOGICAL SYSTEM NUMBER (ACC=X'06')            
         DS    XL7                                                              
*                                                                               
RETINFOX EQU   *                   END OF DMDDNAME RETURNED INFO                
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
         DS    0D                                                               
ARZERO   DC    16F'0'                                                           
WRSCHDR  DC    XL64'00'            COPIES OF DATA SPACE ENTRIES                 
WSYSHDR  DC    XL64'00'                                                         
WFILHDR  DC    XL32'00'                                                         
         DS    0D                                                               
         DC    C'**SSB***'                                                      
SSB      DC    XL2'00',X'FF',XL5'00'                                            
         DC    XL248'00'                                                        
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'**UTL***'                                                      
UTL      DC    256X'00'                                                         
         SPACE 2                                                                
SNAPWORK DS    1000D                                                            
         SPACE 2                                                                
*DMGREQUS                                                                       
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
*FASSBOFF                                                                       
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
*DMSPACED                                                                       
*DMDSYSHDR                                                                      
       ++INCLUDE DMSPACED                                                       
         SPACE 2                                                                
       ++INCLUDE DMDSYSHDR                                                      
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DMSNAPPER 02/26/03'                                      
         END                                                                    
