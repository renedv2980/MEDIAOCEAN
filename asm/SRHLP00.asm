*          DATA SET SRHLP00    AT LEVEL 009 AS OF 05/01/02                      
*PHASE T10900A                                                                  
         TITLE 'SRHLP00 - HELP SYSTEM VERSION 2'                                
HV1      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 HV1WRKX-HV1WRKD,**$HLP**,CLEAR=YES,RR=R9                         
         USING HV1WRKD,RC                                                       
         ST    R9,RELO                                                          
         MVC   SRPARAS(SRPARAL),0(R1)                                           
         L     R9,SRPASYS                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,SRPATWA                                                       
         USING T109FFD,R8          R8=A(TWA)                                    
         L     R7,VSSB                                                          
         MVC   RECLEN,SSBTWAL-SSBD(R7)                                          
         L     R7,SRPAUTL                                                       
         USING UTLD,R7             R7=A(UTL)                                    
         L     R6,SRPATIA          A(TIA)                                       
         USING SRSD,R6             SPECIAL S/R SAVE PAGE                        
         L     RA,SRPACOM                                                       
         USING COMFACSD,RA                                                      
*                                                                               
H10      NI    HV1REQH+6,X'FF'-X'40'                                            
         CLC   HV1SRV+1(4),=C'HELP'         CHECK SERVICE CALL                  
         BNE   *+12                                                             
         MVI   SRVC,C'Y'                                                        
         OI    FLAG,GHNMSGQ        IF $HELP I PROVIDE MESSAGES                  
         EJECT                                                                  
***************************************                                         
*  CHECK TO SEE IF NEXT PAGE WANTED   *                                         
***************************************                                         
         SPACE 1                                                                
         MVC   LANGNUM,TLANG       GET LANGUAGE FROM UTL                        
         CLI   LANGNUM,X'00'       TEST ENGLISH                                 
         BNE   *+8                 NO                                           
         MVI   LANGNUM,X'02'       ALWAYS LOOK FOR NATIVE ENGLISH FIRST         
*                                                                               
         LA    R2,SRPAGENO         READ & LOCK SAVE PAGE                        
         SLL   R2,32-8                                                          
         ICM   R2,3,TNUM                                                        
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(R2),SRSD           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,SR$HELP                                                       
         USING SVEHELPD,R5                                                      
         SR    R1,R1                                                            
         ICM   R1,3,HSVTTRC        CHECK SAVED TRANSACTION COUNT                
         LA    R1,1(R1)                                                         
         CH    R1,TTRCNT                                                        
         BNE   HLPSTART                                                         
         CLI   HV1REQH+5,0         CHECK FOR ANOTHER REQUEST                    
         BNE   HLPSTART                                                         
         TM    HV1SRV+6,X'F0'                                                   
         BO    RESTART                                                          
RETURN   OI    FLAG,GHSAVEQ        SET RETURN FLAG                              
         B     SERVICE                                                          
*                                                                               
RESTART  MVC   MYKEY,HSVFKEY                                                    
         B     RESTART1                                                         
*                                                                               
HLPSTART CLI   SRVC,C'Y'           CHECK FOR SRVC CALL                          
         BNE   AUTO                                                             
         CLI   HV1REQH+5,0         TEST ANY REQUESTED INPUT                     
         BE    H60                                                              
         DROP  R5                                                               
         EJECT                                                                  
***************************************                                         
*   INPUT FOUND SO SCAN & BUILD KEY   *                                         
***************************************                                         
         SPACE 1                                                                
         XC    SCANBLK,SCANBLK     CLEAR SCANNER BLOCK                          
         LA    R2,SCANBLK                                                       
         L     R3,SRPACOM          A(COMFACS)                                   
         USING COMFACSD,R3                                                      
         GOTO1 CSCANNER,DMCB,HV1REQH,(8,(R2)),C',=, '                           
         DROP  R3                                                               
*                                                                               
         MVC   SCANLEN,DMCB+4      NUMBER OF SCANNER ENTRIES                    
         CLI   SCANLEN,0                                                        
         BE    BADFORMT                                                         
         CLI   SCANLEN,3                                                        
         BL    H20                                                              
         CLI   SCANLEN,4           TEST FOR SYS,PRG,SCR,FLD                     
         BNE   BADFORMT                                                         
         OI    FLAGS,X'80'         FLAG FULL KEY FROM PFK                       
*                                                                               
H20      MVI   SYSNUM,0                                                         
         GOTO1 CGETFACT,DMCB,0     GET A(FACTSD)                                
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         L     R3,FASYSLST         A(SYSTEM LIST)                               
         DROP  R1                                                               
*                                                                               
         USING SYSLSTD,R3                                                       
         LH    R4,0(R3)            LENGTH OF TABLE ENTRY                        
         L     R5,2(R3)            A(END OF TABLE)                              
         LA    R3,6(R3)            A(FIRST ENTRY)                               
         ZIC   R1,0(R2)            INPUT LENGTH                                 
         BCTR  R1,0                                                             
*                                                                               
H25      EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),SYSLNAME   TEST MATCH ON SYSTEM NAME                    
         BE    H30                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),SYSLSHRT   TEST MATCH ON SHORT SYSTEM NAME              
         BE    H30                                                              
         BXLE  R3,R4,H25           TRY NEXT TABLE ENTRY                         
         B     INVSYSNM            SYSTEM NAME NOT IN TABLE                     
H30      MVC   SYSNUM,SYSLNUM      SYSTEM NUMBER                                
         DROP  R3                                                               
         EJECT                                                                  
***************************************                                         
*    FIND MATCH ON PROGRAM NAME       *                                         
***************************************                                         
         SPACE 1                                                                
         MVI   PROGNUM,0           ASSUME PROGRAM 'ALL'                         
         LA    R2,32(R2)           NEXT SCANNER ENTRY                           
         CLI   0(R2),0             TEST PROGRAM NAME GIVEN                      
         BE    H50                                                              
*                                                                               
         L     R3,VSELIST          A(SELIST)                                    
         USING SELISTD,R3                                                       
         LH    R4,0(R3)            LENGTH OF TABLE ENTRY                        
         L     R5,2(R3)            A(END OF TABLE)                              
         LA    R3,6(R3)            A(FIRST ENTRY)                               
         CLC   SEOVSYS,SYSNUM      FIND ENTRY FOR THIS SYSTEM                   
         BE    *+10                                                             
         BXLE  R3,R4,*-10          TRY NEXT ENTRY                               
         DC    H'0'                                                             
*                                                                               
         L     R3,SEPGMS           A(PROGRAM NAME LIST)                         
         DROP  R3                                                               
*                                                                               
         USING PGMLSTD,R3                                                       
         LH    R4,0(R3)            LENGTH OF TABLE ENTRY                        
         L     R5,2(R3)            A(END OF TABLE)                              
         LA    R3,6(R3)            A(FIRST ENTRY)                               
         ZIC   R1,0(R2)            INPUT LENGTH                                 
         BCTR  R1,0                                                             
*                                                                               
H40      EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R2),PGMNAME    TEST MATCH ON PROGRAM NAME                   
         BE    *+12                                                             
         BXLE  R3,R4,H40           TRY NEXT TABLE ENTRY                         
         B     INVPRGNM            PROGRAM NAME NOT IN TABLE                    
         MVC   PROGNUM,PGMNUM      SAVE PROGRAM NUMBER                          
         DROP  R3                                                               
H50      CLI   SCANLEN,4                                                        
         BNE   RESTART1                                                         
         MVI   SCRNNUM,0                                                        
         LA    R2,32(R2)           NEXT SCANNER ENTRY                           
         CLI   0(R2),2             TEST SCREEN GIVEN                            
         BNE   BADFORMT                                                         
         MVC   HALF,12(R2)                                                      
         GOTO1 CHEXIN,DMCB,HALF,SCRNNUM,2                                       
         LA    R2,32(R2)           NEXT SCANNER ENTRY                           
         TM    2(R2),X'80'         TEST NUMERIC                                 
         BNO   BADFORMT                                                         
         MVC   FIELDNUM,7(R2)      EXTRACT NUMERIC VALUE                        
         B     RESTART1                                                         
         EJECT                                                                  
***************************************                                         
*  NO INPUT FOUND SO $HELP THIS PROG  *                                         
***************************************                                         
         SPACE 1                                                                
H60      MVI   FIELDNUM,0                                                       
         MVC   SYSNUM,TOVSYS                                                    
         MVC   PROGNUM,TPRG                                                     
RESTART1 OI    FLAG,X'20'          USE THIS KEY                                 
         MVI   PAGENUM,1                                                        
         LA    R1,HV1SRVH                                                       
         ST    R1,AFLD                                                          
         MVI   AFLD,3                                                           
         XC    HV1REQ,HV1REQ                                                    
         CLI   HV1SRV+5,C','       TEST FOR $HELP,99 (PAGE NUMBER)              
         BNE   SERVICE                                                          
         ZIC   RE,HV1SRVH+5                                                     
         SH    RE,=H'7'            EX L'99                                      
         BM    SERVICE             IGNORE IF NOTHING BEYOND '$HELP,'            
         MVI   WORK,X'F0'          TEST FOR NUMERIC                             
         MVC   WORK+1(9),WORK                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),HV1SRV+6                                                 
         CLC   WORK(5),WORK+5                                                   
         BNE   SERVICE             IGNORE UNLESS NUMERIC                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,HV1SRV+6(0)                                                  
         CVB   RE,DUB                                                           
         STC   RE,PAGENUM          OVERRIDE DEFAULT PAGE                        
         B     SERVICE                                                          
         EJECT                                                                  
***************************************                                         
*   WE HAVE ENTERED VIA A PF1 OR ?    *                                         
***************************************                                         
         SPACE 1                                                                
AUTO     LA    R1,HV1SRVH          FIRST FIELD                                  
         SR    R0,R0                                                            
AUTO1    ICM   R0,1,0(R1)          BUMP TO NEXT FIELD                           
         BZ    AUTOX                                                            
         AR    R1,R0                                                            
         TM    1(R1),X'20'         TEST PROTECTED                               
         BO    AUTO1                                                            
         TM    4(R1),X'80'         TEST IF INPUT THIS TIME                      
         BNO   AUTO1                                                            
         NI    4(R1),X'7F'         SET NOT INPUT THIS TIME                      
         OI    6(R1),X'01'         BUT MODIFIED SO INPUT NEXT TIME              
         B     AUTO1                                                            
*                                                                               
AUTOX    XC    AFLD,AFLD                                                        
*&&US*&& MVI   AFLD,C'C'           US HAVE FULL PANELS                          
         L     R5,SRPATIOB         TRANSLATOR I/O BLOCK                         
         USING TIOBD,R5                                                         
         MVC   HELPDISP,TIOBHELP   DISPLACEMENT TO HELP REQUEST FIELD           
         MVC   PFKEY,TIOBAID       PFKEY NUMBER OR ZERO (ENTER)                 
         DROP  R5                                                               
*                                                                               
         CLI   PFKEY,12            HIGHEST PF KEY FOR NOW                       
         BNH   H65                                                              
         ZIC   R1,PFKEY                                                         
         SH    R1,=H'12'           EQUATE HIGH PF KEYS TO LOWER VALUES          
         STC   R1,PFKEY                                                         
*                                                                               
H65      LR    R2,R8               BEGINNING OF TWA                             
         AH    R2,HELPDISP         A(HELP FIELD HEADER)                         
         ST    R2,QFLD                                                          
         ZIC   R1,0(R2)            LENGTH OF FIELD                              
         LTR   R1,R1               TEST FIELD LENGTH ZERO                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BO    *+6                                                              
         DC    H'0'                                                             
         SH    R1,=H'8'            SUBTRACT LENGTH OF HEADER                    
         LA    R2,0(R1,R2)         A(HEADER EXTENSION)                          
         MVC   FIELDNUM,0(R2)      ID NUMBER                                    
         EJECT                                                                  
***************************************                                         
*   CALL GETHELP & SAVE ITS RETURN    *                                         
***************************************                                         
         SPACE 1                                                                
SERVICE  LA    R5,SR$HELP                                                       
         USING SVEHELPD,R5                                                      
         OI    FLAG,GHMKEYQ                                                     
         OI    FLAG,GHCLSQ                                                      
*&&US                                                                           
         CLI   TPRG,RLPQ           TEST RLP PROGRAM                             
         BNE   *+12                                                             
         MVI   PROGNUM,RFPQ        SET PROGRAM TO RFP                           
         MVI   SYSNUM,10           SET SYSTEM TO CONTROL                        
*&&                                                                             
         GOTO1 CGETHELP,DMCB,(FLAG,MYKEY),QFLD,AFLD,0,SR$HELP                   
         NI    TSTAT2,255-TSTATHUN DISABLE UNPROT BIT                           
         TM    16(R1),GHSAVEQ      MORE ?                                       
         BNO   CHKERR                                                           
         TM    16(R1),GHCLSQ                                                    
         BO    RECALL                                                           
         CLI   SRVC,C'Y'                                                        
         BNE   SAVE1                                                            
RECALL   MVC   HV1SRV(17),=C'=HELP            '                                 
         OI    HV1SRVH+6,X'01'                                                  
*                                                                               
SAVE1    MVC   HSVSRVC,SRVC        SAVE SERVICE FLAG                            
         LA    R2,SRPAGENO         WRITE SAVE PAGE                              
         SLL   R2,32-8                                                          
         ICM   R2,3,TNUM                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(R2),SRSD                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     MOREHELP                                                         
*                                                                               
CHKERR   TM    16(R1),GHNONEQ      TEST FOR NO HELP                             
         BO    NOHELP                                                           
         TM    16(R1),GHCTRLQ      TEST FOR NO SYSTEM                           
         BO    XIT                                                              
*                                                                               
         CLI   SRVC,C'Y'           IS THIS A $HELP CALL                         
         BE    *+12                                                             
         TM    16(R1),GHCLSQ       WAS SCREEN CLEARED                           
         BNO   LASTHELP                                                         
         MVC   HV1SRV(17),=C'=RE              '                                 
         OI    HV1SRVH+6,X'01'                                                  
         B     LASTHELP            MUST HAVE BEEN LAST PANEL                    
*                                                                               
HV1X     CLI   SRVC,C'Y'           IS THIS A $HELP CALL                         
         BNE   XIT                                                              
         LA    R1,DMCB             GET MESSAGE FOR LINE 1                       
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,MSGNUM                                                   
         MVC   GTMTYP,MSGTYPE                                                   
         MVI   GTMSYS,1            FORCE SERVICE SYSTEM                         
HV1X1    L     RF,CGETTXT                                                       
         BASR  RE,RF                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
LASTHELP MVC   MSGNUM,=AL2(01)     NO MORE HELP                                 
         B     HV1INFX                                                          
*                                                                               
MOREHELP MVC   MSGNUM,=AL2(03)     HELP DISPLAYED HIT ENTER FOR MORE            
         B     HV1INFX                                                          
*                                                                               
NOHELP   MVC   MSGNUM,=AL2(05)     UNAVAILABLE FOR THIS SYSTEM/PROGRAM          
         B     HV1INFX                                                          
*                                                                               
BADFORMT MVC   MSGNUM,=AL2(06)     ENTER SYSTEM,PROGRAM                         
*                                                                               
HV1INFX  MVI   TSVCREQ+1,X'0B'                                                  
         OI    HV1SRVH+6,X'40'                                                  
         MVI   MSGTYPE,GTMINF      MESSAGE TYPE INFO                            
         B     HV1X                                                             
         SPACE 1                                                                
INVSYSNM MVC   MSGNUM,=AL2(32)     INVALID SYSTEM NAME                          
         B     HV1ERRX                                                          
*                                                                               
INVPRGNM MVC   MSGNUM,=AL2(33)     INVALID PROGRAM NAME                         
         B     HV1ERRX                                                          
*                                                                               
INVPAGE  MVC   MSGNUM,=AL2(03)     INVALID NUMBER                               
*                                                                               
HV1ERRX  MVI   TSVCREQ+1,X'0B'                                                  
         OI    HV1REQH+6,X'40'                                                  
         MVI   MSGTYPE,GTMERR      MESSAGE TYPE IS ERROR                        
         B     HV1X                                                             
         EJECT                                                                  
*&&US                                                                           
RLPQ     EQU   X'2D'               PROGRAM NUMBER FOR RFP                       
RFPQ     EQU   X'2F'               PROGRAM NUMBER FOR RLP                       
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
HV1WRKD  DSECT                                                                  
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAGS    DS    X                                                                
LNK      DS    A                                                                
QFLD     DS    A                                                                
AFLD     DS    A                                                                
TXT      DS    A                                                                
WORK     DS    XL64                                                             
RELO     DS    A                   RELOCATION FACTOR                            
HELPDISP DS    H                   DISPLACEMENT TO HELP REQUEST FIELD           
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
FLAG     DS    XL1                                                              
SCANBLK  DS    XL256               SCANNER BLOCK                                
SCANLEN  DS    X                   NUMBER OF SCANNER ENTRIES                    
PFKEY    DS    X                   PFKEY NUMBER OR ZERO                         
MSGNUM   DS    XL2                 GETTXT MESSAGE NO                            
MSGTYPE  DS    X                   GETTXT MESSAGE TYPE                          
SRVC     DS    C                                                                
*                                                                               
PARMS    DS    5F                                                               
GTHSYS   DS    1F                                                               
*                                                                               
MYKEY    DS    0XL10                                                            
SYSNUM   DS    X                   SYSTEM NUMBER                                
PROGNUM  DS    X                   PROGRAM NUMBER                               
SCRNNUM  DS    X                   SCREEN NUMBER                                
FIELDNUM DS    X                   FIELD NUMBER                                 
PAGENUM  DS    X                   PAGE NUMBER                                  
LANGNUM  DS    X                   LANGUAGE NUMBER (1'S COMPLEMENT)             
         DS    XL4                                                              
*                                                                               
SRPARAS  DS    0F                  SERVICE REQUEST PARAMETER LIST               
SRPASYS  DS    A                   A(SYSFACS)                                   
SRPATIA  DS    A                   A(TIA)                                       
SRPAUTL  DS    A                                                                
SRPACOM  DS    A                   A(COMFACS)                                   
SRPASEL  DS    A                   A(SELIST ENTRY)                              
SRPATWA  DS    A                   A(TWA)                                       
SRPAMAP  DS    A                   A(PHASE MAP)                                 
SRPATIOB DS    A                   A(TRANSLATOR I/O)                            
SRPARAL  EQU   *-SRPARAS                                                        
*                                                                               
HV1WRKX  EQU   *                                                                
*                        SAVE BLOCK FOR GETHELP                                 
SVEHELPD DSECT                                                                  
HSVIDENT DS    XL4                                                              
HSVTTRC  DS    XL2                 TRANSACTION COUNT                            
HSVFLG1  DS    XL1                 FLAGS                                        
HSVFLG2  DS    XL1                                                              
HSVDAKEY DS    XL4                 TTBR ADDR OF CURRENT REC                     
HSVSEQ   DS    XL1                 SEQ NO OF LAST LINE OUT                      
HSVMAXLN DS    XL1                 SAVE FROM PARMS                              
HSVOFFS  DS    XL1                 SAVE FROM PARMS                              
HSVQHDR  DS    XL2                 OFFSET FROM TWA                              
HSVAHDR  DS    XL2                 OFFSET FROM TWA                              
HSVFKEY  DS    XL10                FULL DIRECTORY KEY                           
HSVSRVC  DS    XL1                                                              
SPARE    DS    XL2                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE SRHLPFFD                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDGETHELPD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGETHELPD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SRHLP00   05/01/02'                                      
         END                                                                    
