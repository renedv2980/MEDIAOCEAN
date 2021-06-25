*          DATA SET ACCLB50    AT LEVEL 111 AS OF 10/27/11                      
*PHASE T62150A                                                                  
*INCLUDE TWABLD                                                                 
CLB50    TITLE '- PC COMMS CONTROLLER'                                          
CLB50    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB50**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,BORELO                                                        
         L     RC,ALINK                                                         
         USING LINKD,RC                                                         
         USING FALINKD,FALINK                                                   
         USING FAMSGD,FAMSGBLK                                                  
*                                                                               
         SRL   RF,32-8                                                          
         LA    RE,ROUTSN                                                        
         CR    RF,RE                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     INIT                INITIALIZE                                   
         B     VALTAB              VALIDATE MAP TABLE (FOR DEBUGGING)           
         B     SETMHEL             SET MAP HEADER ENTRY (SETS AMHEL)            
         B     SETMDEL             SET MAP DATA ENTRY (SETS AMDEL)              
         B     SNDHDR              SEND MAP HEADER ELEMENT CODE                 
         B     SNDDATA             SEND FIELD DATA                              
         B     SNDFLD              SEND ELEMENT FIELD                           
         B     SNDELEM             SEND ELEMENT DETAILS                         
         B     SNDREC              SEND RECORD ELEMENT DETAILS                  
ROUTSN   EQU   (*-ROUTS)/L'ROUTS                                                
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITERR  DS    0H                  RETURN ERROR TO FALINK                       
         MVC   FAMSGNO,FVMSGNO     COPY ACGENWORK ERROR DETAILS                 
         MVC   FAMSGTYP,FVOMTYP                                                 
         MVC   FAMSGSYS,FVOSYS                                                  
         MVC   FAMSGXTR(L'FVXTRA),FVXTRA                                        
         XR    RF,RF                                                            
         ICM   RF,1,FVINDX                                                      
         BZ    EXITERR2                                                         
         CVD   RF,BCDUB                                                         
         OI    BCDUB+L'BCDUB-1,X'0F'                                            
         MVC   BCWORK,BCSPACES                                                  
         MVI   BCWORK,C'('                                                      
         MVI   BCWORK+1,C'#'                                                    
         UNPK  BCWORK+2(3),BCDUB                                                
         MVI   BCWORK+5,C')'                                                    
         CLI   BCWORK+2,C'0'                                                    
         BNE   *+14                                                             
         MVC   BCWORK+2(L'BCWORK-3),BCWORK+3                                    
         B     *-14                                                             
         LA    R0,L'FVXTRA                                                      
         LA    RF,FAMSGXTR+L'FVXTRA-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         MVC   2(6,RF),BCWORK                                                   
EXITERR2 MVC   FAMSGPRM(L'FVPARMS),FVPARMS                                      
         OC    FAMSGNO,FAMSGNO     TEST MESSAGE SET                             
         BNZ   *+10                                                             
         MVC   FAMSGNO,=AL2(FVFNOTV) NO - SET TO INVALID INPUT FIELD            
         CLI   FAMSGNO,FF          TEST GENERAL MESSAGE                         
         BNE   EXITN                                                            
         MVI   FAMSGSYS,FF         SET GENERAL MESSAGE                          
         MVI   FAMSGNO,0                                                        
*                                                                               
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION CALL TO FALINK                                       *         
***********************************************************************         
         SPACE 1                                                                
INIT     DS    0H                                                               
         L     RC,4(RD)            EXTEND W/S FOR LINKD                         
         LA    RC,72(RC)                                                        
         ST    RC,ALINK                                                         
         LHI   R3,LINKL                                                         
         L     RF,4(RD)                                                         
         AR    RD,R3                                                            
         ST    RD,8(RF)                                                         
         ST    RF,4(RD)                                                         
         LR    R2,RC               CLEAR W/S                                    
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
         LA    RF,ACTROUTS                                                      
         LA    R0,ACTROUTN                                                      
         XR    RE,RE                                                            
INIT02   LA    RE,1(RE)                                                         
         ST    RB,0(RF)                                                         
         STC   RE,0(RF)                                                         
         LA    RF,L'ACTROUTS(RF)                                                
         BCT   R0,INIT02                                                        
*                                                                               
         XC    FALINK,FALINK                                                    
         LA    RF,BASOLAYH                                                      
         ST    RF,FALABLD                                                       
         ST    RF,FVADDR                                                        
         L     RF,=V(TWABLD)                                                    
         A     RF,BORELO                                                        
         ST    RF,FALTBLD                                                       
         LA    RF,SEND                                                          
         ST    RF,FALASND                                                       
         LA    RF,RECEIVE                                                       
         ST    RF,FALARCV                                                       
         LA    RF,STOP                                                          
         ST    RF,FALASTP                                                       
*        LA    RF,RESUME                                                        
*        ST    RF,FALARSM                                                       
         LA    RF,TWAPGS                                                        
         ST    RF,FALAPGS                                                       
         LA    RF,FAMSGBLK                                                      
         ST    RF,FALAMSG                                                       
         LA    RF,FACON                                                         
         ST    RF,FALACON                                                       
         MVC   FALASWCH,VSWITCH                                                 
         LH    RE,=AL2(FASAVE-TWAD)                                             
         LA    RE,TWAD(RE)                                                      
         ST    RE,FALASVE                                                       
*                                                                               
         GOTO1 LOADOVR                                                          
*                                                                               
         GOTO1 VFALINK,BCPARM,FALINKD                                           
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* RECEIVE CALLBACK ROUTINE                                            *         
***********************************************************************         
         SPACE 1                                                                
RECEIVE  NTR1  ,                                                                
         MVC   AGETDATA,0(R1)                                                   
*                                                                               
         CLI   SVOVR,0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVCD)                                           
         B     EXITERR                                                          
*                                                                               
         MVI   LINKINDS,0          FIRST FOR RECEIVE                            
         MVI   RECMODE,0                                                        
         MVI   LINKMODE,MRCVFST                                                 
         GOTO1 BONTRYA                                                          
         BNE   EXITERR                                                          
*                                                                               
RCV02    DS    0H                                                               
         XC    FARETBK(10*L'FARETBK),FARETBK                                    
         GOTO1 AGETDATA,BCPARM,FALINKD,FARETBK                                  
         BL    EXITN               FALINK WILL HAVE SET ERROR MESSAGE           
         BNE   RCVEND                                                           
         LM    R3,R5,FARETBK       R3=A(TABLENTRY)R4=A(DATA),R5=L'DATA          
*                                                                               
         CLI   FARETBK,0           TEST ELEMENT CODE                            
         BNE   RCV10                                                            
         TM    LINKINDS,LINKIRCV                                                
         BZ    RCV04                                                            
         MVI   LINKMODE,MRCVHDRL   LAST FOR PREVIOUS MAP HEADER                 
         GOTO1 BONTRYA                                                          
         BNE   EXITERR                                                          
*                                                                               
RCV04    DS    0H                                                               
         USING MHELD,R3                                                         
         ST    R3,AMHEL                                                         
         OI    LINKINDS,LINKIRCV                                                
*                                                                               
         XC    ORCVHDRF,ORCVHDRF   SET A(OVERLAY ROUTINES)                      
         XC    ORCVHDRL,ORCVHDRL                                                
         CLI   MHLEN,MHELDL                                                     
         BNH   RCV06                                                            
         L     RE,BONTRYA                                                       
         XR    RF,RF                                                            
         ICM   RF,3,MHRCVF         FIRST FOR ELEMENT RECEIVE                    
         BZ    *+12                                                             
         LA    R0,0(RE,RF)                                                      
         ST    R0,ORCVHDRF                                                      
         ICM   RF,3,MHRCVL         LAST FOR ELEMENT RECEIVE                     
         BZ    *+12                                                             
         LA    R0,0(RE,RF)                                                      
         ST    R0,ORCVHDRL                                                      
         OC    OSND,OSND           TEST SEND ALREADY SET                        
         BNZ   RCV06                                                            
         ICM   RF,3,MHSND          SEND                                         
         BZ    *+12                                                             
         LA    R0,0(RE,RF)                                                      
         ST    R0,OSND                                                          
*                                                                               
RCV06    DS    0H                                                               
         XC    ELEM,ELEM           CLEAR ELEMENT DETAILS                        
         XC    AMDEL,AMDEL                                                      
         TM    MHINDS,MHIRECEL     TEST RECORD ELEMENT                          
         BZ    RCV08               YES - SET ELEMENT CODE/LENGTH                
         MVC   ELEM(L'MHELCOD),MHCODE+1                                         
         CLI   MHELCOD,0                                                        
         BE    *+10                                                             
         MVC   ELEM(L'MHELCOD),MHELCOD                                          
         MVC   ELEM+L'MHELCOD(L'MHELLEN),MHELLEN                                
*                                                                               
RCV08    DS    0H                                                               
         MVI   LINKMODE,MRCVHDRF   FIRST FOR MAP HEADER                         
         GOTO1 BONTRYA                                                          
         BNE   EXITERR                                                          
         B     RCV02                                                            
         DROP  R3                                                               
*                                                                               
RCV10    DS    0H                                                               
         USING MDELD,R3                                                         
         ST    R3,AMDEL                                                         
*                                                                               
         STH   R5,DATALEN          COPY DATA FOR OVERLAY                        
         MVI   DATA,0              CLEAR DATA TO ZEROS OR SPACES                
         CLI   MDTYPE,MDTCHQ                                                    
         BNE   *+8                                                              
         MVI   DATA,C' '                                                        
         MVC   DATA+1(L'DATA-1),DATA                                            
         BCTR  R5,0                                                             
         STH   R5,DATALENX                                                      
         LTR   R5,R5                                                            
         BM    RCV20                                                            
         MVC   DATA(0),0(R4)                                                    
         EX    R5,*-6                                                           
*                                                                               
         CLI   MDTYPE,MDTCHQ       TEST CHARACTERS                              
         BNE   RCV20                                                            
         LA    RE,DATA(R5)         CONVERT TRAILING ZEROS TO SPACES             
         LA    R5,1(R5)                                                         
RCV12    CLI   0(RE),0                                                          
         BNE   RCV20                                                            
         MVI   0(RE),C' '                                                       
         BCTR  RE,0                                                             
         BCT   R5,RCV12                                                         
         B     RCV20                                                            
*                                                                               
RCV20    DS    0H                                                               
         XC    ORCVDATA,ORCVDATA                                                
         CLI   MDLEN,MDELDL                                                     
         BNH   RCV22                                                            
         XR    RF,RF               SET A(OVERLAY RECEIVE DATA ROUTINE)          
         ICM   RF,3,MDRCV                                                       
         BZ    *+12                                                             
         A     RF,BONTRYA                                                       
         ST    RF,ORCVDATA                                                      
*                                                                               
         TM    MDINDS,MDIELFLD     TEST PUT DATA IN ELEM                        
         BZ    RCV22                                                            
         XR    RE,RE                                                            
         IC    RE,MDDISP                                                        
         LA    R1,ELEM(RE)                                                      
         XR    RF,RF               USE LENGTH FROM MAP DATA                     
         ICM   RF,1,MDDLEN         RECEIVED LENGTH MAY NOT BE THE SAME          
         BNZ   *+8                                                              
         LH    RF,DATALEN                                                       
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BM    RCV22                                                            
         MVC   0(0,R1),DATA                                                     
         EX    RF,*-6                                                           
         L     R1,AMHEL                                                         
         TM    MHINDS-MHELD(R1),MHIRECEL                                        
         BZ    RCV22               TEST RECORD ELEMENT                          
         LA    RE,1(RE,RF)         RE = MIN. LENGTH OF ELEMENT                  
         CLM   RE,1,ELEM+1                                                      
         BNH   *+8                                                              
         STC   RE,ELEM+1           RESET ELEMENT LENGTH                         
*                                                                               
RCV22    DS    0H                                                               
         TM    MDINDS,MDIMODE      TEST SET RECMODE                             
         BZ    *+10                                                             
         MVC   RECMODE,DATA                                                     
         MVI   LINKMODE,MRCVDATA                                                
         GOTO1 BONTRYA                                                          
         BNE   EXITERR                                                          
         B     RCV02                                                            
         DROP  R3                                                               
*                                                                               
RCVEND   DS    0H                                                               
         MVI   LINKMODE,MRCVHDRL   LAST FOR LAST ELEMENT                        
         GOTO1 BONTRYA                                                          
         BNE   EXITERR                                                          
         MVI   LINKMODE,MRCVLST    LAST FOR RECEIVE                             
         GOTO1 BONTRYA                                                          
         BNE   EXITERR                                                          
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND CALLBACK ROUTINE                                               *         
***********************************************************************         
         SPACE 1                                                                
SEND     NTR1  ,                                                                
         TM    LINKINDS,LINKIRCV   TEST RECEIVED ANYTHING                       
         BZ    EXITY                                                            
*                                                                               
         MVC   ASETELEM,0(R1)                                                   
         MVC   AADDDATA,4(R1)                                                   
*                                                                               
         MVI   LINKMODE,MSND                                                    
         GOTO1 BONTRYA                                                          
         BNE   EXITERR                                                          
*                                                                               
SENDEND  DS    0H                                                               
                                                                                
         TM    LINKINDS,LINKISNT   TEST ANYTHING SENT                           
         BZ    EXITY                                                            
         GOTO1 AADDDATA,BCPARM,FALINKD,0,0,0  END OF SEND                       
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* LOAD OVERLAY                                                        *         
*                                                                     *         
* EXIT: BONTRYA = A(OVERLAY)                                          *         
***********************************************************************         
         SPACE 1                                                                
LOADOVR  NTR1  ,                                                                
         LA    R2,BASOLAYH                                                      
         USING FALSCRD,R2                                                       
         CLI   FALCONA,C'U'        TEST FIRST TIME FOR A RECEIVE                
         BNE   LOVR10                                                           
         CLI   FALCONC,C'O'                                                     
         BNE   LOVR10                                                           
*                                                                               
         GOTO1 VGETFACT,BCPARM,(X'80',0),F#FAL1ST                               
         L     R3,0(,R1)           GET RETURN BLOCK                             
         CLC   =C'B=',0(R3)                                                     
         BNE   LOADOVRN                                                         
         AHI   R3,2                BUMP PAST "B="                               
         GOTO1 VHEXIN,BCPARM,(R3),BOWORK1,2                                     
         CLI   BCPARM+15,1                                                      
         BNE   LOADOVRN                                                         
         XR    R3,R3                                                            
         IC    R3,BOWORK1          R3=ELEMENT CODE                              
*                                                                               
         LA    R4,OVRTAB                                                        
         USING OVRTABD,R4                                                       
LOVR02   CLI   OVRTABD,EOT                                                      
         BE    LOADOVRN                                                         
         CLM   R3,3,OVREL                                                       
         BE    *+12                                                             
         LA    R4,OVRTABL(R4)                                                   
         B     LOVR02                                                           
         MVC   SVOVR,OVRLAY                                                     
         DROP  R4                                                               
*                                                                               
LOVR10   CLI   SVOVR,0             TEST NOTHING DONE YET                        
         BE    LOADOVRX                                                         
         GOTO1 VCOLY,BCPARM,(SVOVR,0),0                                         
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XR    RF,RF                                                            
         ICM   RF,7,1(R1)          SET A(OVERLAY)                               
         ST    RF,BONTRYA                                                       
*                                                                               
         MVI   LINKMODE,MSETTAB                                                 
         GOTO1 BONTRYA                                                          
         MVC   FALAMAP,AMAPTAB                                                  
*                                                                               
LOADOVRX DS    0H                                                               
         B     EXITY                                                            
*                                                                               
LOADOVRN DS    0H                                                               
         MVC   FVXTRA(8),0(R3)                                                  
         B     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* TEST VALIDITY OF MAP TABLE (FOR DEBUGGING)                          *         
*                                                                     *         
* NTRY: AMAPTAB = A(MAP TABLE)                                        *         
* EXIT: DC H'0' IF TABLE INVALID                                      *         
***********************************************************************         
         SPACE 1                                                                
VALTAB   DS    0H                                                               
         ICM   R3,15,AMAPTAB                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING MHELD,R3            R3=A(MAP HEADER ELEMENT)                     
*                                                                               
VTAB02   DS    0H                                                               
         CLI   MHLEN,MHLENX        TEST END-OF-TABLE                            
         BE    VALTABX                                                          
*                                                                               
         CLI   MHLEN,MHELDL        TEST HEADER EXTENDED                         
         BE    VTAB04                                                           
         CLI   MHLEN,MHELDL2       YES - MUST BE >= MHELDL2                     
         BNL   VTAB04                                                           
         LR    RF,R3               RF=DISP. IN OVERLAY                          
         S     RF,BONTRYA                                                       
         DC    H'0'                                                             
*                                                                               
VTAB04   DS    0H                                                               
         XR    R4,R4                                                            
         ICM   R4,3,MHDISP                                                      
         BNZ   *+12                                                             
         LR    RF,R3               RF=DISP. IN OVERLAY                          
         S     RF,BONTRYA                                                       
         DC    H'0'                                                             
         LA    R4,MHELD(R4)        R4=A(NEXT MAP HEADER)                        
*                                                                               
         XR    R2,R2                                                            
         IC    R2,MHLEN                                                         
         LA    R2,MHELD(R2)        R2=A(MAP ELEMENT DATA)                       
         USING MDELD,R2                                                         
*                                                                               
         XR    RF,RF                                                            
VTAB06   DS    0H                                                               
         ICM   RF,1,MDLEN          TEST END OF ELEMENTS                         
         BZ    VTAB12                                                           
*                                                                               
         CLI   MDDLEN,MDLENV       TEST DATA IS VARIABLE LENGTH                 
         BNE   VTAB08                                                           
         CLI   MDTYPE,MDTCHQ       YES - TYPE MUST BE STRING                    
         BE    VTAB08                                                           
         LR    RF,R2               RF=DISP. IN OVERLAY                          
         S     RF,BONTRYA                                                       
         DC    H'0'                                                             
*                                                                               
VTAB08   DS    0H                                                               
         CLI   MDLEN,MDELDL        TEST ELEMENT EXTENDED                        
         BE    VTAB10                                                           
         CLI   MDLEN,MDELDL2       YES MUST BE >= MDELDL2                       
         BNL   *+12                                                             
         LR    RF,R2               RF=DISP. IN OVERLAY                          
         S     RF,BONTRYA                                                       
         DC    H'0'                                                             
*                                                                               
         TM    MDINDS,MDIMODE      TEST TO SET RECMODE                          
         BZ    VTAB10                                                           
         CLI   MDTYPE,MDTCHQ       TYPE MUST BE CHAR                            
         BNE   *+12                                                             
         CLI   MDDLEN,1            DATA LENGTH MUST BE 1                        
         BE    VTAB10                                                           
         LR    RF,R2               RF=DISP. IN OVERLAY                          
         S     RF,BONTRYA                                                       
         DC    H'0'                                                             
*                                                                               
VTAB10   DS    0H                                                               
         AR    R2,RF               BUMP R2 TO NEXT ELEMENT                      
         B     VTAB06                                                           
*                                                                               
VTAB12   LA    R2,1(R2)            BUMP R2 OVER END-OF-ELEMENTS                 
         CR    R4,R2               TEST ENDS IN CORRECT PLACE                   
         BE    *+12                                                             
         LR    RF,R3               RF=DISP. IN OVERLAY                          
         S     RF,BONTRYA                                                       
         DC    H'0'                                                             
*                                                                               
         LR    R3,R4               BUMP R3 TO NEXT MAP HEADER                   
         B     VTAB02                                                           
         DROP  R3,R2                                                            
*                                                                               
VALTABX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET A(MAPPING HEADER ELEMENT)                            *         
*                                                                     *         
* NTRY: P1 BYTE 0 = X'80' IF RECORD ELEMENT CODE TO BE MATCHED        *         
*             1-3 = ELEMENT CODE                                      *         
*          OR 1-3 = A(ELEMENT CODE)                                   *         
* EXIT: AMHEL     = A(MHELD)                                          *         
*       CC        = EQUAL IF FOUND                                    *         
***********************************************************************         
         SPACE 1                                                                
SETMHEL  DS    0H                                                               
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)          R2 = ELEMENT CODE                            
         OC    1(1,R1),1(R1)                                                    
         BZ    *+8                                                              
         LH    R2,0(R2)                                                         
*                                                                               
         ICM   R3,15,AMAPTAB       LOCATE TABLE ENTRY                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING MHELD,R3                                                         
         XR    RF,RF                                                            
SMHEL02  CLI   MHLEN,MHLENX                                                     
         BE    SETMHELN                                                         
*                                                                               
         TM    0(R1),X'80'         TEST RECORD ELEMENT CODE REQUIRED            
         BZ    SMHEL04                                                          
         CLI   MHLEN,MHELDL2       TEST HEADER EXTENDED                         
         BL    SMHEL08                                                          
         TM    MHINDS,MHIRECEL                                                  
         BZ    SMHEL08                                                          
         CLI   MHELCOD,0           TEST OVERRIDE ELEMENT CODE                   
         BE    SMHEL04                                                          
         CLM   R2,1,MHELCOD                                                     
         BE    SETMHELY                                                         
         B     SMHEL08                                                          
*                                                                               
SMHEL04  CLM   R2,3,MHCODE                                                      
         BE    SETMHELY                                                         
*                                                                               
SMHEL08  ICM   RF,3,MHDISP                                                      
         BXH   R3,RF,SMHEL02                                                    
*                                                                               
SETMHELN XC    AMHEL,AMHEL                                                      
         B     EXITN                                                            
*                                                                               
SETMHELY ST    R3,AMHEL                                                         
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET A(MAPPING ELEMENT DATA)                              *         
*                                                                     *         
* NTRY: P1    = MAP FIELD CODE                                        *         
*    OR P1    = A(MAP FIELD CODE)                                     *         
*       AMHEL = A(MAPPING ELEMENT HEADER)                             *         
* EXIT: AMDEL = A(MDELD)                                              *         
*          CC = EQUAL IF FOUND                                        *         
***********************************************************************         
         SPACE 1                                                                
SETMDEL  DS    0H                                                               
         L     R2,0(R1)            R2 = MAP FIELD CODE                          
         OC    0(2,R1),0(R1)                                                    
         BZ    *+8                                                              
         LH    R2,0(R2)                                                         
*                                                                               
         ICM   R3,15,AMHEL         LOCATE TABLE ENTRY                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XR    RF,RF                                                            
         IC    RF,MHLEN-MHELD(R3)                                               
         AR    R3,RF                                                            
         USING MDELD,R3                                                         
SMDEL02  ICM   RF,1,MDLEN                                                       
         BZ    SETMDELN                                                         
         CLM   R2,3,MDCODE                                                      
         BE    SETMDELY                                                         
         BXH   R3,RF,SMDEL02                                                    
*                                                                               
SETMDELN XC    AMDEL,AMDEL                                                      
         B     EXITN                                                            
*                                                                               
SETMDELY ST    R3,AMDEL                                                         
         B     EXITY                                                            
         SPACE 1                                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SEND MAP HEADER ELEMENT CODE                                        *         
*                                                                     *         
* NTRY: P1 = ELEMENT CODE                                             *         
*    OR P1 = A(MAPPING HEADER ELEMENT)                                *         
***********************************************************************         
         SPACE 1                                                                
SNDHDR   DS    0H                                                               
         L     R3,0(R1)                                                         
         USING MHELD,R3            R3 = A(MAPPING HEADER ELEMENT)               
         LH    R2,2(R1)            OR R2 = ELEMENT CODE                         
         CR    R3,R2                                                            
         BE    SHDR02                                                           
         ST    R3,AMHEL                                                         
         B     SHDR06                                                           
*                                                                               
SHDR02   DS    0H                                                               
         ICM   R3,15,AMHEL         TEST ALREADY HAVE ENTRY                      
         BZ    SHDR04                                                           
         CLM   R2,3,MHCODE                                                      
         BE    SHDR06                                                           
*                                                                               
SHDR04   DS    0H                  LOCATE A(ENTRY)                              
         GOTO1 ASETMHEL,BCPARM,(R2)                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AMHEL                                                         
*                                                                               
SHDR06   DS    0H                  SEND CODE                                    
         GOTO1 ASETELEM,BCPARM,FALINKD,MHELD,0                                  
         OI    LINKINDS,LINKISNT                                                
         XC    AMDEL,AMDEL                                                      
*                                                                               
SNDHDRX  B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SEND MAP FIELD DATA                                                 *         
*                                                                     *         
* NTRY: P1        = MAP CODE                                          *         
*    OR P1        = A(MAPPING DATA ELEMENT)                           *         
*       P2 BYTE 0 = L(DATA) - ONLY REQUIRED IF VARIABLE LENGTH DATA   *         
*             1-3 = A(DATA)                                           *         
***********************************************************************         
         SPACE 1                                                                
SNDDATA  DS    0H                                                               
         XR    R5,R5                                                            
         ICM   R5,7,5(R1)          R5=A(DATA)                                   
         XR    R4,R4                                                            
         IC    R4,4(R1)            R4=L(DATA)                                   
*                                                                               
         L     R3,0(R1)                                                         
         USING MDELD,R3            R3 = A(MAPPING DATA ELEMENT)                 
         LH    R2,2(R1)            OR R2 = MAP CODE                             
         CR    R3,R2                                                            
         BE    SDATA02                                                          
         ST    R3,AMDEL                                                         
         B     SDATA06                                                          
*                                                                               
SDATA02  DS    0H                                                               
         ICM   R3,15,AMDEL         TEST ALREADY HAVE ENTRY                      
         BZ    SDATA04                                                          
         CLM   R2,3,MDCODE                                                      
         BE    SDATA06                                                          
*                                                                               
SDATA04  DS    0H                  LOCATE A(ENTRY)                              
         GOTO1 ASETMDEL,BCPARM,(R2)                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AMDEL                                                         
*                                                                               
SDATA06  DS    0H                                                               
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         IC    R4,MDDLEN                                                        
*                                                                               
         TM    MDINDS,MDINNULL     TEST IGNORE ZERO/SPACES                      
         BZ    SDATA20                                                          
         LR    RE,R4                                                            
         BCTR  RE,0                RE = EXECUTABLE LENGTH                       
         CLI   MDTYPE,MDTPKQ       TEST PACKED NUMBER                           
         BE    *+12                                                             
         CLI   MDTYPE,MDTCAQ                                                    
         BNE   SDATA08                                                          
         EX    RE,*+12                                                          
         BE    SNDDATAX                                                         
         B     SDATA20                                                          
         CP    BCPZERO,0(0,R5)                                                  
*                                                                               
SDATA08  CLI   MDTYPE,MDTCHQ       TEST STRING FOR SPACES (THEN ZEROS)          
         BNE   SDATA10                                                          
         EX    RE,*+8                                                           
         BE    SNDDATAX                                                         
         CLC   0(0,R5),SPACES                                                   
*                                                                               
SDATA10  EX    RE,*+8              TEST OTHER TYPES FOR 0                       
         BZ    SNDDATAX                                                         
         OC    0(0,R5),0(R5)                                                    
*                                                                               
SDATA20  DS    0H                                                               
         GOTO1 AADDDATA,BCPARM,FALINKD,MDELD,(R5),(R4)                          
         OI    LINKINDS,LINKISNT                                                
*                                                                               
SNDDATAX B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SEND ELEMENT FIELD (EXTRACT DATA FROM ELEMENT AND SEND IT           *         
*                                                                     *         
* NTRY: P1        = MAP CODE                                          *         
*    OR P1        = A(MAPPING DATA ELEMENT)                           *         
*       P2 BYTE 0 = X'80' ON TO IGNORE ELEMENT ROUTINE MDSND          *         
*             1-3 = A(ELEMENT)                                        *         
***********************************************************************         
         SPACE 1                                                                
SNDFLD   DS    0H                                                               
         XR    RF,RF                                                            
         ICM   RF,7,5(R1)          COPY ELEMENT TO ELEM                         
         LA    RE,ELEM                                                          
         CR    RE,RF                                                            
         BE    *+10                                                             
         MVC   ELEM,0(RF)                                                       
*                                                                               
         MVC   BCBYTE1,4(R1)       SAVE INDICATORS                              
*                                                                               
         L     R3,0(R1)                                                         
         USING MDELD,R3            R3 = A(MAPPING DATA ELEMENT)                 
         LH    R2,2(R1)            OR R2 = MAP CODE                             
         CR    R3,R2                                                            
         BE    SFLD02                                                           
         ST    R3,AMDEL                                                         
         B     SFLD06                                                           
*                                                                               
SFLD02   DS    0H                                                               
         ICM   R3,15,AMDEL         TEST ALREADY HAVE ENTRY                      
         BZ    SFLD04                                                           
         CLM   R2,3,MDCODE                                                      
         BE    SFLD06                                                           
*                                                                               
SFLD04   DS    0H                  LOCATE A(ENTRY)                              
         GOTO1 ASETMDEL,BCPARM,(R2)                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AMDEL                                                         
*                                                                               
SFLD06   DS    0H                                                               
         CLI   MDLEN,MDELDL2       TEST ELEMENT IS EXTENDED                     
         BL    SNDFLDX                                                          
         XC    DATA,DATA                                                        
         XC    DATALEN,DATALEN                                                  
         XC    DATALENX,DATALENX                                                
         TM    MDINDS,MDIELFLD     TEST IS AN ELEMENT FIELD                     
         BZ    SFLD10                                                           
         CLC   MDDISP,ELEM+1       TEST FIELD IS PART OF ELEMENT                
         BNL   SNDFLDX                                                          
         XR    R2,R2                                                            
         IC    R2,MDDISP           R2=DISP. TO DATA                             
         XR    R4,R4                                                            
         ICM   R4,1,MDDLEN         R4=L(DATA)                                   
         BNZ   SFLD08                                                           
         IC    R4,ELEM+1           IF VARIABLE LENGTH                           
         SR    R4,R2               ASSUME LENGTH = L'ELEMENT - DISP.            
SFLD08   DS    0H                                                               
         LA    R2,ELEM(R2)         R2=A(DATA)                                   
         STH   R4,DATALEN          SET DATA LENGTH                              
         BCTR  R4,0                                                             
         STH   R4,DATALENX                                                      
         MVC   DATA,0(R2)          COPY DATA                                    
         EX    R4,*-6                                                           
*                                                                               
SFLD10   DS    0H                                                               
         XC    OSNDFLD,OSNDFLD     TEST FOR OVERLAY ROUTINE                     
         XR    RF,RF                                                            
         ICM   RF,3,MDSND                                                       
         BZ    SFLD12                                                           
         A     RF,BONTRYA                                                       
         ST    RF,OSNDFLD                                                       
         TM    BCBYTE1,X'80'       TEST IGNORE ROUTINE                          
         BO    SFLD12                                                           
         MVI   LINKMODE,MSNDFLD                                                 
         GOTO1 BONTRYA                                                          
         BNE   SNDFLDX             CC = EQUAL RETURNED THEN CONTINUE            
*                                                                               
SFLD12   DS    0H                                                               
         TM    MDINDS,MDIELFLD     TEST IS AN ELEMENT FIELD                     
         BZ    SNDFLDX                                                          
         LH    RF,DATALEN                                                       
         GOTO1 ASNDDATA,BCPARM,MDELD,((RF),DATA)                                
*                                                                               
SNDFLDX  DS    0H                                                               
         B     EXITY                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SEND ELEMENT DETAILS                                                *         
*                                                                     *         
* NTRY: P1        = 0 TO LOOK UP MAPPING HEADER FROM ELEMENT'S CODE   *         
*    OR P1        = MAPPING HEADER ELEMENT CODE                       *         
*    OR P1        = A(MAPPING HEADER ELEMENT)                         *         
*       P2 BYTE 0 = X'80' ON TO IGNORE ELEMENT ROUTINE MHSNDEL        *         
*             1-3 = A(ELEMENT)                                        *         
***********************************************************************         
         SPACE 1                                                                
SNDELEM  DS    0H                                                               
         XR    RF,RF                                                            
         ICM   RF,7,5(R1)          COPY ELEMENT TO ELEM                         
         LA    RE,ELEM                                                          
         CR    RF,RE                                                            
         BE    *+10                                                             
         MVC   ELEM,0(RF)                                                       
*                                                                               
         MVC   BCBYTE1,4(R1)       SAVE INDICATORS                              
*                                                                               
         ICM   R3,15,0(R1)         TEST USE ELEMENT'S CODE                      
         USING MHELD,R3                                                         
         BNZ   SELEM02                                                          
         XR    R2,R2                                                            
         IC    R2,ELEM                                                          
         GOTO1 ASETMHEL,BCPARM,(X'80',(R2))                                     
         BNE   EXITN                                                            
         L     R3,AMHEL                                                         
         B     SELEM06                                                          
*                                                                               
SELEM02  LH    R2,2(R1)                                                         
         CR    R2,R3               TEST GIVEN ELEMENT CODE                      
         BNE   SELEM04                                                          
         GOTO1 ASETMHEL,BCPARM,(R2)                                             
         BNE   EXITN                                                            
         L     R3,AMHEL                                                         
         B     SELEM06                                                          
*                                                                               
SELEM04  DS    0H                                                               
         ST    R3,AMHEL            GIVEN A(MAP HEADER ELEMENT)                  
*                                                                               
SELEM06  DS    0H                                                               
         XC    OSNDEL,OSNDEL                                                    
         XR    RF,RF                                                            
         ICM   RF,3,MHSNDEL        TEST FOR SEND ELEMENT ROUTINE                
         BZ    SELEM10                                                          
         A     RF,BONTRYA          YES - GO TO OVERLAY INSTEAD                  
         ST    RF,OSNDEL                                                        
         TM    BCBYTE1,X'80'       TEST IGNORE ROUTINE                          
         BO    SELEM10                                                          
         MVI   LINKMODE,MSNDEL                                                  
         GOTO1 BONTRYA                                                          
         BNE   SNDELEMX            CC = EQUAL RETURNED THEN CONTINUE            
*                                                                               
SELEM10  DS    0H                                                               
         GOTO1 ASETELEM,BCPARM,FALINKD,MHELD,0                                  
         OI    LINKINDS,LINKISNT                                                
         XR    RF,RF                                                            
         IC    RF,MHLEN                                                         
         AR    R3,RF                                                            
         USING MDELD,R3            R3 = A(MAPPING ELEMENT DATA)                 
SELEM12  CLI   MDLEN,MDLENX        TEST END OF TABLE                            
         BE    SNDELEMX                                                         
*                                                                               
         GOTO1 ASNDFLD,BCPARM,MDELD,ELEM                                        
*                                                                               
         XR    RF,RF                                                            
         IC    RF,MDLEN                                                         
         BXH   R3,RF,SELEM12                                                    
         DROP  R3                                                               
*                                                                               
SNDELEMX DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SEND RECORD ELEMENT DETAILS                                         *         
*                                                                     *         
* NTRY: P1 BYTE 0 = DISP. TO FIRST ELEMENT (DEFAULT FOR ACCMST REC)   *         
*             1-3 = A(RECORD)                                         *         
***********************************************************************         
         SPACE 1                                                                
SNDREC   DS    0H                                                               
         XR    R2,R2                                                            
         ICM   R2,1,0(R1)                                                       
         BNZ   *+8                                                              
         LA    R2,ACCRFST-ACCRECD                                               
         XR    R3,R3                                                            
         ICM   R3,7,1(R1)                                                       
         AR    R3,R2               R3 = A(FIRST ELEMENT)                        
*                                                                               
SREC02   CLI   0(R3),0             TEST END OF RECORD                           
         BE    SNDRECX                                                          
         XC    ELEM,ELEM           COPY ELEMENT TO ELEM                         
         IC    RF,1(R3)                                                         
         BCTR  RF,0                                                             
         MVC   ELEM(0),0(R3)                                                    
         EX    RF,*-6                                                           
         GOTO1 ASNDELEM,BCPARM,0,ELEM                                           
         XR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         BXH   R3,RF,SREC02                                                     
*                                                                               
SNDRECX  DS    0H                                                               
         B     EXITY                                                            
         EJECT                                                                  
STOP     NTR1  ,                                                                
         DC    H'0'                                                             
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
                                                                                
*WAPGS   DC    X'020300'           LIST OF TWA PAGES FOR FALINK                 
TWAPGS   DC    X'0102030400'       LIST OF TWA PAGES FOR FALINK                 
SPACES   DC    CL256' '                                                         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* OVERLAY TABLE                                                       *         
***********************************************************************         
OVRTABD  DSECT                                                                  
OVRLAY   DS    XL1                 OVERLAY                                      
OVREL    DS    AL2                 ELEMENT CODE                                 
OVRTABL  EQU   *-OVRTABD                                                        
*                                                                               
CLB50    CSECT                                                                  
OVRTAB   DS    0X                                                               
         DC    AL1(OC#GEN),AL2(MH#INI)     INITIALIZATION                       
         DC    AL1(OC#GEN),AL2(MH#ACV)     ACCOUNT VALIDATION                   
         DC    AL1(OC#GEN),AL2(MH#JOPT)    JOB OPTION VALUES                    
         DC    AL1(OC#GEN),AL2(MH#JWCOP)   JOB & W/C OPTION VALUES              
         DC    AL1(OC#FMT),AL2(MH#FMT)     FORMAT RECORD                        
         DC    AL1(OC#CPJ),AL2(MH#CPL)     CLIENT PRODUCT LIST                  
         DC    AL1(OC#CPJ),AL2(MH#JBL)     JOB LIST                             
         DC    AL1(OC#CPJ),AL2(MH#JDT)     JOB DETAILS                          
         DC    AL1(OC#DSC),AL2(MH#DSR)     DATA SOURCE LIST                     
         DC    AL1(OC#TRN),AL2(MH#TRL)     TRANSACTION LIST                     
         DC    AL1(OC#IIN),AL2(MH#IIL)     INTERNAL INVOICES                    
         DC    AL1(OC#SUM),AL2(MH#SUM)     JOB SUMMARY                          
         DC    AL1(OC#BIL),AL2(MH#BIL)     BILL LIST                            
         DC    AL1(OC#BIL),AL2(MH#BLR)     BILL LIST - REVERSE                  
         DC    AL1(OC#BEDIT),AL2(MH#BED)   BILL EDIT                            
         DC    AL1(OC#RVC),AL2(MH#RVC)     REVALUE CURRENCIES                   
         DC    AL1(OC#MAT),AL2(MH#MAT)     UPDATE/DRAFT MATCH                   
         DC    AL1(OC#NARR),AL2(MH#NARR)   NARRATIVE                            
OVRTABX  DC    AL1(EOT)                                                         
*                                                                               
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* SAVED WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   TWAD+X'A00'                                                      
SVVALS   DS    0XL512                                                           
SVOVR    DS    XL1                 OVERLAY NUMBER                               
         DS    (L'SVVALS-(*-SVVALS))X                                           
         EJECT                                                                  
* ACCLBLINK                                                                     
       ++INCLUDE ACCLBLINK                                                      
*                                                                               
*FAFACTS (GETFACT)                                                              
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
LINKD    DSECT                                                                  
         ORG   CONTWS                                                           
FALINK   DS    XL(FALINKDL)        FALINK BLOCK - COVERED BY FALINKD            
FARETBK  DS    10F                 FALINK RETURN BLOCK                          
FARETBKL EQU   *-FARETBK                                                        
FACON    DS    XL(L'FALCON)        BUFFER FOR CONTROL FIELD                     
FAMSGBLK DS    XL(FAMSGDL)         MESSAGE BLOCK - COVERED BY FAMSGD            
*                                                                               
ALKROUTS DS    0A                  FALINK CALL BACK ROUTINES                    
ASETELEM DS    A                   A(SET ELEMENT ROUTINE)                       
AADDDATA DS    A                   A(ADD DATA ROUTINE)                          
AGETDATA DS    A                   A(GET DATA ROUTINE)                          
*                                                                               
         DS    (L'CONTWS-(*-CONTWS))X                                           
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'111ACCLB50   10/27/11'                                      
         END                                                                    
