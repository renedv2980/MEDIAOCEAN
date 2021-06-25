*          DATA SET SEACS14    AT LEVEL 075 AS OF 03/10/04                      
*&&      SET   NOP=N                                                            
         TITLE 'SEACS14 - SECURITY ACCESS - DDS PERSON RECORDS'                 
*PHASE TA0D14,*                                                                 
*INCLUDE SCINKEY                                                                
*INCLUDE DATTIM                                                                 
         SPACE 2                                                                
ACS14    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACS14*,RA,R9,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RD,APWORKA                                                       
                                                                                
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         SPACE 1                                                                
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     EXIT                05 - APMDELR                                 
         B     EXIT                06 - APMRESR                                 
         B     EXIT                07 - APMVALP                                 
         B     EXIT                08 - APMGETS                                 
         B     EXIT                09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     EXIT                11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     EXIT                14 - APMLSCR                                 
         B     EXIT                15 - APMVALQ                                 
         B     EXIT                16 - APMREPP                                 
         B     EXIT                17 - APMSETT                                 
         B     EXIT                18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*************************************************************                   
*        ROUTINE TO VALIDATE KEY OF PERSON RECORD           *                   
*************************************************************                   
         SPACE 1                                                                
VALKEY   EQU   *                                                                
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,CUAGYSEC                                                 
         XC    SAPEDEF,SAPEDEF                                                  
*                                                                               
VKEY010  MVI   FVMINL,3                                                         
         GOTO1 AFVAL,DDSPIDH       READ PERSONAL-ID                             
         BNE   VALKEYXX                                                         
*                                                                               
         MVC   SAPEPID,FVIFLD      READ HI FOR CURRENT RECORD                   
         LA    R1,IOHI+IOCONFIL+IO1                                             
         CLI   APACTN,ACTCHA                                                    
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX                                                          
*                                                                               
         L     R2,AIOAREA1                                                      
         CLC   IOKEYSAV(SAPEDEF-SAPEKEY),SAPEKEY                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     VALKEYX                                                          
*                                                                               
         MVC   DDSPID(8),SAPEPID   DISPLAY PERSONAL ID                          
         OI    DDSPIDH+6,X'80'                                                  
*                                                                               
VALSYS   MVI   FVMINL,1            READ SYSTEM FIELD                            
         GOTO1 AFVAL,DDSSYSH                                                    
         BNE   VKEY09                                                           
         GOTO1 AVALSYS,DDSSYSH     VALIDATE SYSTEM                              
         BNE   VKEY09                                                           
         MVC   SYSTEM,APWORK       HOLD FOR LATER USE                           
*                                                                               
VKEY09   MVC   HALF,SAPEDEF        INVERSE DATE                                 
         XC    HALF,=X'FFFF'                                                    
         GOTO1 VDATCON,DMCB,(2,HALF),(17,DDSDEF)                                
         OI    DDSDEFH+6,X'80'                                                  
*                                                                               
         MVI   APINDS,APIOKCHA+APIOKDIS                                         
         B     VALKEYX                                                          
*                                                                               
VALKEYX  MVC   APRECKEY(L'SAPEKEY),SAPEKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLC   APACTN,TWALACT                                                   
         BE    EXIT                                                             
         XC    NEXSYS,NEXSYS                                                    
         XC    DISSYS,DISSYS                                                    
VALKEYXX B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        ROUTINE TO ADD OR CHANGE A DDS AUTH ELEMENT        *                   
*************************************************************                   
         SPACE 1                                                                
VALREC   EQU   *                                                                
         CLC   DISSYS,SYSTEM       HAS SYSTEM CHANGED                           
         BNE   DISREC                                                           
*                                                                               
         L     R2,AIOAREA1         SET UP PERSON RECORD FOR ADD/CHANGE          
         USING SAPEREC,R2                                                       
*                                                                               
VRCHA030 XC    APELEM,APELEM       DELETE DDS ELEMENT                           
         MVI   APELEM,SADDSELQ                                                  
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,SAPEREC                                                  
*                                                                               
         MVI   APELEM,SADDLELQ     LIST ELEMENT                                 
         MVI   APELEM+1,2                                                       
         MVC   APELEM+2,SYSTEM     SET SYSTEM                                   
         MVI   APELEM+3,1          SET READ ONLY                                
         GOTO1 ADELELS,SAPEREC                                                  
*                                                                               
         MVI   APELEM+3,2          SET UPDATIVE                                 
         GOTO1 ADELELS,SAPEREC                                                  
*                                                                               
         XC    APELEM,APELEM       BUILD DDS ACCESS ELEMENT                     
         LA    R3,APELEM                                                        
         USING SADDSD,R3                                                        
         MVI   SADDSEL,SADDSELQ                                                 
         MVI   SADDSLN,SADDSLNQ                                                 
*                                                                               
VALGOD   MVI   FVMINL,1            TEST FOR A GOD                               
         NI    SADDSAT1,255-SADDSGOD                                            
         GOTO1 AFVAL,DDSGODH                                                    
         BAS   RE,YAYRNEY          WAS IT YES OR NO                             
         BNE   *+8                                                              
         OI    SADDSAT1,SADDSGOD   HAIL OH MIGHTY DDS USER                      
*                                                                               
         MVI   FVMINL,1            VALIDATE DEPT                                
         GOTO1 AFVAL,DDSDEPH                                                    
         LA    R1,DEPTTAB                                                       
VALD010  SR    RF,RF               EX COMP FOR NAME                             
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),1(R1)                                                  
         BE    VALD020                                                          
         LA    R1,L'DEPTTAB(R1)                                                 
         CLI   0(R1),X'FF'                                                      
         BE    VALD020                                                          
         BNE   VALD010                                                          
         B     VALRECN                                                          
*                                                                               
VALD020  MVC   DDSDEPT,0(R1)       SAVE DEPT                                    
*                                                                               
         MVC   DDSDEP(8),1(R1)                                                  
         OI    DDSDEPH+6,X'80'     DISPLAY THE DEPT NAME                        
*                                                                               
         MVI   FVMINL,1            VALIDATE LEVEL                               
         GOTO1 AFVAL,DDSLEVH                                                    
         BNE   VALRECX                                                          
         TM    FVIIND,FVINUM       MUST BE NUMERIC                              
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALRECN                                                          
         OC    SCFULL,SCFULL       MUST BE > 0                                  
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALRECN                                                          
         CLC   SCFULL,=F'16'       MUST BE <=16                                 
         BH    *-16                                                             
         SR    R1,R1                                                            
         IC    R1,SCFULL+3                                                      
         BCTR  R1,0                                                             
         STC   R1,DDSLEVL          SAVE LEVEL                                   
*                                                                               
         MVC   DDSDPLV,DDSDEPT     MERGE INTO DDSDPLV                           
         OC    DDSDPLV,DDSLEVL                                                  
*                                                                               
         MVC   SADDSDEP,DDSDPLV                                                 
*                                                                               
         GOTO1 AADDELS,SAPEREC     ADD DD ELEMENT                               
*                                                                               
         XC    APELEM,APELEM       BUILD DDS AGENCY LIST ELEMENTS               
         LA    R3,APELEM                                                        
         USING SADDLD,R3                                                        
         MVI   SADDLEL,SADDLELQ    LIST ELEMENT                                 
         MVI   SADDLLN,5                                                        
         MVC   SADDLSYS,SYSTEM     SET SYSTEM                                   
         MVI   SADDLACC,1          SET READ ONLY                                
*                                                                               
         MVI   FVMINL,1            VALIDATE LEVEL                               
         GOTO1 AFVAL,DDSOVLH                                                    
         BNE   VALLIST1                                                         
*                                                                               
         TM    FVIIND,FVINUM       MUST BE NUMERIC                              
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALRECXX                                                         
         OC    SCFULL,SCFULL       MUST BE > 0                                  
         BZ    VALRECN                                                          
         CLC   SCFULL,=F'16'       MUST BE <=16                                 
         BH    *-16                                                             
         SR    R1,R1                                                            
         IC    R1,SCFULL+3                                                      
         BCTR  R1,0                                                             
         STC   R1,SADDLLEV         SAVE LEVEL                                   
         OI    SADDLLEV,X'80'      FLAG OVERIDE USED                            
*                                                                               
VALLIST1 CLI   DDSLIS1H+5,0        TEST ANY INPUT                               
         BE    VALUP1                                                           
*                                                                               
         LA    R1,DDSLIS1          LIST 1                                       
         LA    R8,SADDLAGL                                                      
         MVI   SADDLACC,1          READ ACCESS ONLY                             
*                                                                               
         CLC   0(3,R1),=C'ALL'     TEST FOR ALL ACCESS                          
         BNE   VALLIST2                                                         
         MVC   0(2,R8),=X'FFFF'                                                 
         LA    R8,2(R8)                                                         
         B     VALUP1                                                           
*                                                                               
VALLIST2 CLI   0(R1),C' '          BLANK IS END OF LIST                         
         BNH   VALLIST3                                                         
         MVC   0(2,R8),0(R1)       NON BLANK IS 2 CHR AGY                       
         LA    R8,2(R8)                                                         
         LA    R1,2(R1)                                                         
         CLI   0(R1),C','          NEXT MAY BE A ,                              
         BNE   VALLIST2                                                         
         LA    R1,1(R1)                                                         
         B     VALLIST2            NEXT AGENCY                                  
*                                                                               
VALLIST3 CLI   DDSLIS2H+5,0        TEST ANY INPUT                               
         BE    VALUP1                                                           
         LA    R1,DDSLIS2                                                       
*                                                                               
VALLIST4 CLI   0(R1),C' '          BLANK IS END OF LIST                         
         BNH   VALUP1                                                           
         MVC   0(2,R8),0(R1)       NON BLANK IS 2 CHR AGY                       
         LA    R8,2(R8)                                                         
         LA    R1,2(R1)                                                         
         CLI   0(R1),C','          NEXT MAY BE A ,                              
         BNE   VALLIST4                                                         
         LA    R1,1(R1)                                                         
         B     VALLIST4            NEXT AGENCY                                  
*                                                                               
VALUP1   SR    R8,R3               R8=ELEMENT LEN                               
         CH    R8,=H'254'                                                       
         BH    VALRECN                                                          
         STC   R8,SADDLLN                                                       
         GOTO1 AADDELS,SAPEREC     ADD READ ONLY ELEMENT                        
*                                                                               
VALLIST5 XC    APELEM,APELEM       BUILD DDS AGENCY LIST ELEMENTS               
         LA    R3,APELEM                                                        
         USING SADDLD,R3                                                        
         MVI   SADDLEL,SADDLELQ    LIST ELEMENT                                 
         MVI   SADDLLN,5                                                        
         MVC   SADDLSYS,SYSTEM     SET SYSTEM                                   
         MVI   SADDLACC,2          SET WRITE                                    
*                                                                               
VALLIST6 CLI   DDSLIS7H+5,0        TEST ANY INPUT                               
         BE    VALUP2                                                           
*                                                                               
         LA    R1,DDSLIS7          LIST 1                                       
         LA    R8,SADDLAGL                                                      
         MVI   SADDLACC,2          READ WRITE                                   
*                                                                               
         CLC   0(3,R1),=C'ALL'     TEST FOR ALL ACCESS                          
         BNE   VALLIST7                                                         
         MVC   0(2,R8),=X'FFFF'                                                 
         LA    R8,2(R8)                                                         
         B     VALUP2                                                           
*                                                                               
VALLIST7 CLI   0(R1),C' '          BLANK IS END OF LIST                         
         BNH   VALLIST8                                                         
         MVC   0(2,R8),0(R1)       NON BLANK IS 2 CHR AGY                       
         LA    R8,2(R8)                                                         
         LA    R1,2(R1)                                                         
         CLI   0(R1),C','          NEXT MAY BE A ,                              
         BNE   VALLIST7                                                         
         LA    R1,1(R1)                                                         
         B     VALLIST7            NEXT AGENCY                                  
*                                                                               
VALLIST8 CLI   DDSLIS8H+5,0        TEST ANY INPUT                               
         BE    VALUP2                                                           
         LA    R1,DDSLIS8                                                       
*                                                                               
VALLIST9 CLI   0(R1),C' '          BLANK IS END OF LIST                         
         BNH   VALUP2                                                           
         MVC   0(2,R8),0(R1)       NON BLANK IS 2 CHR AGY                       
         LA    R8,2(R8)                                                         
         LA    R1,2(R1)                                                         
         CLI   0(R1),C','          NEXT MAY BE A ,                              
         BNE   VALLIST9                                                         
         LA    R1,1(R1)                                                         
         B     VALLIST9            NEXT AGENCY                                  
*                                                                               
VALUP2   SR    R8,R3               R8=ELEMENT LEN                               
         CH    R8,=H'254'                                                       
         BH    VALRECN                                                          
         STC   R8,SADDLLN                                                       
         GOTO1 AADDELS,SAPEREC     ADD WRITE ELEMENT                            
*                                                                               
VRUPD    GOTO1 ASETACT,SAPEREC                                                  
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
*                                                                               
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
VALRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
*                                                                               
VALRECN  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         LTR   RB,RB                                                            
VALRECXX B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        SUBROUTINES USED BY VALREC                         *                   
*************************************************************                   
         SPACE 1                                                                
CALSNIP  ST    RE,SAVERE           PARSNIP CALL                                 
         ST    R1,FVADDR                                                        
         ST    R1,DMCB                                                          
         GOTO1 VPARSNIP,DMCB,,BLOCK,(X'50',SEPTAB)                              
         CLI   4(R1),1                                                          
         BE    CALSNIPX                                                         
         BAS   RE,UNSNIP                                                        
         BNE   VALRECN                                                          
         LR    R8,R1                                                            
CALSNIPX L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
YAYRNEY  BNE   NEYLAD              NO INPUT MEANS NO                            
         CLI   FVIFLD,C'Y'                                                      
         BNE   *+14                                                             
         MVC   8(3,R1),LCYES       ECHO BACK YES                                
         NI    1(R1),255-X'08'                                                  
         CLI   FVIFLD,C'N'                                                      
         BNE   *+14                                                             
NEYLAD   MVC   8(3,R1),LCNO        ECHO BACK NO                                 
         OI    1(R1),X'08'                                                      
         OI    6(R1),X'80'                                                      
         CLC   8(3,R1),LCYES       SET CC ON EXIT                               
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        CONVERT PARSNIP BLOCK TO CAGCAGCAGCAGCAG           *                   
*************************************************************                   
         SPACE 1                                                                
UNSNIP   NTR1                                                                   
         LA    R4,BLOCK                                                         
         USING PSND,R4                                                          
VAGYFST  CLI   PSNTAG,PSNFLDQ      MUST START WITH A FIELD                      
         BNE   UNSNIPER                                                         
         TM    PSNERR,0                                                         
         BNE   UNSNIPER                                                         
         MVC   FULL,PSNFLD         SAVE A(NEXT FIELD)                           
         L     R1,PSNCOMP                                                       
         MVC   WORK+1(8),0(R1)                                                  
         MVC   WORK+0(1),PSNLEN                                                 
         BAS   RE,VALCTRY          MUST BE A COUNTRY CODE                       
         BNE   UNSNIPER                                                         
         ICM   R1,15,PSNATTR       MUST HAVE AT LEAST 1 ATTRIB                  
         BZ    UNSNIPER                                                         
VAGY010  ICM   R4,15,PSNATTR       GET A(ATTRIBUTE)                             
         BZ    VAGYNXT                                                          
         TM    PSNERR,0                                                         
         BNE   UNSNIPER                                                         
*                                                                               
         CLI   PSNLEN,2            MUST BE 2 LONG                               
         BNE   VAGY020                                                          
         L     R1,PSNCOMP                                                       
         MVC   0(1,R8),BYTE        SAVE LANG                                    
         MVC   1(2,R8),0(R1)       SAVE AGY                                     
         LA    R8,3(R8)                                                         
         B     VAGY010             NEXT ATTRIB                                  
*                                                                               
VAGY020  CLI   PSNLEN,3            IF AGY IS 3CHR LONG                          
         BNE   VAGY030                                                          
         L     R1,PSNCOMP                                                       
         CLC   0(3,R1),=C'ALL'     IT CAN BE ALL                                
         BNE   UNSNIPER                                                         
         MVC   0(1,R8),BYTE        SAVE CTRY                                    
         XC    1(2,R8),1(R8)       CLEAR AGY                                    
         LA    R8,3(R8)                                                         
         B     VAGY010             NEXT ATTRIB                                  
*                                                                               
VAGY030  L     R1,PSNCOMP                                                       
         CLC   0(2,R1),=C'L='                                                   
         BE    VAGYLIST                                                         
         BNE   UNSNIPER                                                         
*                                                                               
VAGYLIST EQU   *                                                                
         MVC   0(1,R8),BYTE        SAVE LANG                                    
         LA    R8,1(R8)            SAVE AGY                                     
         SR    RF,RF                                                            
         IC    RF,PSNLEN                                                        
         SH    RF,=H'2'                                                         
         STC   RF,0(R8)            SET LEN                                      
         MVC   1(10,R8),2(R1)                                                   
         LA    RF,1(RF)                                                         
         AR    R8,RF                                                            
         B     VAGY010             NEXT ATTRIB                                  
*                                                                               
VAGYNXT  ICM   R4,15,FULL          NEXT FIELD                                   
         BNZ   VAGYFST                                                          
*                                                                               
VAGYLST  LR    R1,R8               R8=ELEMENT LEN                               
         CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
UNSNIPER LR    R1,R4               ERROR EXIT                                   
         LTR   RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        VALIDATE COUNTRY CODE                              *                   
*        ENTRY WORK+0=LEN WORK+1=CTRY                       *                   
*        EXIT  BYTE=CODE WORK+1=CTRY                        *                   
*************************************************************                   
         SPACE 1                                                                
VALCTRY  NTR1                                                                   
         L     R1,ACTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         ZIC   R8,WORK                                                          
         SH    R8,=H'1'            R8=L'INPUT-1                                 
         BNM   VRCTY1                                                           
         B     VRCTY2              NO COUNTRY INPUT                             
VRCTY1   EX    R8,*+8                                                           
         B     *+10                                                             
         CLC   CTRYSHR(0),WORK+1   COMPARE ON SHORT NAME                        
         BE    VRCTY2                                                           
         BXLE  R1,RE,VRCTY1                                                     
         B     VRCTYX                                                           
*                                                                               
VRCTY2   MVC   BYTE,CTRYCODE       SAVE COUNTRY CODE                            
         MVC   WORK+1(L'CTRYSHR),CTRYSHR                                        
VRCTYY   CR    RB,RB                                                            
         B     EXIT                                                             
VRCTYX   LTR   RB,RB               CC NEQ                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
DISKEY   LA    R2,APRECKEY         GET PERSON RECORD KEY DATA                   
*                                                                               
         MVC   DDSPID(8),SAPEPID   DISPLAY PERSONAL ID                          
         OI    DDSPIDH+6,X'80'                                                  
*                                                                               
         GOTO1 ADISSYS,SYSTEM      REDISPLAY SYSTEM                             
         CLI   SYSTEM,0                                                         
         MVC   DDSSYS,ALL                                                       
         BE    *+10                                                             
         MVC   DDSSYS,APWORK                                                    
         OI    DDSSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         B     DISKEYX                                                          
*                                                                               
DISKEYX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
DISREC   L     R2,AIOAREA1         DISPLAY PERSON RECORD DATA                   
*                                                                               
         TWAXC DDSGODH                                                          
*                                                                               
         CLC   SYSTEM,DISSYS                                                    
         BNE   *+10                                                             
         MVC   SYSTEM,NEXSYS                                                    
*                                                                               
         GOTO1 ADISSYS,SYSTEM      REDISPLAY SYSTEM                             
         CLI   SYSTEM,0                                                         
         MVC   DDSSYS,ALL                                                       
         BE    *+10                                                             
         MVC   DDSSYS,APWORK                                                    
         OI    DDSSYSH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
         XC    APELEM,APELEM       READ DDS ACCESS ELEMENT                      
         LA    R3,APELEM                                                        
         USING SADDSD,R3                                                        
         MVI   SADDSEL,SADDSELQ                                                 
         GOTO1 AGETELS,SAPEREC                                                  
         ICM   R3,15,APPARM        GET ELEMENT ADDR                             
         BZ    DISRECX                                                          
*                                                                               
         MVC   DDSGOD,LCNO         TEST FOR A GOD                               
         OI    DDSGODH+1,X'08'                                                  
         TM    SADDSAT1,SADDSGOD   HAIL OH MIGHTY DDS USER                      
         BZ    *+14                                                             
         MVC   DDSGOD,LCYES                                                     
         NI    DDSGODH+1,255-X'08'                                              
         OI    DDSGODH+6,X'80'                                                  
*                                                                               
         MVC   DDSDPLV,SADDSDEP                                                 
         NI    DDSDPLV,X'F0'                                                    
         LA    R1,DEPTTAB                                                       
DISR010  SR    RF,RF               EX COMP FOR NAME                             
         CLC   0(1,R1),DDSDPLV                                                  
         BE    DISR020                                                          
         LA    R1,L'DEPTTAB(R1)                                                 
         CLC   0(L'DEPTTAB,R1),FFS                                              
         BNE   DISR010                                                          
DISR020  MVC   DDSDEP(8),1(R1)                                                  
         OI    DDSDEPH+6,X'80'                                                  
*                                                                               
         MVC   DDSDPLV,SADDSDEP    EDIT DEPT LEVEL                              
         NI    DDSDPLV,X'0F'                                                    
         SR    R1,R1                                                            
         IC    R1,DDSDPLV                                                       
         LA    R1,1(R1)                                                         
         EDIT  (R1),(2,DDSLEV),ZERO=NOBLANK,ALIGN=LEFT                          
         OI    DDSLEVH+6,X'80'                                                  
*                                                                               
         XC    APELEM,APELEM       READ DDS ACCESS ELEMENT                      
         LA    R3,APELEM                                                        
         USING SADDLD,R3                                                        
         MVI   SADDLEL,SADDLELQ                                                 
         MVI   APELEM+1,2                                                       
         MVC   APELEM+2(1),SYSTEM                                               
         MVI   APELEM+3,1          READ ONLY                                    
         GOTO1 AGETELS,SAPEREC                                                  
         ICM   R3,15,APPARM        GET ELEMENT ADDR                             
         BZ    DISRECX                                                          
*                                                                               
         TM    SADDLLEV,X'80'      EDIT OVERIDE LEVEL                           
         BZ    DREC030                                                          
         NI    SADDLLEV,X'7F'                                                   
         SR    R1,R1                                                            
         IC    R1,SADDLLEV                                                      
         LA    R1,1(R1)                                                         
         EDIT  (R1),(2,DDSOVL),ZERO=NOBLANK,ALIGN=LEFT                          
         OI    DDSOVLH+6,X'80'                                                  
*                                                                               
DREC030  SR    R1,R1                                                            
         IC    R1,SADDLLN                                                       
         AR    R1,R3                                                            
         ST    R1,FULL             REMEMBER END OF ELEMENT                      
*                                                                               
         LA    R1,SADDLAGL                                                      
         CLC   0(2,R1),=X'FFFF'    TEST FOR ALL                                 
         BNE   *+14                                                             
         MVC   DDSLIS1(3),=C'ALL'                                               
         B     RDEC039                                                          
*                                                                               
         LA    R8,DDSLIS1                                                       
RDEC035  OC    0(2,R1),0(R1)       ANYTHING THERE                               
         BZ    RDEC039                                                          
         MVC   0(2,R8),0(R1)                                                    
         MVI   2(R8),C','                                                       
         LA    R8,3(R8)            NEXT                                         
         LA    R1,2(R1)                                                         
         C     R1,FULL                                                          
         BL    RDEC035                                                          
         BCTR  R8,0                                                             
         MVI   0(R8),C' '                                                       
         B     RDEC039                                                          
*                                                                               
RDEC039  XC    APELEM,APELEM       READ DDS ACCESS ELEMENT                      
         LA    R3,APELEM                                                        
         USING SADDLD,R3                                                        
         MVI   SADDLEL,SADDLELQ                                                 
         MVI   APELEM+1,2                                                       
         MVC   APELEM+2(1),SYSTEM                                               
         MVI   APELEM+3,2          READ WRITE                                   
         GOTO1 AGETELS,SAPEREC                                                  
         ICM   R3,15,APPARM        GET ELEMENT ADDR                             
         BZ    DISRECX                                                          
*                                                                               
DREC040  SR    R1,R1                                                            
         IC    R1,SADDLLN                                                       
         AR    R1,R3                                                            
         ST    R1,FULL             REMEMBER END OF ELEMENT                      
*                                                                               
         LA    R1,SADDLAGL                                                      
         CLC   0(2,R1),=X'FFFF'    TEST FOR ALL                                 
         BNE   *+14                                                             
         MVC   DDSLIS7(3),=C'ALL'                                               
         B     RDEC049                                                          
*                                                                               
         LA    R8,DDSLIS7                                                       
RDEC045  OC    0(2,R1),0(R1)       ANYTHING THERE                               
         BZ    RDEC049                                                          
         MVC   0(2,R8),0(R1)                                                    
         MVI   2(R8),C','                                                       
         LA    R8,3(R8)            NEXT                                         
         LA    R1,2(R1)                                                         
         C     R1,FULL                                                          
         BL    RDEC045                                                          
         BCTR  R8,0                                                             
         MVI   0(R8),C' '                                                       
         B     RDEC049                                                          
*                                                                               
RDEC049  EQU   *                                                                
*                                                                               
DISREC90 GOTO1 ADISACT,SAPEREC                                                  
*                                                                               
DISRECX  SR    R1,R1               FIND NEXT EL                                 
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),SADDLELQ      IS IT A LIST                                 
         BE    *+8                                                              
         LA    R3,SAPEDATA         GOTO START OF DATA                           
DISRECX1 CLI   0(R3),SADDLELQ                                                   
         BE    DISRECXX            SAVE NEXT SYSTEM                             
         CLI   0(R3),0                                                          
         BE    DISRECXX                                                         
         SR    R1,R1                                                            
         ICM   R1,1,1(R3)                                                       
         BZ    DISRECXX                                                         
         AR    R3,R1               NEXT ELEMENT                                 
         B     DISRECX1                                                         
*                                                                               
DISRECXX MVC   NEXSYS,SADDLSYS     SAVE NEXT SYSTEM                             
         MVC   DISSYS,SYSTEM                                                    
         CLI   APACTN,ACTCHA       TEST FOR CHANGE                              
         BE    ICMSG03                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY AGENCY ACCESS LIST                         *                   
*************************************************************                   
         SPACE 1                                                                
DRAGS    ST    RE,SAVERE                                                        
         LA    R4,BLOCK                                                         
         XC    0(255,R4),0(R4)                                                  
         LA    R8,SADDLAGL                                                      
         C     R8,FULL             TEST FOR END OF ELEMENT                      
         BNL   DRAGSX                                                           
*                                                                               
DRAGS00  MVC   BYTE,0(R8)          GET COUNTRY                                  
*                                                                               
DRAGS001 BAS   RE,GETCTRY                                                       
         MVC   0(3,R4),WORK        BUILD CTR(                                   
         MVI   3(R4),C'('                                                       
         LA    R4,4(R4)                                                         
DRAGS01  OC    1(2,R8),1(R8)                                                    
         BZ    DRAGS010                                                         
*                                                                               
         CLI   1(R8),X'10'                                                      
         BH    DRAGS02             NORMAL CTRY,AGY                              
*                                                                               
         MVC   0(2,R4),=C'L='      ELSE L=LIST                                  
         SR    R1,R1                                                            
         IC    R1,1(R8)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R4),2(R8)                                                    
         AR    R8,R1                                                            
         LA    R8,3(R8)                                                         
         AR    R4,R1                                                            
         LA    R4,3(R4)                                                         
         B     DRAGS021                                                         
*                                                                               
DRAGS02  MVC   0(2,R4),1(R8)                                                    
         LA    R4,2(R4)                                                         
         B     DRAGS020                                                         
*                                                                               
DRAGS010 MVC   0(3,R4),=C'ALL'                                                  
         LA    R4,3(R4)                                                         
*                                                                               
DRAGS020 LA    R8,3(R8)            NEXT FIELD                                   
DRAGS021 MVI   0(R4),C')'          CLOSE BRACKETS                               
         C     R8,FULL             TEST FOR END OF ELEMENT                      
         BNL   DRAGSX                                                           
*                                                                               
         CLC   0(1,R8),BYTE        SAME COUNTRY                                 
         BE    *+8                                                              
         LA    R4,1(R4)            IF CTRY SAME AA,BB                           
*                                                                               
         MVI   0(R4),C','          ELSE AA),                                    
         LA    R4,1(R4)                                                         
*                                                                               
         BE    DRAGS01             SAME COUNTRY                                 
         BNE   DRAGS00                                                          
*                                                                               
DRAGSX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        GET COUNTRY NAME                                   *                   
*        BYTE=CODE WORK=CTRY                                *                   
*************************************************************                   
         SPACE 1                                                                
GETCTRY  NTR1                                                                   
         MVC   HALF,BYTE           SAVE ACTUAL VALUE                            
         NI    BYTE,255-X'80'      REMOVE UPDATIVE BIT                          
         L     R1,ACTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
GTCTY1   CLC   CTRYCODE,BYTE       COMPARE ON CODE                              
         BE    GTCTY2                                                           
         BXLE  R1,RE,GTCTY1                                                     
         B     GTCTYX                                                           
*                                                                               
GTCTY2   MVC   WORK,SPACES                                                      
         MVC   WORK(L'CTRYSHR),CTRYSHR                                          
GTCTYY   MVC   BYTE,HALF                                                        
         CR    RB,RB                                                            
         B     EXIT                                                             
GTCTYX   MVC   BYTE,HALF           CC NEQ                                       
         LTR   RB,RB               CC NEQ                                       
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*************************************************************                   
*        CONTROL INFO EXITS                                 *                   
*************************************************************                   
         SPACE 1                                                                
ICMSG03  LA    R1,3                RECORDS DISPLAYED ENTER CHANGES              
         B     SETCINF                                                          
*                                                                               
SETCINF  XC    APPARM(32),APPARM   SET UP GETTXT BLOCK                          
         LA    RF,APPARM                                                        
         MVI   8(RF),C'I'          MESSAGE TYPE                                 
         MVI   21(RF),X'0A'        CONTROL MSG                                  
         STH   R1,2(RF)            MESSAGE NUMBER                               
         MVC   FVMSGNO,=AL2(FVFGTSET)                                           
         B     EXIT                BACK TO GENERAL                              
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LITERALS                               *                   
*************************************************************                   
         SPACE 1                                                                
LCYES    DC    C'Yes'                                                           
LCNO     DC    C'No '                                                           
ALL      DC    C'ALL     '                                                      
FFS      DC    16X'FF'                                                          
SEPTAB   DC    X'01FF'                                                          
         DC    X'01',C','                                                       
         DC    X'01',C','                                                       
         DC    X'01',C','                                                       
         SPACE 1                                                                
SAEDDS   MVC   FVMSGNO,=AL2(CE#UPDDS)                                           
         MVC   FVOSYS,ASSYSE                                                    
         LA    R1,DDSGODH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
         B     NO                  CANNOT UPDATE DDS PASSWORD                   
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
*                                                                               
ECHOALL  MVC   FVIFLD-FVIHDR(L'CT@ALL,R1),CT@ALL                                
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BR    RE                                                               
         SPACE 1                                                                
       ++INCLUDE SEDDSDEPT                                                      
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    32C' '                                                           
FFILL    DC    32X'FF'                                                          
CAPFILL  DC    (L'APWORK)X'40'                                                  
* SEACSDICT                                                                     
       ++INCLUDE SEACSDICT                                                      
         LTORG                                                                  
         SPACE 1                                                                
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
SAVERE   DS    F                                                                
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
WORK     DS    CL32                                                             
*                                                                               
BLOCK    DS    CL800                                                            
*                                                                               
LOCALX   EQU   *                                                                
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSAED                                                       
         ORG   SAVOVER                                                          
SYSTEM   DS    X                                                                
DISSYS   DS    X                                                                
NEXSYS   DS    X                                                                
DDSDPLV  DS    X                                                                
DDSLEVL  DS    X                                                                
DDSDEPT  DS    X                                                                
                                                                                
         EJECT                                                                  
* DDPARSNIPD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDPARSNIPD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075SEACS14   03/10/04'                                      
         END                                                                    
