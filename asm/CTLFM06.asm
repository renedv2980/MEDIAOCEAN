*          DATA SET CTLFM06    AT LEVEL 009 AS OF 05/01/02                      
*PHASE TA0206A                                                                  
         TITLE 'CTLFM06 - CONTROL FILE MAINT - PROFILE RECORDS'                 
CTLFM06  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**LFM6**,RA,RR=RE                                      
         USING WRKD,RC             RC=A(OVERLAY TEMP W/S)                       
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,REC                                                           
         ST    R4,AREC                                                          
         USING CTPREC,R4           R4=A(RECORD)                                 
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
KEYVAL   XC    CTPKEY,CTPKEY                                                    
         MVI   CTPKTYP,C'P'                                                     
         SPACE 2                                                                
         GOTO1 AFVAL,PROSYSTH      VALIDATE SYSTEM                              
         BZ    EXIT                MUST BE INPUT                                
         SR    R1,R1               GET L'INPUT FOR COMPARE                      
         IC    R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         L     R5,ASYSTBL                                                       
         USING SYSLSTD,R5          R5=A(SYSTEMS LIST)                           
KEYV2    CLI   SYSLNUM,0           END OF TABLE                                 
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),SYSLNAME     COMPARE INPUT WITH TABLE                     
         BE    *+12                                                             
         LA    R5,SYSLLEN(R5)                                                   
         B     KEYV2                                                            
         CLI   SYSLRPLT,C' '       MUST HAVE A KEY LETTER                       
         BE    EIIF                                                             
         MVC   CTPKSYS,SYSLRPLT    MOVE SYSTEM TO KEY                           
         XC    PROSYST,PROSYST     RE-DISPLAY SYSTEM                            
         MVC   PROSYST,SYSLNAME                                                 
         OI    PROSYSTH+6,X'80'                                                 
         DROP  R5                                                               
         SPACE 2                                                                
         GOTO1 AFVAL,PROPRGMH      VALIDATE PROGRAM                             
         BZ    EXIT                MUST BE INPUT                                
         CLI   FLDH+5,2            AND LENGTH OF 2                              
         BNE   EFTS                                                             
         MVC   CTPKPROG,FLD        MOVE PROGRAM TO KEY                          
         SPACE 2                                                                
         MVI   IFLAG,0                                                          
         XC    IDNUM,IDNUM                                                      
         NI    PROSDSCH+1,X'FF'-X'20'                                           
         OI    PROFRMCH+1,X'20'                                                 
         MVI   KLEVEL,1                                                         
         GOTO1 AFVAL,PROORGNH      VALIDATE ORIGIN-ID                           
*&&US*&& BZ    KEYV4               OPTIONAL INPUT                               
*&&UK*&& BZ    EXIT                                                             
         CLI   FLDH+5,3            LENGTH NOT LESS THAN 3                       
         BL    EFTS                                                             
*&&UK                                                                           
         BNE   *+14                                                             
         CLC   PROORGN(3),=C'ALL'  TEST 'ALL' INPUT                             
         BE    KEYV4                                                            
*&&                                                                             
         MVC   KEYSAVE,CTPKEY      AND SAVE KEY                                 
         LR    R5,R4                                                            
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY       AND BUILD A KEY                              
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,FLD          MOVE ID TO KEY                               
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   ERNF                                                             
         LA    R5,CTIDATA          LOOK FOR ID# ELEMENT                         
         SR    R1,R1                                                            
KEYV3    CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'02'                                                      
         BE    *+14                                                             
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     KEYV3                                                            
         MVC   IDNUM,2(R5)                                                      
         MVI   KLEVEL,2                                                         
         OI    PROSDSCH+1,X'20'                                                 
         NI    PROFRMCH+1,X'FF'-X'20'                                           
         OI    IFLAG,X'80'         SET USERID INPUT                             
         DROP  R5                                                               
         MVC   CTPKEY,KEYSAVE                                                   
         B     KEYV5                                                            
*                                                                               
KEYV4    XC    PROFRMC,PROFRMC                                                  
*                                                                               
KEYV5    MVC   KEY,CTPKEY                                                       
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         MVC   KEYNEXT,KEY                                                      
         XC    PROTYPE,PROTYPE     SET DEFAULT PROFILE TYPE                     
         SPACE 2                                                                
         GOTO1 AFVAL,PROPROTH      VALIDATE PROFILE TYPE                        
         BNZ   *+18                                                             
         MVI   PROPROT,C'P'                                                     
         MVI   PROPROTH+5,1                                                     
         OI    PROPROTH+6,X'80'                                                 
         BASR  RE,RF                                                            
         XC    PROTYPE,PROTYPE                                                  
         MVC   PROTYPE(1),FLD                                                   
         MVI   FNDX,1                                                           
         CLI   FLDH+5,1            P/D/S/T                                      
         BE    KEYV6                                                            
         CLI   FLD,C'D'            D,XXX                                        
         BE    KEYV8                                                            
         CLI   FLD,C'T'            T,DDMMMYY                                    
         BE    KEYV12                                                           
         B     EIIF                                                             
KEYV6    MVI   FNDX,2                                                           
         CLI   PROTYPE,C'D'        DAY                                          
         BE    EMIF                                                             
         CLI   PROTYPE,C'T'        TEMP                                         
         BE    EMIF                                                             
         MVI   FNDX,1                                                           
         CLI   PROTYPE,C'P'        PERM                                         
         BE    VALEND                                                           
         CLI   PROTYPE,C'S'        SACRED                                       
         BE    VALEND                                                           
         B     EIIF                                                             
KEYV8    OI    IFLAG,X'40'         D,XXXX VALIDATION                            
         CLI   FLDH+5,4                                                         
         BL    EFTS                CHECK LENGTH                                 
         SR    R1,R1               AND GET IT FOR COMPARE                       
         IC    R1,FLDH+5                                                        
         SH    R1,=H'3'                                                         
         LA    R5,DAYTAB                                                        
         MVI   FNDX,2                                                           
KEYV10   CLI   0(R5),0             END OF DAY TABLE                             
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),FLD+2       COMPARE INPUT WITH TABLE ENTRY               
         BE    *+12                                                             
         LA    R5,L'DAYTAB(R5)                                                  
         B     KEYV10                                                           
         MVC   PROTYPE+1(3),0(R5)  SET-UP PROTYPE PARAMETER                     
         B     VALEND                                                           
KEYV12   OI    IFLAG,X'40'                                                      
         CLI   FLDH+5,7                                                         
         BE    KEYV14                                                           
         CLI   FLDH+5,8            T,DDMMMYY VALIDATION                         
         BL    EFTS                CHECK LENGTH                                 
         GOTO1 VDATVAL,DMCB,(0,FLD+2),WORK                                      
         OC    DMCB(4),DMCB                                                     
         MVI   FNDX,2              INCORRECT DATE                               
         BZ    EIIF                                                             
*                                  CONVERT YYMMDD DATE TO PWOS YMD              
         GOTO1 VDATCON,DMCB,(0,WORK),(1,PROTYPE+1)                              
         B     VALEND                                                           
KEYV14   CLC   FLD+2(5),=C'TODAY'                                               
         BNE   EIIF                                                             
         L     RF,APARM                                                         
         L     RF,12(RF)           RF=A(COMFACS)                                
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)            EXTRACT DATE FROM FACTSD                     
         MVC   DUB,FADATE-FACTSD(RF)                                            
         GOTO1 VDATCON,DMCB,(4,DUB),(1,PROTYPE+1)                               
         SPACE 2                                                                
VALEND   LA    R1,PROSYSTH                                                      
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         CLI   ACTN,CHANGE         CANT CHANGE KEY ON ACTN CHANGE               
         BNE   VALEN2                                                           
         MVI   ACTN,DISPLAY        SET ACTN TO DISP IF KEY CHANGED              
         CLC   KEY(22),LKEY                                                     
         BNE   VALEN2                                                           
         CLC   PROTYPE,LASTPRO     PROFILE MUST BE SAME ALSO                    
         BNE   VALEN2                                                           
         CLC   IDNUM,LIDNUM                                                     
         BNE   VALEN2                                                           
         MVI   ACTN,CHANGE         RESET ACTN IF KEYS OK                        
VALEN2   CLI   ACTN,ADD                                                         
         BNE   *+12                                                             
         CLI   KLEVEL,1            CANT ADD SYS/PRG/ID RECORD                   
         BNE   EFNV                                                             
         CLI   ACTN,COPY                                                        
         BNE   *+12                                                             
         CLI   KLEVEL,1            CANT COPY SYS/PRG/ID RECORD                  
         BNE   EFNV                                                             
         CLI   KLEVEL,1                                                         
         BNE   *+16                                                             
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD               READ SYS/PRG RECORD INTO REC                 
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'                                                     
         BZ    VALEN4                                                           
         CLI   ACTN,ADD            N/F ONLY VALID FOR ADD/COPY                  
         BE    DATAVAL                                                          
         CLI   ACTN,COPY                                                        
         BE    DATAVAL                                                          
         B     ERNF                                                             
VALEN4   CLI   ACTN,ADD            CANT EXIST FOR ADD/COPY                      
         BE    ERAE                                                             
         CLI   ACTN,COPY                                                        
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        DELETED REC CAN ONLY BE DISPLAYED            
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY                                                     
         BNE   ERNF                                                             
         CLI   KLEVEL,1                                                         
         BNE   VALEN6                                                           
         CLI   ACTN,DISPLAY                                                     
         BE    DISPREC                                                          
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
VALEN6   MVC   REC2(L'CTPKEY),REC                                               
         LA    R4,REC2                                                          
         MVC   CTPKORIG,IDNUM      MOVE ID TO KEY                               
         MVC   KEY,CTPKEY                                                       
         MVC   KEYSAVE,KEY                                                      
         MVC   KEYNEXT,KEY                                                      
         ST    R4,AREC                                                          
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD               READ SYS/PRG/ID RECORD INTO REC2             
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'                                                     
         BZ    VALEN8                                                           
         CLI   ACTN,CHANGE                                                      
         BNE   *+16                                                             
         MVI   ACTN,ADD                                                         
         MVI   KLEVEL,3                                                         
         B     DATAVAL                                                          
         MVI   KLEVEL,1                                                         
         LA    R4,REC                                                           
         ST    R4,AREC                                                          
         B     DISPREC                                                          
VALEN8   TM    DMCB+8,X'02'                                                     
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY                                                     
         BNE   ERNF                                                             
         CLI   ACTN,DISPLAY                                                     
         BE    DISPREC                                                          
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         EJECT                                                                  
*              CONTROL FOR DISPLAY                                              
*                                                                               
DISPREC  TWAXC PROVALSH                                                         
         LA    R1,PRODSCLH         CLEAR PROT INDICS                            
         BAS   RE,CLEARP                                                        
         BAS   RE,CLPRIVAL                                                      
         BAS   RE,LDPRISAV                                                      
         XC    VLIST,VLIST                                                      
         MVI   DISPTYPE,C'D'       SET TYPE TO DEFAULT                          
         LA    R4,REC                                                           
         ST    R4,AREC                                                          
         LA    R7,DLIST                                                         
         BAS   RE,EXTRACT          EXTRACT PROFILE TYPES                        
         BAS   RE,DISPRO                                                        
         CLI   KLEVEL,1                                                         
         BE    DISPEND                                                          
         BAS   RE,LDPRISAV                                                      
         BAS   RE,CLPRIVAL                                                      
         MVI   DISPTYPE,C'O'       SET TYPE TO OVERRIDE                         
         LA    R4,REC2                                                          
         ST    R4,AREC                                                          
         LA    R7,OLIST                                                         
         BAS   RE,EXTRACT          EXTRACT PROFILE TYPES                        
         BAS   RE,DISPRO                                                        
         B     DISPEND                                                          
*                                  CLEAR PRIORITY CODE VALUES                   
CLPRIVAL XC    PRISCVAL,PRISCVAL                                                
         XC    PRIPTVAL,PRIPTVAL                                                
         XC    PRIPOVAL,PRIPOVAL                                                
         BR    RE                                                               
*                                  LOAD PRIORITY CODE SAVE VALUES               
LDPRISAV MVC   PRISCSAV,PRISCVAL                                                
         MVC   PRIPTSAV,PRIPTVAL                                                
         MVC   PRIPOSAV,PRIPOVAL                                                
         BR    RE                                                               
         EJECT                                                                  
*              DISPLAY REQUESTED PROFILE INFO                                   
*                                                                               
DISPRO   NTR1                                                                   
         LA    R5,CTPDATA          R5=A(FIRST ELEMENT)                          
DISPR2   CLI   0(R5),0             END OF RECORD                                
         BE    DISPR12                                                          
         CLI   0(R5),X'01'         IGNORE ACTIVITY ELEMENT                      
         BE    DISPR4                                                           
         CLI   0(R5),X'02'         DESCRIPTION ELEMENT                          
         BE    DISPDIS                                                          
*                                  ALL OTHER ELEMENTS S/B PROFILES              
         MVC   WORK(1),0(R5)                                                    
         NI    WORK,X'F0'                                                       
         CLI   WORK,X'40'          IS THIS A PROFILE ELEMENT                    
         BNE   DISPR4              NO - IGNORE                                  
         CLC   2(1,R5),PROTYPE       ELSE FILTER ON PROFILE TYPE/DATA           
         BE    DISPR3                                                           
         CLI   0(R5),CTPRIELQ        UNLESS PRIORITY CODE EL.                   
         BNE   DISPR4                                                           
         CLI   2(R5),C'P'            AND PERMANENT TYPE ELEMENT                 
         BNE   DISPR4                                                           
         B     DISPR7                  THEN GO STRAIGHT TO PROCESSOR            
*                                                                               
DISPR3   OC    PROTYPE+1(3),PROTYPE+1                                           
         BZ    DISPR6                                                           
         CLC   3(3,R5),PROTYPE+1                                                
         BE    DISPR6                                                           
DISPR4   SR    R6,R6               BUMP TO NEXT ELEMENT                         
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DISPR2                                                           
DISPR6   MVC   PROTYPE+1(3),3(R5)  FILL IN PROFILE DATA IF N/I                  
DISPR7   LA    R6,ELTAB            R6=A(ELEMENT TABLE)                          
DISPR8   CLC   0(1,R6),0(R5)       MATCH ELEMENT TYPE WITH TABLE                
         BE    DISPR10                                                          
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF ELEMENT NOT KNOWN                     
         LA    R6,L'ELTAB(R6)                                                   
         B     DISPR8                                                           
DISPR10  L     RF,0(R6)            A(PROCESSOR)                                 
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     DISPR4              GET NEXT ELEMENT                             
DISPR12  XIT1                                                                   
         SPACE 2                                                                
         DS    0F                                                               
ELTAB    DS    0CL4                                                             
         DC    X'40',AL3(DISPDES)                                               
         DC    X'41',AL3(DISPATN)                                               
         DC    X'42',AL3(DISPOUT)                                               
         DC    X'43',AL3(DISPMOD)                                               
         DC    X'44',AL3(DISPPR)                                                
         DC    X'45',AL3(DISPRCL)                                               
         DC    X'46',AL3(DISPSRT)                                               
         DC    X'47',AL3(DISPPQP)                                               
         DC    X'48',AL3(DISPPRO)                                               
         DC    X'49',AL3(DISPPQC)                                               
         DC    X'4A',AL3(DISPPAK)                                               
         DC    X'4B',AL3(DISPSDS)                                               
         DC    X'4C',AL3(DISPSLI)                                               
         DC    X'4D',AL3(DISPJCL)                                               
         DC    X'4E',AL3(DISPEXP)                                               
         DC    X'4F',AL3(DISPFRM)                                               
         DC    X'00',AL3(0)                                                     
         EJECT                                                                  
DISPDIS  DS    0H                  PROFILE DESCRIPTION                          
         XC    PROPROD,PROPROD                                                  
         SR    R6,R6                                                            
         IC    R6,1(R5)                                                         
         SH    R6,=H'3'                                                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   PROPROD(0),2(R5)                                                 
         MVC   PRODSCL+L'PRODSCL-1(1),DISPTYPE                                  
         B     DISPR4                                                           
DISPDES  DS    0H                  DESTINATION ID                               
         ST    RE,DUB                                                           
         XC    PRODSTI,PRODSTI                                                  
         USING CTDCOD,R5                                                        
         LA    R6,IOAREA                                                        
         ST    R6,AREC                                                          
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF NUMERIC ID REC                  
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID+8(2),CTDCNUM                                              
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD               READ NUMERIC ID REC                          
         TM    DMCB+8,X'12'        TEST FOR DELETED/NOT FOUND                   
         BNZ   DISPDE4                                                          
         CLI   DMCB+8,0                                                         
         BNE   EIIO                MUST BE OK                                   
         LA    R6,CTIDATA                                                       
         SR    RF,RF                                                            
DISPDE2  CLI   0(R6),0             LOCATE ALPHA ID POINTER ELEMENT              
         BNE   *+6                                                              
         DC    H'0'                DIE IF NOT FOUND                             
         CLI   0(R6),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     DISPDE2                                                          
         MVC   PRODSTI,2(R6)       MOVE ALPHA ID TO SCREEN                      
         ST    R4,AREC             RESTORE A(IO)                                
         MVC   PRODESL+L'PRODESL-1(1),DISPTYPE                                  
         L     RE,DUB                                                           
         BR    RE                                                               
DISPDE4  L     RE,DUB                                                           
         MVI   PRODSTI,C'#'                                                     
         LA    R6,PRODSTI+1                                                     
         EDIT  (B2,CTDCNUM),(4,0(R6)),ALIGN=LEFT,FILL=0                         
         MVI   PRODESL+L'PRODESL-1,C'*'                                         
         ST    R4,AREC                                                          
         BR    RE                                                               
         DROP  R6                                                               
DISPATN  DS    0H                  ATTENTION TYPE                               
         XC    PROATYP,PROATYP                                                  
         USING CTACOD,R5                                                        
         MVC   PROATYP,CTACODE                                                  
         MVC   PROATTL+L'PROATTL-1(1),DISPTYPE                                  
         BR    RE                                                               
DISPOUT  DS    0H                  OUTPUT ID                                    
         XC    PROOUTP,PROOUTP                                                  
         USING CTOCOD,R5                                                        
         MVC   PROOUTP,CTOCODE                                                  
         MVC   PROOUTL+L'PROOUTL-1(1),DISPTYPE                                  
         BR    RE                                                               
DISPMOD  DS    0H                  OUTPUT MODE                                  
         XC    PROMODP,PROMODP                                                  
         USING CTOCOD,R5                                                        
         MVC   PROMODP,CTOCODE                                                  
         MVC   PROMODL+L'PROMODL-1(1),DISPTYPE                                  
         BR    RE                                                               
DISPPQP  DS    0H                  PRTQUE PASSWORD                              
         XC    PROPQPW,PROPQPW                                                  
         USING CTPQPD,R5                                                        
         MVC   PROPQPW,CTPQPWD                                                  
         MVC   PROPQPL+L'PROPQPL-1(1),DISPTYPE                                  
         BR    RE                                                               
DISPPQC  DS    0H                  PRTQUE RETAIN CLASS                          
         XC    PROPQRC,PROPQRC                                                  
         USING CTPQCD,R5                                                        
         MVC   PROPQRC,CTPQCLAS                                                 
         MVC   PROPQRL+L'PROPQRL-1(1),DISPTYPE                                  
         BR    RE                                                               
         EJECT                                                                  
DISPPR   DS    0H                  PRIORITY CODE/PRG TYPE/PRG OUTPUT            
         USING CTPRID,R5                                                        
         ST    RE,DUB                                                           
DPPR010  CLI   CTPRILEN,8          TEST OLD STYLE ELEMENT                       
         BNE   DPPR020             NO                                           
         CLC   2(1,R5),PROTYPE                                                  
         BNE   DPPRX                                                            
         MVC   PROPRSC,CTPRISC     YES ONLY 1ST CHR VALID                       
         MVC   PROPRTL+L'PROPRTL-1(1),DISPTYPE                                  
         B     DPPRX                                                            
*                                                                               
DPPR020  EQU   *                                                                
         CLI   CTPRISC,C' '        SORT CODE                                    
         BE    DPPR030                                                          
         CLC   2(1,R5),PROTYPE                                                  
         BE    DPPR022                                                          
         MVC   PRISCSAV,CTPRISC                                                 
         CLI   PRISCVAL,0                                                       
         BNE   DPPR030                                                          
         MVC   BYTE,DISPTYPE                                                    
         NI    BYTE,X'FF'-X'40'                                                 
         MVC   PROPRTL+L'PROPRTL-1(1),BYTE                                      
         B     *+10                                                             
DPPR022  MVC   PROPRTL+L'PROPRTL-1(1),DISPTYPE                                  
         MVC   PRISCVAL,CTPRISC                                                 
         MVC   PROPRSC,CTPRISC                                                  
*                                                                               
DPPR030  CLI   CTPRIPT,C' '        PROGRAM TYPE                                 
         BE    DPPR040                                                          
         CLC   2(1,R5),PROTYPE                                                  
         BE    DPPR032                                                          
         MVC   PRIPTSAV,CTPRIPT                                                 
         CLI   PRIPTVAL,0                                                       
         BNE   DPPR040                                                          
         MVC   BYTE,DISPTYPE                                                    
         NI    BYTE,X'FF'-X'40'                                                 
         MVC   PROPPTL+L'PROPPTL-1(1),BYTE                                      
         B     *+10                                                             
DPPR032  MVC   PROPPTL+L'PROPPTL-1(1),DISPTYPE                                  
         MVC   PRIPTVAL,CTPRIPT                                                 
         MVC   PROPRPT,CTPRIPT                                                  
         BAS   RE,DPRTCODE                                                      
*                                                                               
DPPR040  CLI   CTPRIPO,C' '        PROGRAM PROFILE                              
         BE    DPPRX                                                            
         CLC   2(1,R5),PROTYPE                                                  
         BE    DPPR042                                                          
         MVC   PRIPOSAV,CTPRIPO                                                 
         CLI   PRIPOVAL,0                                                       
         BNE   DPPRX                                                            
         MVC   BYTE,DISPTYPE                                                    
         NI    BYTE,X'FF'-X'40'                                                 
         MVC   PROPPOL+L'PROPPOL-1(1),BYTE                                      
         B     *+10                                                             
DPPR042  MVC   PROPPOL+L'PROPPOL-1(1),DISPTYPE                                  
         MVC   PRIPOVAL,CTPRIPO                                                 
         MVC   PROPRPO,CTPRIPO                                                  
         BAS   RE,DPROCODE                                                      
*                                                                               
DPPRX    L     RE,DUB                                                           
         BR    RE                                                               
         EJECT                                                                  
DISPRCL  DS    0H                  READER CLASS                                 
         XC    PRORCLA,PRORCLA                                                  
         USING CTRCLD,R5                                                        
         MVC   PRORCLA,CTRCLASS                                                 
         MVC   PRORDCL+L'PRORDCL-1(1),DISPTYPE                                  
         BR    RE                                                               
DISPSRT  NTR1                                                                   
         XC    PROSRTF,PROSRTF                                                  
         USING CTSRTD,R5           SORT FORMULA                                 
         SR    R6,R6                                                            
         IC    R6,CTSRTLEN         ELEMENT LENGTH                               
         SH    R6,=H'6'            -6                                           
         SRL   R6,1                /2=LOOP COUNT                                
         LA    R7,CTSRTFRM         R7=A(FIRST PARAM)                            
         LA    R8,PROSRTF          R8=A(SCREEN LINE)                            
DISPSR2  MVC   DUB(2),0(R7)        GET SORT PARAM                               
         XC    DUB1,DUB1                                                        
         MVC   DUB1+3(1),DUB       START COLUMN                                 
         NI    DUB1+3,X'7F'        TURN OFF X'80' IF PRESENT                    
         MVC   DUB1+7(1),DUB+1     LENGTH                                       
         EDIT  (B4,DUB1),(3,0(R8)),ALIGN=LEFT                                   
         AR    R8,R0                                                            
         MVI   0(R8),C','                                                       
         LA    R8,1(R8)                                                         
         EDIT  (B4,DUB1+4),(2,0(R8)),ALIGN=LEFT                                 
         AR    R8,R0                                                            
         MVI   0(R8),C','                                                       
         LA    R8,1(R8)                                                         
         MVC   0(2,R8),=C'A,'                                                   
         TM    0(R7),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(R8),C'D'                                                       
         LA    R8,2(R8)            BUMP TO NEXT OUTPUT FIELD                    
         LA    R7,2(R7)            BUMP TO NEXT INPUT FIELD                     
         BCT   R6,DISPSR2                                                       
         BCTR  R8,0                                                             
         MVI   0(R8),0                                                          
         MVC   PROSRTL+L'PROSRTL-1(1),DISPTYPE                                  
         XIT1                                                                   
         EJECT                                                                  
DISPJCL  DS    0H                  SPECIAL JCL BOOK                             
         XC    PROSJCL,PROSJCL                                                  
         USING CTJCLD,R5                                                        
         MVC   PROSJCL,CTJCLEX                                                  
         MVC   PROJCLL+L'PROJCLL-1(1),DISPTYPE                                  
         BR    RE                                                               
         SPACE 1                                                                
DISPEXP  NTR1                      SPECIAL PHASES                               
         XC    PROSPHS,PROSPHS                                                  
         USING CTPHSD,R5                                                        
         MVI   LINES,C' '          CLEAR BLOCK                                  
         MVC   LINES+1(79),LINES                                                
         LA    R5,CTPHS01                                                       
         LA    R6,LINES                                                         
         LA    R7,1                                                             
         LA    R8,4                                                             
         SR    RE,RE                                                            
DISPEXP2 CLI   0(R5),C' '                                                       
         BE    DISPEXP4                                                         
         STC   R7,0(R6)            MOVE PHASE# AND TEST LEVEL TO BLOCK          
         OI    0(R6),X'F0'                                                      
         MVC   10(1,R6),0(R5)                                                   
         LA    R6,20(R6)                                                        
         LA    RE,1(RE)            BUMP ENTRY COUNT                             
DISPEXP4 LA    R5,1(R5)            UNSCAN BLOCK INTO TWA FIELD                  
         LA    R7,1(R7)                                                         
         BCT   R8,DISPEXP2                                                      
         LR    R5,RE                                                            
         GOTO1 VUNSCAN,DMCB,((R5),LINES),PROSPHSH                               
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                DIE IF DATA WON'T FIT                        
         MVC   PROPHSL+L'PROPHSL-1(1),DISPTYPE                                  
         XIT1                                                                   
DISPPRO  DS    0H                  PROCESSING INSTRS                            
         USING CTPRCD,R5                                                        
         LA    R6,PROPRCAH                                                      
         LA    R7,PROPRCBH                                                      
         MVC   PROPROL+L'PROPROL-1(1),DISPTYPE                                  
         B     DISPNAR                                                          
DISPPAK  DS    0H                  PACKING INSTRS                               
         LA    R6,PROPACAH                                                      
         LA    R7,PROPACBH                                                      
         MVC   PROPACL+L'PROPACL-1(1),DISPTYPE                                  
         B     DISPNAR                                                          
DISPSLI  DS    0H                  PACKING SLIP INSTRS                          
         LA    R6,PROPINAH                                                      
         LA    R7,PROPINBH                                                      
         MVC   PROSHPL+L'PROSHPL-1(1),DISPTYPE                                  
         B     DISPNAR                                                          
DISPNAR  SR    R1,R1               GENERAL 2 LINE NARRATIVE HANDLER             
         XC    8(59,R6),8(R6)                                                   
         XC    8(59,R7),8(R7)                                                   
         IC    R1,1(R5)                                                         
         SH    R1,=H'6'            R5=L'NARRATIVE-1                             
         SR    R8,R8                                                            
         C     R1,=F'60'           MAX L'SCREEN LINE                            
         BNH   DISPNA2                                                          
         LR    R8,R1                                                            
         LA    R1,60               R1=L'FIRST LINE DATA                         
         SR    R8,R1               R8=L'SECOND LINE DATA                        
DISPNA2  C     R1,=F'60'           ADJUST FOR 59 CHAR FIELD                     
         BL    *+6                                                              
         BCTR  R1,0                                                             
         CLI   0(R5),X'4C'         TEST SHIPPING INSTRUCTIONS                   
         BNE   DISPNA3                                                          
         TM    6(R5),X'80'         TEST SPECIAL NUMERIC SHIPPING UNIT           
         BO    DISPNA3                                                          
         ST    R1,DMCB+8           STORE LENGTH IN PARAMETER 3                  
         ST    RE,SAVEDRE                                                       
         GOTO1 ADISPSHP,DMCB,8(R6),6(R5)   CALL SPECIAL ROUTINE                 
         L     RE,SAVEDRE                                                       
         B     DISPNA4                                                          
*                                                                               
DISPNA3  BCTR  R1,0                OTHERWISE MOVE IN AS IS                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R6),6(R5)       MOVE LINE 1 NARR TO SCREEN                   
*                                                                               
DISPNA4  LA    R1,66(R5)           R1=A(NEXT CHUNK OF NARRATIVE)                
         LTR   R8,R8                                                            
         BZR   RE                                                               
         C     R8,=F'60'           ADJUST FOR 59 CHAR FIELD                     
         BL    *+6                                                              
         BCTR  R8,0                                                             
         BCTR  R8,0                                                             
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R7),0(R1)       MOVE LINE 2 NARR TO SCREEN                   
         BR    RE                                                               
         EJECT                                                                  
DISPSDS  DS    0H                  REPORT SHORT DESCRIPTION                     
         XC    PROSDSC,PROSDSC                                                  
         USING CTSDSD,R5                                                        
         MVC   PROSDSC,CTSDSTXT                                                 
         MVC   PROSDSL+L'PROSDSL-1(1),DISPTYPE                                  
         BR    RE                                                               
         SPACE 1                                                                
DISPFRM  DS    0H                  REPORT FORM CODE                             
         XC    PROFRMC,PROFRMC                                                  
         USING CTFRMD,R5                                                        
         MVC   PROFRMC,CTFRMCOD                                                 
         MVC   PROFRML+L'PROFRML-1(1),DISPTYPE                                  
         BR    RE                                                               
         EJECT                                                                  
*              SET NEXT ACTIONS AND EXIT                                        
*                                                                               
DISPEND  MVC   LASTPRO,PROTYPE     SAVE DISPLAYED PROFILE TYPE                  
         MVC   LIDNUM,IDNUM                                                     
         BAS   RE,DISPVALS         DISPLAY PROFILE TYPES                        
         TM    CTPSTAT,X'80'                                                    
         BO    DISPE4                                                           
         MVI   NACTN,OKCHA+OKDEL+OKCOPY                                         
         LA    R1,PROPRODH                                                      
         ST    R1,FADR                                                          
         B     DISPXX                                                           
DISPE4   MVI   NACTN,OKRES                                                      
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
DISPXX   MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
*              UPDATE/ADD PROFILE RECORD                                        
*                                                                               
DATAVAL  CLI   ACTN,CHANGE                                                      
         BE    DATAV2                                                           
         CLI   ACTN,ADD                                                         
         BE    DATAV0                                                           
*                                  COPY RECORD                                  
         MVC   KEYSAVE,KEY         RESTORE LAST KEY & READ RECORD               
         MVC   KEY,LKEY                                                         
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0            MUST BE OK                                   
         BNE   EIIO                                                             
         MVI   TEMP,X'01'          DELETE ACTIVITY ELEMENT                      
         GOTO1 ADELEL                                                           
         GOTO1 ABLDACT             AND ADD A NEW ONE                            
         GOTO1 APUTEL                                                           
         MVC   CTPKEY,KEYSAVE      RESTORE COPY-TO KEY                          
         MVC   KEY,CTPKEY                                                       
         B     DATAV26             GO AND ADD RECORD                            
*                                                                               
DATAV0   DS    0H                                                               
         MVC   LASTPRO,PROTYPE                                                  
         MVC   LIDNUM,IDNUM                                                     
         MVC   KEY,KEYSAVE         ADD                                          
         MVI   TEMP,0                                                           
         GOTO1 ABLDREC             BUILD A KEY+LENGTH                           
         B     DATAV6                                                           
DATAV2   MVI   TEMP,X'01'          CHANGE                                       
         GOTO1 ADELEL              DELETE ACTIVITY ELEMENT                      
         MVI   TEMP,X'02'          DELETE DESCRIPTION ELEMENT                   
         GOTO1 ADELEL                                                           
         LA    R6,LASTPRO          A(DATA)                                      
         LA    R5,4                L'DATA COMPARE                               
         LA    R7,ELTAB                                                         
*                                  DELETE ALL COMMON ELEMENTS FROM              
DATAV4   CLI   0(R7),0             RECORD                                       
         BE    DATAV6                                                           
         MVC   TEMP(1),0(R7)                                                    
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE '),(TEMP,(R4)),((R5),(R6))           
         LA    R7,4(R7)                                                         
         B     DATAV4                                                           
DATAV6   GOTO1 ABLDACT             BUILD ACTIVITY ELEMENT                       
         GOTO1 APUTEL              AND ADD TO RECORD                            
         BZ    EXIT                                                             
         SPACE 2                                                                
         GOTO1 AFVAL,PROPRODH      VALIDATE PROFILE DESCRIPTION                 
         BZ    DATAV7                                                           
         SR    R6,R6                                                            
         IC    R6,FLDH+5                                                        
         MVI   TEMP,X'02'          BUILD A DESCRIPTION ELEMENT                  
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+2(0),FLD       MOVE DESC TO ELEMENT                         
         LA    R6,3(R6)                                                         
         STC   R6,TEMP+1                                                        
         GOTO1 APUTEL              AND ADD TO RECORD                            
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV7   DS    0H                  BUILD ID KEY                                 
         LA    R6,IOAREA                                                        
         ST    R6,AREC                                                          
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         GOTO1 AFVAL,PRODSTIH         VALIDATE DEST ID                          
         BZ    DATAV7C                                                          
         CLI   FLDH+5,3                                                         
         BL    EIIF                                                             
         MVC   CTIKID,FLD          MOVE ID TO KEY                               
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   EIIF                                                             
         LA    R6,CTIDATA          LOOK FOR ID# ELEMENT (X'02')                 
         SR    R7,R7                                                            
DATAV7A  CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                DIE IF N/F                                   
         CLI   0(R6),X'02'                                                      
         BE    *+14                                                             
         IC    R7,1(R6)                                                         
         AR    R6,R7                                                            
         B     DATAV7A                                                          
         MVC   DUB(2),2(R6)        SAVE ID NUMBER                               
         ST    R4,AREC                                                          
         MVC   TEMP(2),=X'4012'    BUILD DEST CODE ELEMENT                      
         LA    R5,TEMP                                                          
         USING CTDCOD,R5                                                        
         MVC   CTDCOTYP(4),LASTPRO                                              
         MVC   CTDCODE,FLD                                                      
         MVC   CTDCNUM,DUB                                                      
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV7C  DS    0H                  VALIDATE ATTENTION TYPE                      
         ST    R4,AREC                                                          
         GOTO1 AFVAL,PROATYPH                                                   
         BZ    DATAV8                                                           
         MVC   TEMP(2),=X'4109'    BUILD ATTENTION TYPE ELEMENT                 
         LA    R5,TEMP                                                          
         USING CTACOD,R5                                                        
         MVC   CTACOTYP(4),LASTPRO                                              
         MVC   CTACODE,FLD                                                      
         GOTO1 APUTEL              AND ADD TO RECORD                            
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV8   DS    0H                  VALIDATE OUTPUT TYPE                         
         LA    R6,IOAREA                                                        
         ST    R6,AREC                                                          
         USING CTOREC,R6                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKEY,C'O'                                                      
         GOTO1 AFVAL,PROOUTPH                                                   
         BZ    DATAV9                                                           
         CLI   FLDH+5,3                                                         
         BL    EIIF                                                             
         MVC   CTOKID,FLD          MOVE ID TO KEY                               
         MVC   KEY,CTOKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   EIIF                                                             
         ST    R4,AREC                                                          
         MVC   TEMP(2),=X'4210'    BUILD OUTPUT TYPE ELEMENT                    
         LA    R5,TEMP                                                          
         USING CTOCOD,R5                                                        
         MVC   CTOCOTYP(4),LASTPRO                                              
         MVC   CTOCODE,FLD                                                      
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV9   DS    0H                  VALIDATE OUTPUT MODE                         
         LA    R6,IOAREA                                                        
         ST    R6,AREC                                                          
         USING CTOREC,R6                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKEY,C'O'                                                      
         GOTO1 AFVAL,PROMODPH                                                   
         BZ    DATAV9A                                                          
         CLI   FLDH+5,3                                                         
         BL    EIIF                                                             
         MVC   CTOKID,FLD          MOVE ID TO KEY                               
         MVC   KEY,CTOKEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   EIIF                                                             
         ST    R4,AREC                                                          
         MVC   TEMP(2),=X'4310'    BUILD OUTPUT MODE ELEMENT                    
         LA    R5,TEMP                                                          
         USING CTOCOD,R5                                                        
         MVC   CTOCOTYP(4),LASTPRO                                              
         MVC   CTOCODE,FLD                                                      
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV9A  ST    R4,AREC             RESTORE A(IO)                                
         GOTO1 AFVAL,PROPQPWH      VALIDATE PRTQUE PASSWORD                     
         BZ    DATAV9B                                                          
         OC    LIDNUM,LIDNUM       ONLY VALID IF USER SPECIFIC PROFILE          
         BZ    EIIF                                                             
         MVC   TEMP(2),=X'470C'    BUILD ELEMENT                                
         LA    R5,TEMP                                                          
         USING CTPQPD,R5                                                        
         MVC   CTPQPTYP(4),LASTPRO                                              
         MVC   CTPQPWD,FLD                                                      
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV9B  GOTO1 AFVAL,PROPQRCH      VALIDATE PRTQUE RETAIN CLASS                 
         BZ    DATAV10                                                          
         MVC   TEMP(2),=X'4907'    BUILD ELEMENT                                
         LA    R5,TEMP                                                          
         USING CTPQCD,R5                                                        
         MVC   CTPQCTYP(4),LASTPRO                                              
         MVC   CTPQCLAS,FLD                                                     
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
DATAV10  MVC   TEMP(2),=X'4409'    BUILD PRIORITY CODE ELEMENT                  
         LA    R5,TEMP                                                          
         USING CTPRID,R5                                                        
         MVC   CTPRITYP(4),LASTPRO                                              
         MVI   CTPRISC,C' '                                                     
         MVI   CTPRIPT,C' '                                                     
         MVI   CTPRIPO,C' '                                                     
*                                                                               
DTV10A   GOTO1 AFVAL,PROPRSCH      VALIDATE SORT CODE                           
         BNZ   DTV10A1                                                          
*&&US*&& CLI   IFLAG,0             REQUIRED IF MASTER PROFILE IN US             
*&&US*&& BE    EXIT                                                             
*&&US*&& B     DTV10B                                                           
*&&UK*&& B     DTV10B              OPTIONAL INPUT FIELD IN UK                   
DTV10A1  CLC   FLD(L'PRISCSAV),PRISCSAV                                         
         BE    DTV10B                                                           
         CLI   FLD,C'A'                                                         
         BL    EIIF                                                             
         CLI   FLD,C'9'                                                         
         BH    EIIF                                                             
         MVC   CTPRISC,FLD         SET SORT CODE                                
*                                                                               
DTV10B   XC    PROPXPT,PROPXPT     VALIDATE PROGRAM TYPE                        
         OI    PROPXPTH+6,X'80'                                                 
         GOTO1 AFVAL,PROPRPTH                                                   
         BNZ   DTV10B1                                                          
*&&US*&& CLI   IFLAG,0             REQUIRED IF MASTER PROFILE IN US             
*&&US*&& BE    EXIT                                                             
*&&US*&& B     DTV10C                                                           
*&&UK*&& BZ    DTV10C              OPTIONAL FIELD IN UK                         
DTV10B1  CLC   FLD(L'PRIPTSAV),PRIPTSAV                                         
         BE    DTV10B4                                                          
         CLI   FLD,C'A'                                                         
         BL    EIIF                                                             
         CLI   FLD,C'9'                                                         
         BH    EIIF                                                             
         CLI   FLD,C'Z'            Z VALID ONLY IF USER ID INPUT                
         BNE   DTV10B2                                                          
         OC    IDNUM,IDNUM                                                      
         BZ    EIIF                                                             
DTV10B2  MVC   CTPRIPT,FLD                                                      
DTV10B4  BAS   RE,DPRTCODE                                                      
*                                                                               
DTV10C   XC    PROPXPO,PROPXPO     VALIDATE PROGRAM OUTPUT                      
         OI    PROPXPOH+6,X'80'                                                 
         GOTO1 AFVAL,PROPRPOH                                                   
         BZ    DTV10D                                                           
         CLC   FLD(L'PRIPOSAV),PRIPOSAV                                         
         BE    DTV10C4                                                          
         CLI   FLD,C'A'                                                         
         BL    EIIF                                                             
         CLI   FLD,C'9'                                                         
         BH    EIIF                                                             
         CLI   FLD,C'Z'            Z VALID ONLY IF USER ID INPUT                
         BNE   DTV10C2                                                          
         OC    IDNUM,IDNUM                                                      
         BZ    EIIF                                                             
DTV10C2  MVC   CTPRIPO,FLD                                                      
DTV10C4  BAS   RE,DPROCODE                                                      
*                                                                               
DTV10D   CLC   CTPRISC(3),=C'   '  TEST IF ALL THREE FIELDS BLANK               
         BE    DATAV11             YES THEN OMIT ELEMENT                        
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
DATAV11  GOTO1 AFVAL,PRORCLAH      VALIDATE READER CLASS                        
         BZ    DATAV12                                                          
         LA    R6,TABCLA                                                        
         MVI   FERN,2                                                           
DATAV11A CLI   0(R6),X'FF'                                                      
         BE    EXIT                                                             
         CLC   0(1,R6),FLD                                                      
         BE    *+12                                                             
         LA    R6,1(R6)                                                         
         B     DATAV11A                                                         
         MVI   FERN,X'FF'                                                       
         MVC   TEMP(2),=X'4507'    BUILD READER CLASS ELEMENT                   
         LA    R5,TEMP                                                          
         USING CTRCLD,R5                                                        
         MVC   CTRCLTYP(4),LASTPRO                                              
         MVC   CTRCLASS,FLD                                                     
         GOTO1 APUTEL              AND ADD TO RECORD                            
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV12  GOTO1 AFVAL,PROSRTFH      VALIDATE SORT FORMULA                        
         BZ    DATAV17                                                          
         MVC   TEMP(2),=X'4606'    BUILD SORT FORMULA ELEMENT                   
         LA    R5,TEMP                                                          
         USING CTSRTD,R5                                                        
         MVC   CTSRTTYP(4),LASTPRO                                              
         GOTO1 VSCANNER,DMCB,(0,PROSRTFH),(30,LINES)                            
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         IC    R7,DMCB+4                                                        
         LTR   R7,R7                                                            
         BZ    EIIF                                                             
         D     R6,=F'3'            NUMBER OF LINES INPUT MUST BE A              
         LTR   R6,R6               MULTIPLE OF 3                                
         BNZ   EIIF                                                             
*                                  R7=# OF MULTIPLES OF 3                       
         LA    R6,LINES            R6=A(INPUT)                                  
         LA    R8,CTSRTFRM         R8=A(OUTPUT)                                 
         MVI   FNDX,1                                                           
         SPACE 2                                                                
DATAV14  CLI   1(R6),0             VALIDATE START COLUMN                        
         BNE   EIIF                                                             
         TM    2(R6),X'80'                                                      
         BZ    EFNN                                                             
         OC    4(4,R6),4(R6)                                                    
         BZ    EIIF                                                             
         CLC   4(4,R6),=F'160'     WAS 80 (ONE CARD REQUESTS)                   
         BH    EIIF                                                             
         MVC   DUB(1),7(R6)                                                     
         LA    R6,32(R6)           NEXT LINE                                    
         BAS   RE,FNDUP            INCREMENT FNDX                               
         CLI   1(R6),0             VALIDATE LENGTH                              
         BNE   EIIF                                                             
         TM    2(R6),X'80'                                                      
         BZ    EFNN                                                             
         OC    4(4,R6),4(R6)                                                    
         BZ    EIIF                                                             
         CLC   4(4,R6),=F'80'                                                   
         BH    EIIF                                                             
         MVC   DUB+1(1),7(R6)                                                   
         SR    RE,RE                                                            
         IC    RE,DUB                                                           
         SR    R1,R1                                                            
         IC    R1,DUB+1                                                         
         AR    R1,RE                                                            
         C     R1,=F'161'          MAX FOR 2 CARD REQUESTS                      
         BH    EIIF                                                             
         LA    R6,32(R6)           NEXT LINE                                    
         BAS   RE,FNDUP            INCREMENT FNDX                               
         CLI   1(R6),0             VALIDATE ASCENDING/DESCENDING                
         BNE   EIIF                                                             
         CLI   0(R6),1                                                          
         BH    EFTL                                                             
         CLI   12(R6),C'A'                                                      
         BE    DATAV16                                                          
         CLI   12(R6),C'D'                                                      
         BNE   EIIF                                                             
         OI    DUB,X'80'                                                        
DATAV16  MVC   0(2,R8),DUB         MOVE PARAMETERS TO ELEMENT                   
         LA    R8,2(R8)                                                         
         LA    R6,32(R6)           NEXT LINE                                    
         BAS   RE,FNDUP            INCREMENT FNDX                               
         BCT   R7,DATAV14                                                       
         SR    R8,R5               R8=L'ELEMENT                                 
         STC   R8,1(R5)                                                         
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV17  GOTO1 AFVAL,PROSJCLH      SPECIAL JCL BOOK                             
         BZ    DATAV17A                                                         
         MVC   TEMP(2),=X'4D10'    BUILD SPECIAL JCL ELEMENT                    
         LA    R5,TEMP                                                          
         USING CTJCLD,R5                                                        
         MVC   CTJCLTYP(4),LASTPRO                                              
         MVC   CTJCLEX,FLD                                                      
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV17A GOTO1 AFVAL,PROSPHSH      SPECIAL PHASES                               
         BZ    DATAV18                                                          
         MVC   TEMP(2),=X'4E0A'    BUILD SPECIAL PHASE ELEMENT                  
         LA    R5,TEMP                                                          
         USING CTPHSD,R5                                                        
         MVC   CTPHSTYP(4),LASTPRO                                              
         MVI   CTPHS01,C' '        SET DEFAULT VALUES (SPACES)                  
         MVC   CTPHS02(3),CTPHS01                                               
*                                                                               
         GOTO1 VSCANNER,DMCB,(0,PROSPHSH),(4,LINES)                             
         CLI   DMCB+4,0                                                         
         BE    EIIF                                                             
         LA    R6,LINES                                                         
         MVI   FNDX,1                                                           
         SPACE 2                                                                
DATAV17B CLC   FNDX,DMCB+4                                                      
         BH    DATAV17C                                                         
         CLI   0(R6),1             L'PART1                                      
         BNE   EIIF                                                             
         CLI   12(R6),C'1'         V'PART1 (PHASE S/B 1-4)                      
         BL    EIIF                                                             
         CLI   12(R6),C'4'         V'PART1                                      
         BH    EIIF                                                             
         CLI   1(R6),1             L'PART2                                      
         BNE   EIIF                                                             
         CLI   22(R6),C'A'         V'PART2 (TEST LEVEL S/B A,B OR C)            
         BL    EIIF                                                             
         CLI   22(R6),C'C'         V'PART2                                      
         BH    EIIF                                                             
         SR    R1,R1                                                            
         IC    R1,7(R6)                                                         
         LA    R1,CTPHS01-1(R1)    POINT TO PHASE TEST LEVEL                    
         CLI   0(R1),C' '                                                       
         BNE   EDIF                                                             
         MVC   0(1,R1),22(R6)      SET TEST LEVEL                               
         BAS   RE,FNDUP            BUMP FNDX                                    
         LA    R6,32(R6)           AND BLOCK POINTER                            
         B     DATAV17B                                                         
         SPACE 2                                                                
DATAV17C MVI   FNDX,0                                                           
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV18  GOTO1 AFVAL,PROPRCAH      PROCESSING INSTRUCTIONS                      
         BZ    DATAV20                                                          
         MVC   TEMP(2),=X'4806'                                                 
         LA    R6,PROPRCAH                                                      
         LA    R7,PROPRCBH                                                      
         BAS   RE,BUILDINS                                                      
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV20  GOTO1 AFVAL,PROPACAH      BREAKDOWN INSTRUCTIONS                       
         BZ    DATAV22                                                          
         MVC   TEMP(2),=X'4A06'                                                 
         LA    R6,PROPACAH                                                      
         LA    R7,PROPACBH                                                      
         BAS   RE,BUILDINS                                                      
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV22  GOTO1 AFVAL,PROPINAH      SHIPPING UNIT AND ROUTE                      
         BZ    DATAV23                                                          
         MVC   TEMP(2),=X'4C06'                                                 
         LA    R6,PROPINAH                                                      
         LA    R7,PROPINBH                                                      
         BAS   RE,BUILDINS                                                      
         CLI   FTBFLAG,1           SET IF ERROR RETURNED FROM BLDSHP            
         BE    EFTB                                                             
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
DATAV23  GOTO1 AFVAL,PROSDSCH      REPORT SHORT DESCRIPTION                     
         BZ    DATAV23A                                                         
         MVI   TEMP,CTSDSELQ                                                    
         MVI   TEMP+1,CTSDSLNQ                                                  
         LA    R5,TEMP                                                          
         USING CTSDSD,R5                                                        
         MVC   CTSDSTYP(4),LASTPRO                                              
         MVC   CTSDSTXT,FLD                                                     
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         SPACE 2                                                                
DATAV23A GOTO1 AFVAL,PROFRMCH      REPORT FORM CODE                             
         BZ    DATAV24                                                          
         MVI   TEMP,CTFRMELQ                                                    
         MVI   TEMP+1,CTFRMLNQ                                                  
         LA    R5,TEMP                                                          
         USING CTFRMD,R5                                                        
         MVC   CTFRMTYP(4),LASTPRO                                              
         MVC   CTFRMCOD,FLD                                                     
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         EJECT                                                                  
DATAV24  MVC   KEY,KEYSAVE         UPDATE RECORD                                
         ST    R4,AREC                                                          
         CLI   KLEVEL,1                                                         
         BE    DATAV25                                                          
         MVI   WORK,0                                                           
         LA    R5,REC2+28          DELETE DUP ELS FROM REC2                     
DATAV24A CLI   0(R5),0                                                          
         BE    DATAV24E                                                         
         CLI   0(R5),X'01'                                                      
         BE    DATAV24D                                                         
         LA    R6,REC+28                                                        
DATAV24B CLI   0(R6),0                                                          
         BE    DATAV24D                                                         
         CLC   1(1,R5),1(R6)                                                    
         BNE   DATAV24C                                                         
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R5),2(R6)                                                    
         BNE   *+12                                                             
         MVI   0(R5),X'FF'                                                      
         OI    WORK,X'FF'                                                       
DATAV24C SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DATAV24B                                                         
DATAV24D SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DATAV24A                                                         
DATAV24E CLI   WORK,0                                                           
         BE    DATAV24F                                                         
         MVI   TEMP,X'FF'                                                       
         GOTO1 ADELEL                                                           
DATAV24F CLI   KLEVEL,3                                                         
         BNE   DATAV25                                                          
         MVI   ACTN,CHANGE                                                      
         CLC   CTPLEN,=H'34'       ANYTHING ON RECORD ?                         
         BH    *+12                                                             
         MVI   NACTN,OKCHA                                                      
         B     DATAVXXX                                                         
         GOTO1 AADD                ADD RECORD                                   
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     DATAVXX                                                          
         SPACE 2                                                                
DATAV25  CLI   ACTN,ADD                                                         
         BE    DATAV26                                                          
         GOTO1 AWRITE                                                           
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     DATAVXX                                                          
         SPACE 2                                                                
DATAV26  GOTO1 AADD                ADD RECORD                                   
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         SPACE 2                                                                
DATAVXX  MVI   NACTN,OKCHA+OKDEL+OKCOPY                                         
         SPACE 2                                                                
DATAVXXX MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
*              UPDATE FNDX                                                      
*                                                                               
FNDUP    SR    RF,RF                                                            
         IC    RF,FNDX                                                          
         LA    RF,1(RF)                                                         
         STC   RF,FNDX                                                          
         BR    RE                                                               
         SPACE 2                                                                
*              BUILD A NARRATIVE ELEMENT                                        
*                                                                               
BUILDINS NTR1                                                                   
         MVI   FTBFLAG,0           FIELD TOO BIG INDICATOR                      
         LA    R5,TEMP                                                          
         MVC   2(4,R5),LASTPRO                                                  
         CLI   TEMP,X'4C'          TEST SHIPPING INSTRUCTIONS                   
         BNE   BUILDIN1                                                         
         CLI   8(R6),C'0'          TEST SPECIAL NUMERIC SHIPPING UNIT           
         BL    BUILDIN1                                                         
         CLI   8(R6),C'9'                                                       
         BH    BUILDIN1                                                         
         ZIC   R6,FLDH+5                                                        
         GOTO1 ABLDSHP,DMCB,FLD,TEMP+6,(R6)  CALL SPECIAL ROUTINE               
         CLI   8(R1),0                                                          
         BNE   *+12                                                             
         MVI   FTBFLAG,1           FLAG FIELD TOO BIG                           
         B     EXIT                                                             
         ZIC   R6,8(R1)            R6 = LENGTH OF ELEMENT DATA                  
         B     BUILDIN2                                                         
*                                  OTHERWISE MOVE IN AS IS                      
BUILDIN1 MVC   6(60,R5),FLD        FIRST HALF OF NARRATIVE                      
         SR    R6,R6                                                            
         IC    R6,FLDH+5                                                        
*                                                                               
BUILDIN2 LR    R1,R7               R1=A(SECOND SCREEN LINE)                     
         SR    R8,R8                                                            
         GOTO1 AFVAL                                                            
         BZ    *+8                                                              
         IC    R8,FLDH+5                                                        
         LTR   R8,R8                                                            
         BZ    *+14                                                             
         LA    R6,60                                                            
         MVC   66(60,R5),FLD       DO NOT CONTATENATE NARRATIVE                 
         AR    R6,R8               R6=TOTAL L'NARRATIVE                         
         LA    R6,6(R6)                                                         
         STC   R6,1(R5)                                                         
         XIT1                                                                   
         SPACE 2                                                                
*              CLEAR DISPLAY INDICATOR IN PROTS                                 
*                                                                               
CLEARP   NTR1                                                                   
         SR    R2,R2                                                            
CLEARP2  CLI   0(R1),0                                                          
         BE    CLEARPX                                                          
         TM    1(R1),X'20'                                                      
         BZ    CLEARP3                                                          
         OI    6(R1),X'80'                                                      
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         BCTR  RE,0                                                             
         AR    RE,R1                                                            
         MVI   0(RE),0                                                          
CLEARP3  IC    R2,0(R1)                                                         
         AR    R1,R2                                                            
         B     CLEARP2                                                          
CLEARPX  XIT1                                                                   
         EJECT                                                                  
*              BUILD A LIST OF PROFILE TYPES FOR THIS REC R7=A(LIST)            
*                                                                               
EXTRACT  NTR1                                                                   
         LA    R5,CTPDATA                                                       
         ST    R7,DUB                                                           
*                                                                               
EXTRAC2  CLI   0(R5),0             END OF RECORD                                
         BE    EXTRXIT                                                          
         LA    R1,ELTAB                                                         
*                                                                               
EXTRAC4  CLI   0(R1),0             CHECK IF A PROFILE ELEMENT                   
         BE    EXTRAC8             NO - IGNORE                                  
         CLC   0(1,R1),0(R5)                                                    
         BE    *+12                                                             
         LA    R1,L'ELTAB(R1)                                                   
         B     EXTRAC4                                                          
         L     R7,DUB                                                           
*                                                                               
EXTRAC6  OC    0(4,R7),0(R7)       END OF PROFILE LIST                          
         BNZ   *+14                                                             
         MVC   0(4,R7),2(R5)       YES - POP IT IN                              
         B     EXTRAC8                                                          
         CLC   2(4,R5),0(R7)       ALREADY IN LIST                              
         BE    EXTRAC8             YES - IGNORE IT                              
         LA    R7,4(R7)            BUMP TO NEXT ENTRY                           
         B     EXTRAC6                                                          
*                                                                               
EXTRAC8  SR    R6,R6               BUMP TO NEXT ELEMENT                         
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     EXTRAC2                                                          
*                                                                               
EXTRXIT  XIT1                                                                   
         EJECT                                                                  
*              EDIT PROFILE LIST AND MOVE TO TWA                                
*                                                                               
DISPVALS NTR1                                                                   
         XC    PROVALS,PROVALS                                                  
         OI    PROVALSH+6,X'80'                                                 
         XC    TEMP,TEMP                                                        
         LA    R5,TEMP             A(EDITED PROFILE LIST)                       
         MVI   DISPTYPE,C'D'                                                    
         LA    R6,DLIST                                                         
         B     DISPV4              EDIT DEFAULT PROFILE LIST                    
*                                                                               
DISPV2   MVI   DISPTYPE,C'O'                                                    
         LA    R6,OLIST            EDIT OVERRIDE PROFILE LIST                   
*                                                                               
DISPV4   MVC   0(1,R5),DISPTYPE                                                 
         MVI   1(R5),C'='                                                       
*                                                                               
DISPV6   OC    0(4,R6),0(R6)       END OF LIST                                  
         BZ    DISPV10                                                          
         CLI   1(R5),C'='          START OF LIST                                
         BNE   *+12                                                             
         LA    R5,1(R5)                                                         
         B     *+8                                                              
         MVI   0(R5),C' '          DELIMIT LAST FIELD                           
         MVC   1(1,R5),0(R6)       T/D/S/P                                      
         LA    R7,1                                                             
         OC    1(3,R6),1(R6)                                                    
         BZ    DISPV8                                                           
         MVI   2(R5),C','          D,XXX                                        
         MVC   3(3,R5),1(R6)                                                    
         LA    R7,4(R7)                                                         
         CLI   0(R6),C'D'                                                       
         BE    DISPV8                                                           
         LA    R7,4(R7)                                                         
         GOTO1 VDATCON,DMCB,(1,1(R6)),(8,3(R5))                                 
         CLI   3(R5),C' '          CHECK US/UK FORMAT                           
         BNE   *+8                                                              
         OI    3(R5),X'F0'                                                      
         TM    3(R5),X'F0'                                                      
         BO    *+8                                                              
         LA    R7,1(R7)                                                         
*                                                                               
DISPV8   LA    R6,4(R6)            BUMP TO NEXT ENTRY                           
         LA    R5,1(R7,R5)         AND BUMP OUTPUT                              
         B     DISPV6                                                           
*                                                                               
DISPV10  CLI   1(R5),C'='          ANYTHING IN OUTPUT                           
         BNE   *+14                                                             
         MVC   2(4,R5),=C'NONE'                                                 
         LA    R5,7(R5)                                                         
         LA    R5,2(R5)                                                         
         CLI   DISPTYPE,C'O'       OVERRIDES DONE YET                           
         BNE   DISPV2              NO - GO AND DO THEM                          
         MVC   PROVALS,TEMP        YES - MOVE TO TWA                            
         XIT1                                                                   
         EJECT                                                                  
*        DISPLAY PRIORITY TYPE CODE                                             
*                                                                               
DPRTCODE EQU   *                                                                
         XC    PROPXPT,PROPXPT                                                  
         CLI   PROPRPT,C'A'                                                     
         BL    *+22                                                             
         CLI   PROPRPT,C'I'                                                     
         BH    *+14                                                             
         MVC   PROPXPT,=C'*1'                                                   
         B     DPRTCX                                                           
         CLI   PROPRPT,C'J'                                                     
         BL    *+22                                                             
         CLI   PROPRPT,C'R'                                                     
         BH    *+14                                                             
         MVC   PROPXPT,=C'*2'                                                   
         B     DPRTCX                                                           
         CLI   PROPRPT,C'S'                                                     
         BL    *+22                                                             
         CLI   PROPRPT,C'Y'                                                     
         BH    *+14                                                             
         MVC   PROPXPT,=C'*3'                                                   
         B     DPRTCX                                                           
         CLI   PROPRPT,C'0'                                                     
         BL    DPRTCX                                                           
         CLI   PROPRPT,C'9'                                                     
         BH    DPRTCX                                                           
         MVC   PROPXPT,=C'*4'                                                   
DPRTCX   BR    RE                                                               
         SPACE 2                                                                
*        DISPLAY PRIORITY OUTPUT CODE                                           
*                                                                               
DPROCODE EQU   *                                                                
         XC    PROPXPO,PROPXPO                                                  
         CLI   PROPRPO,C'A'                                                     
         BL    *+22                                                             
         CLI   PROPRPO,C'I'                                                     
         BH    *+14                                                             
         MVC   PROPXPO,=C'*1'                                                   
         B     DPROCX                                                           
         CLI   PROPRPO,C'J'                                                     
         BL    *+22                                                             
         CLI   PROPRPO,C'R'                                                     
         BH    *+14                                                             
         MVC   PROPXPO,=C'*2'                                                   
         B     DPROCX                                                           
         CLI   PROPRPO,C'S'                                                     
         BL    *+22                                                             
         CLI   PROPRPO,C'Y'                                                     
         BH    *+14                                                             
         MVC   PROPXPO,=C'*3'                                                   
         B     DPROCX                                                           
         CLI   PROPRPO,C'0'                                                     
         BL    DPROCX                                                           
         CLI   PROPRPO,C'9'                                                     
         BH    DPROCX                                                           
         MVC   PROPXPO,=C'*4'                                                   
DPROCX   BR    RE                                                               
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
*              LIST OF DAYS OF WEEK                                             
*                                                                               
DAYTAB   DS    0CL9                                                             
         DC    CL9'MONDAY'                                                      
         DC    CL9'TUESDAY'                                                     
         DC    CL9'WEDNESDAY'                                                   
         DC    CL9'THURSDAY'                                                    
         DC    CL9'FRIDAY'                                                      
         DC    CL9'SATURDAY'                                                    
         DC    CL9'SUNDAY'                                                      
         DC    X'00'                                                            
*                                                                               
TABCLA   DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ012345',X'FF'                        
         SPACE 2                                                                
*              LITERALS                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER OVERLAY W/S                                       
*                                                                               
WRKD     DSECT                                                                  
RELO     DS    A                                                                
SAVEDRE  DS    A                   USED IN BUILDINS ROUTINE                     
PROTYPE  DS    CL4                                                              
IDNUM    DS    H                                                                
BYTE     DS    XL1                                                              
IFLAG    DS    XL1                                                              
KLEVEL   DS    CL1                                                              
DISPTYPE DS    CL1                                                              
FTBFLAG  DS    XL1                 FLAGS NUMERIC SHIPPING UNIT TOO BIG          
*                                  SAVE LAST VALUE SET FOR                      
PRISCVAL DS    CL1                   CTPRISC                                    
PRIPTVAL DS    CL1                   CTPRIPT                                    
PRIPOVAL DS    CL1                   CTPRIPO                                    
*                                                                               
VLIST    DS    0CL160                                                           
DLIST    DS    CL80                                                             
OLIST    DS    CL80                                                             
LINES    DS    30CL32                                                           
REC      DS    1024C                                                            
REC2     DS    1024C                                                            
WRKX     EQU   *                                                                
         SPACE 2                                                                
*                                                                               
       ++INCLUDE CTLFMACTNS                                                     
         SPACE 2                                                                
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
LASTPRO  DS    CL4                                                              
LIDNUM   DS    CL2                                                              
*                                  SAVE PERMANENT DEFAULT VALUES OF             
PRISCSAV DS    CL1                   CTPRISC                                    
PRIPTSAV DS    CL1                   CTPRIPT                                    
PRIPOSAV DS    CL1                   CTPRIPO                                    
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMF9D                                                                      
       ++INCLUDE CTLFMF9D                                                       
         EJECT                                                                  
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
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009CTLFM06   05/01/02'                                      
         END                                                                    
