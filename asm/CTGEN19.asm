*          DATA SET CTGEN19    AT LEVEL 023 AS OF 09/02/05                      
*PHASE TA0B19B                                                                  
*INCLUDE EXPRESS                                                                
*INCLUDE HEXIN                                                                  
         TITLE 'CTGEN19 - FILE MAINTENANCE - PROFILE FIELD RECORDS'             
GEN19    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CG19**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING CTUREC,R2           R2=A(RECORD KEY)                             
         L     RC,AAPLOCAL                                                      
         USING LOCALD,RC           RC=A(LOCAL W/S)                              
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     DELREC              05 - APMDELR                                 
         B     RESREC              06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     EXIT                11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     EXIT                15 - APMVALQ                                 
         B     EXIT                16 - APMREPP                                 
         B     EXIT                17 - APMSETT                                 
         B     EXIT                18 - APMPUTK                                 
         B     VALREC              19 - APMNEWK (FOR COPY)                      
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF PROFILE FIELD RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         USING CTUREC,R2           R2=A(RECORD KEY)                             
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ    RECORD TYPE                                  
         MVI   FVMINL,1                                                         
         GOTO1 AVALSYS,FLDSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALKEYX                                                          
         L     R3,APPARM           APPARM = A(SYSLIST ENTRY)                    
         USING SYSLSTD,R3                                                       
         MVC   CTUKSYS,SYSLUSLT                                                 
         DROP  R3                                                               
*                                                                               
         MVI   FVMINL,2            VALIDATE PROGRAM NAME                        
         GOTO1 AFVAL,FLDPRGH       PROGRAM NAMES ARE 2 OR 3 BYTES               
         BNE   VALKEYX             OFF-LINE ARE 2, RIGHT JUSTIFIED              
         CLI   FVILEN,3                                                         
         BE    *+14                                                             
         MVC   CTUKPROG+1(2),FVIFLD                                             
         B     *+10                                                             
         MVC   CTUKPROG,FVIFLD                                                  
         OI    FLDPRGH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
VK010    MVI   FVMINL,0            VALIDATE LANGUAGE                            
         GOTO1 AFVAL,FLDLNGH                                                    
         BNL   VK015               LANGUAGE ENTERED: VALIDATE                   
         MVC   CTUKLANG,CULANG     NO LANG, USE CONNECTED                       
         CLI   CULANG,3            MAKE SURE ENGLISH USE 00 DEFAULT             
         BNL   *+8                                                              
         MVI   CTUKLANG,0                                                       
         GOTO1 ADISLNG,CTUKLANG    TELL USER WHAT WE ARE DOING                  
         MVC   FLDLNG(3),APWORK                                                 
         OI    FLDLNGH+(FVOIND-FVIHDR),FVOXMT                                   
         B     VK020               OFF TO LOOK FOR RECORD                       
VK015    GOTO1 AVALLNG,FLDLNGH     VALIDATE LANGUAGE NAME                       
         BNE   VALKEYX                                                          
         MVC   CTUKLANG,APWORK                                                  
*                                                                               
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   FLDLNG,C' '         DISPLAY FULL NAME.                           
         MVC   FLDLNG+1(L'FLDLNG-1),FLDLNG                                      
         MVC   FLDLNG(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
         OI    FLDLNGH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
VK020    MVC   APRECKEY(L'CTUKEY),CTUKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK) DEFAULT OK MESSAGE BEFORE VLDN.              
*                                                                               
VKPOSNF  EQU   *                   POSIT RECORD NOT FOUND.                      
VKEFSEL  CLI   CTUKLANG,3          ENGLISH GET ONE CHANCE                       
         BNL   VKEFFOR             FOREIGNERS GET NATIVE THEN DEFAULT.          
         TM    IOERR,IOERNF        ENGLISH, SO IF FOUND...                      
         BZ    VKADMFND            OFF TO ADMIT EXISTS.                         
         B     VKEFSELX                                                         
VKEFFOR  TM    IOERR,IOERNF        FOREIGN, SO IF FOUND...                      
         BZ    VKADMFND            OFF TO ADMIT EXISTS.                         
         CLI   APACTN,ACTDIS       DON'T LOOK FOR ENGLISH IF UPD                
         BNE   VKEFSELX                                                         
         MVI   CTUKLANG,0          FORIEGN NOT EXIST, DISPLAY ENGLISH           
         MVC   APRECKEY(L'CTUKEY),CTUKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX                                                          
         MVC   FVMSGNO,=AL2(CE#LNFDE)                                           
         MVC   FVOSYS,ASSYSE                                                    
         MVC   FLDLNG,=C'ENGLISH      '  TELL USER IT'S ENGLISH                 
         B     VKADMFND            OFF TO ADMIT EXISTS.                         
VKEFSELX EQU   *                   END OF ENG/FOREIGN SELECT.                   
         MVI   APINDS,APIOKADD     GOT HERE SO NO RECORD EXISTS                 
         B     VKPOSNFX                                                         
VKADMFND EQU   *                   IT'S FOUND BUT COULD BE DELETED              
*                                                                               
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKRES   (LOGICALLY DELETED)                   
*                                                                               
VKPOSNFX EQU   *                                                                
*                                                                               
VALKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE A PROFILE FIELD RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
VALREC   L     R2,AIOAREA1                                                      
         XC    CTUKEY(CTUDATA-CTUREC+1),CTUKEY                                  
         MVC   CTUKEY,APRECKEY                                                  
         MVC   CTULEN,=AL2(CTUDATA-CTUREC+1)                                    
*                                                                               
VRPOSVAL EQU   *                   POSIT SCREEN INFO IS OK.                     
*                                                                               
VRACTEL  GOTO1 ASETACT,CTUREC      ACTIVITY ELEMENT                             
*                                                                               
VRDESCEL EQU   *                   DESCRIPTION ELEMENT.                         
         MVI   FVMINL,0                                                         
         MVI   FVMAXL,L'FLDDSC                                                  
         GOTO1 AFVAL,FLDDSCH       CHECK FIELD                                  
         BL    VRDEEND             LOW=>NOT ENTERED SO DON'T ADD EL.            
         USING CTDSCD,R3                                                        
         LA    R3,APELEM                                                        
         XC    APELEM,APELEM                                                    
         MVI   CTDSCEL,CTDSCELQ    ELEMENT ID                                   
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         LA    RF,CTDSC-CTDSCD(RF)                                              
         STC   RF,CTDSCLEN         ELEMENT LENGTH                               
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTDSC(0),FVIFLD     DESCRIPTION TEXT                             
         GOTO1 AADDELS,CTUREC      ADD THE ELEMENT                              
VRDEEND  EQU   *                                                                
*                                                                               
         DROP  R3                                                               
*                                                                               
         USING LINED,R4                                                         
         LA    R4,FLDFNUMH                                                      
VRLINTOP EQU   *                   TOP OF LINE LOOP.                            
*                                                                               
         CLI   0(R4),0                                                          
         BE    VRLINEND                                                         
*                                                                               
         CLC   LINDSC(3),=C'++d'   MEANS DELETE LINE                            
         BE    VRDELLIN                                                         
         CLC   LINDSC(3),=C'++D'   MEANS DELETE LINE                            
         BNE   VRNOTDEL                                                         
VRDELLIN EQU   *                   DELETE THE LINE                              
         CLI   APACTN,ACTCHA                                                    
         BE    VRDL1                                                            
         OI    FVOIND,FVOCUR                                                    
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VRADMINV            ONLY VALID FOR CHANGE                        
VRDL1    TWAXC LINDSCH,LINDFTH     CLEAR LINE SO USER REALISES.                 
         B     VRDLEND             DEL'D BY DOING NOTHING.                      
*                                                                               
VRNOTDEL EQU   *                   NOT DELETED SO VALIDATE.                     
*                                                                               
         XC    APELEM,APELEM       SET UP ELEMENT READY                         
         USING CTFDD,R3            (EXCEPT FOR LENGTH)                          
         LA    R3,APELEM                                                        
         MVI   CTFDEL,CTFDELQ                                                   
*                                                                               
VRSEQNO  PACK  DUB,LINNUM          FIELD SEQUENCE NO                            
         CVB   R1,DUB                                                           
         STC   R1,CTFDNUM                                                       
*                                                                               
VRPOS3E  EQU   *                   POSIT FIRST THREE FIELDS ENTERED             
VRFDESC  MVI   FVMINL,0            FIELD DESCRIPTION                            
         MVI   FVMAXL,L'LINDSC                                                  
         GOTO1 AFVAL,LINDSCH                                                    
         BNL   VRFDESC1            LOW => NOT ENTERED                           
         LA    RF,LINDSCH                                                       
         ST    RF,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VRADM3E             QUIT TO MUST BE ALL BLANK IF N/I             
VRFDESC1 MVC   LINDSC,FVIFLD                                                    
*                                                                               
         XR    RF,RF               FIXED PORTION LENGTH.                        
         LA    RF,CTFDDESC-CTFDD(RF)                                            
         XR    RE,RE               VARIABLE LENGTH DESCRIPTION                  
         IC    RE,FVILEN                                                        
         AR    RF,RE                                                            
         STC   RF,CTFDLEN                                                       
*                                                                               
         IC    RF,FVXLEN           MOVE DESC TO ELEMENT                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CTFDDESC(0),FVIFLD                                               
*                                                                               
VRFTYPE  MVI   FVMINL,0            TYPE                                         
         MVI   FVMAXL,L'LINTYP                                                  
         GOTO1 AFVAL,LINTYPH                                                    
         BNL   VRFTYPE1                                                         
         OI    FVOIND,FVOCUR                                                    
         MVI   FVCURS,1                                                         
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VRADM3E             QUIT TO MUST BE ALL BLANK IF N/I             
*                                                                               
VRFTYPE1 CLI   FVILEN,2            TWO BYTE TYPE MUST HAVE '*'                  
         BNE   VRFTYPE3                                                         
         CLI   LINTYP+1,C'*'                                                    
         BE    VRFTYPE2                                                         
         LA    RF,LINTYPH                                                       
         ST    RF,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VRADMINV            QUIT IF NOT '*'                              
VRFTYPE2 OI    CTFDOTHR,X'80'      DDS ONLY PROFILE OPTION                      
VRFTYPE3 MVC   LINTYP,FVIFLD                                                    
         MVC   CTFDTYPE,FVIFLD                                                  
*                                                                               
         CLI   FVIFLD,C'O'         MEDIA OFFICE FIELD                           
         BNE   VRFVAL                                                           
         MVI   CTFDTYPE,C'C'       STORE AS CHARACTER                           
         OI    CTFDOTHR,X'20'      INDICATE 2 BYTE MEDIA OFFICE                 
*                                                                               
VRFVAL   MVI   FVMINL,0            ACCEPTABLE VALUES                            
         MVI   FVMAXL,L'LINVAL                                                  
         GOTO1 AFVAL,LINVALH                                                    
         BNL   VRFVAL1                                                          
         LA    RF,LINVALH                                                       
         ST    RF,APCURSOR                                                      
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VRADM3E             QUIT TO MUST BE ALL BLANK IF N/I             
VRFVAL1  MVC   LINVAL,FVIFLD                                                    
         MVC   CTFDLIST,FVIFLD                                                  
*                                                                               
VRFDFT   MVI   FVMINL,0            DEFAULT                                      
         MVI   FVMAXL,L'LINDFT                                                  
         GOTO1 AFVAL,LINDFTH                                                    
         MVC   LINDFT,FVIFLD                                                    
*                                                                               
         XR    R0,R0               DEFAULT CAN BE I/P OR NOT.                   
         CLI   FVILEN,0            SET R0 FOR EXPRESS AND THE                   
         BE    VRFDFT0             ELEMENT VALUES & INDICATORS.                 
         MVC   DEFAULT,FVIFLD                                                   
         LA    R0,DEFAULT                                                       
         B     VRFDFT1                                                          
VRFDFT0  OI    CTFDOTHR,X'40'      DEFAULT NOT ENTERED.                         
*                                                                               
VRFDFT1  EQU   *                                                                
         MVC   APBYTE,LINTYP                                                    
         CLI   LINTYP,C'O'         TYPE 'O' OFFICE                              
         BNE   *+8                                                              
         MVI   APBYTE,C'C'         VALIDATE LIKE HEXADECIMAL                    
*                                                                               
         L     RF,ACOM             PICK UP ADDRESS OF COMFACS LIST              
         USING COMFACSD,RF                                                      
         GOTO1 =V(EXPRESS),APPARM,APBYTE,LINVAL,(R0),CSCANNER,RR=APRELO         
         DROP  RF                                                               
*                                                                               
         CLI   APPARM,0            0 = OK                                       
         BE    VREXPOK                                                          
*                                                                               
         XR    RF,RF                                                            
         L     RE,APPARM+16        ADDRESS OF ERROR MESSAGE                     
         MVC   GENMSG(60),0(RE)                                                 
         MVC   FVMSGNO,=AL2(FVFSET)                                             
VREXPF1  CLI   APPARM,1            1 = FIRST FIELD (TYPE) INVALID               
         BNE   VREXPF2                                                          
         LA    RF,LINTYPH                                                       
         ST    RF,APCURSOR                                                      
         B     VRADMINV                                                         
*                                                                               
VREXPF2  CLI   APPARM,2            2 = SECOND FIELD (VALUE) INVALID             
         BNE   VREXPF3                                                          
         LA    RF,LINVALH                                                       
         ST    RF,APCURSOR                                                      
         B     VRADMINV                                                         
*                                                                               
VREXPF3  LA    RF,LINDFTH          3 = THIRD FIELD (DEFAULT) INVALID            
         ST    RF,APCURSOR                                                      
         B     VRADMINV                                                         
*                                                                               
VREXPOK  EQU   *                   AT LAST WE KNOW IT'S OK, SO ADD EL.          
         MVC   CTFDDEF,APPARM+4    DEFAULT VALUE IS IN P2                       
         GOTO1 AADDELS,CTUREC                                                   
*                                                                               
         B     VRP3EEND                                                         
VRADM3E  EQU   *                   1 OF 1ST 3 NOT ENTERED,                      
*                                  ALL LINE MUST BE BLANK OR NULLS              
         CLI   LINDSC,0                                                         
         BE    VRCLCDSC            1ST BYTE NULL SO CHECK REST                  
         CLI   LINDSC,C' '                                                      
         BE    VRCLCDSC            1ST BYTE SPACE SO CHECK REST                 
         B     VRADMINV            1ST BYTE NOT SP AND NOT NULL SO BAD          
VRCLCDSC CLC   LINDSC+1(L'LINDSC-1),LINDSC                                      
         BNE   VRADMINV            ALL NOT SP AND NOT NULL SO BAD               
*                                                                               
         CLI   LINTYP,0                                                         
         BE    VRCLCTYP                                                         
         CLI   LINTYP,C' '                                                      
         BE    VRCLCTYP                                                         
         B     VRADMINV                                                         
VRCLCTYP CLC   LINTYP+1(L'LINTYP-1),LINTYP                                      
         BNE   VRADMINV                                                         
*                                                                               
         CLI   LINVAL,0                                                         
         BE    VRCLCVAL                                                         
         CLI   LINVAL,C' '                                                      
         BE    VRCLCVAL                                                         
         B     VRADMINV                                                         
VRCLCVAL CLC   LINVAL+1(L'LINVAL-1),LINVAL                                      
         BNE   VRADMINV                                                         
*                                                                               
         CLI   LINDFT,0                                                         
         BE    VRCLCDFT                                                         
         CLI   LINDFT,C' '                                                      
         BE    VRCLCDFT                                                         
         B     VRADMINV                                                         
VRCLCDFT CLC   LINDFT+1(L'LINDFT-1),LINDFT                                      
         BNE   VRADMINV                                                         
*                                                                               
VRA3EOK  MVC   FVMSGNO,=AL2(FVFOK) IF WE GET HERE IT'S ALL BLANK => OK          
*                                  ALL BLANK COUNTS AS DELETE.                  
VRP3EEND EQU   *                   END OF POSIT FIRST 3 ENTERED.                
VRDLEND  EQU   *                   END OF ++D SELECTION (DELETE LINE)           
*                                                                               
         LA    R4,LINLEN(R4)                                                    
         B     VRLINTOP                                                         
VRLINEND EQU   *                   END OF LINE LOOP.                            
*                                                                               
         CLI   APACTN,ACTADD                                                    
         BE    VRADDCPY                                                         
         CLI   APACTN,ACTCPY                                                    
         BE    VRADDCPY                                                         
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         B     *+8                                                              
VRADDCPY LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    APCURSOR,APCURSOR                                                
         B     VRPOSEND            END OF GOOD STUFF SO HOP OVER BAD.           
VRADMINV EQU   *                   ADMIT SCREEN INVALID                         
VRPOSEND EQU   *                   END OF POSIT SCREEN OK.                      
VALRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF FIELD RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
DISKEY   LA    R2,APRECKEY                                                      
         TWAXC FLDSYSH,FLDPRGH                                                  
         L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE                                                       
DISKEY0  CLI   SYSLNUM,0           CHECK FOR EOT.                               
         BE    DISKEY2                                                          
         CLC   CTUKSYS,SYSLUSLT    MATCH ON USER LETTER                         
         BE    DISKEY1                                                          
         LA    RE,SYSLLEN(RE)      BUMP TO NEXT ENTRY                           
         B     DISKEY0                                                          
*                                                                               
DISKEY1  MVC   FLDSYS,SYSLNAME     FOUND ENTRY, DISPLAY LONG NAME               
         OI    FLDSYSH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
DISKEY2  MVC   FLDPRG,CTUKPROG     DISPLAY PROGRAM                              
         OI    FLDPRGH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
DISKEY3  GOTO1 ADISLNG,CTUKLANG    DISPLAY LANGUAGE.                            
         MVC   FLDLNG(3),APWORK    SHORT NAME IS ENOUGH                         
         OI    FLDLNGH+(FVOIND-FVIHDR),FVOXMT                                   
         DROP  RE                                                               
*                                                                               
DISKEYX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PROFILE FIELD RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
DISREC   L     R2,AIOAREA1                                                      
         LA    R3,CTUDATA                                                       
         LA    R4,FLDFNUMH                                                      
         USING LINED,R4                                                         
         TWAXC FLDDSCH                                                          
*                                                                               
         USING CTDSCD,R3                                                        
         XR    RF,RF                                                            
         IC    RF,CTDSCLEN                                                      
         AR    R3,RF               POINT TO NEXT ELEMENT                        
*                                                                               
         CLI   CTDSCEL,CTDSCELQ    DOES DESCRIPTION ELEMENT EXIST?              
         BE    DRDESC                                                           
         TWAXC FLDDSCH,FLDDSCH                                                  
         B     DRDESCX                                                          
DRDESC   EQU   *                   PUT DESCRIPTION ON SCREEN                    
         IC    RF,CTDSCLEN                                                      
         XR    RE,RE                                                            
         LA    RE,CTDSC-CTDSCD(RE) RE=L'FIXED PORTION                           
         SR    RF,RE               RF=L'DESCRIPTION                             
         BCTR  RF,0                RF=EXECUTE LENGTH                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDSC(0),CTDSC                                                  
*                                                                               
         IC    RF,CTDSCLEN         POINT TO NEXT ELEMENT                        
         AR    R3,RF                                                            
DRDESCX  EQU   *                                                                
         DROP  R3                  DROP THE DESCRIPTION ELEMENT                 
*                                                                               
         USING CTFDD,R3            HANG THE FIELD ELEMENT DSECT                 
*                                                                               
DRLINTOP CLI   CTFDEL,0            TOP OF SCREEN LINE LOOP.                     
         BE    DRLINEND            UNTIL NO MORE ELEMENTS                       
*                                                                               
DRFULMT  PACK  DUB,LINNUM          COMPARE THE SCREEN LINE TO EL SEQ#.          
         CVB   RF,DUB                                                           
         CLM   RF,1,CTFDNUM                                                     
         BE    DRFULLIN            SAME SO OFF TO FILL THE LINE.                
*                                                                               
DRMTLIN  MVI   LINDSC,C' '         NO MATCH ON SEQ# SO EMPTY LINE               
         MVC   LINDSC+1(L'LINDSC-1),LINDSC                                      
         MVC   LINTYP,=CL2'  '                                                  
         MVI   LINVAL,C' '                                                      
         MVC   LINVAL+1(L'LINVAL-1),LINVAL                                      
         MVI   LINDFT,C' '                                                      
         MVC   LINDFT+1(L'LINDFT-1),LINDFT                                      
         B     DRFULMTX                                                         
*                                                                               
DRFULLIN EQU   *                   EL SEQ#=SCR LINE# SO FILL LINE.              
         XR    RE,RE               FIELD DESCRIPTION FIRST                      
         XR    RF,RF                                                            
         IC    RF,CTFDLEN                                                       
         LA    RE,CTFDDESC-CTFDD(RE) RE=LENGTH OF FIXED PORTION.                
         SR    RF,RE               RF=LENGTH OF DESCRIPTION                     
         BCTR  RF,0                RF=EXEC LENGTH                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LINDSC(0),CTFDDESC                                               
*                                                                               
         MVC   LINTYP(1),CTFDTYPE  FIELD TYPE                                   
         TM    CTFDOTHR,X'80'      IS IT A DDS ONLY TYPE?                       
         BZ    *+8                                                              
         MVI   LINTYP+1,C'*'       YES, SO IT HAS A STAR                        
*                                                                               
         TM    CTFDOTHR,X'20'      2 BYTE MEDIA OFFICE                          
         BZ    *+8                                                              
         MVI   LINTYP,C'O'         MEDIA OFFICE FIELD                           
*                                                                               
         MVC   LINVAL,CTFDLIST     ACCEPTABLE VALUES                            
*                                                                               
         TM    CTFDOTHR,X'40'      DOES IT HAVE A DEFAULT VALUE?                
         BZ    DRDFAULT            YES IT DOES                                  
DRNODFT  MVI   LINDFT,C' '                                                      
         MVC   LINDFT+1(L'LINDFT-1),LINDFT                                      
         B     DRDFEND                                                          
*                                                                               
DRDFAULT EQU   *                   DEFAULT EXISTS                               
         CLI   CTFDTYPE,C'N'       DEFAULT DISPLAY DEPENDS ON TYPE              
         BE    DRDFNUM                                                          
         CLI   CTFDTYPE,C'X'                                                    
         BE    DRDFHEX                                                          
*                                                                               
DRDFCHAR MVC   LINDFT(1),CTFDDEF   CHARACTER DEFAULT JUST MOVE TO LINE.         
         B     DRDFEND                                                          
*                                                                               
DRDFNUM  EQU   *                   NUMERIC DEFAULT, NEED TO EDIT.               
         EDIT  (B1,CTFDDEF),(3,LINDFT),ALIGN=LEFT,ZERO=NOBLANK                  
         B     DRDFEND                                                          
*                                                                               
DRDFHEX  EQU   *                   HEX DEFAULT, USE HEXOUT.                     
         L     RF,ACOM             PICK UP ADDRESS OF COMFACS LIST              
         USING COMFACSD,RF                                                      
         GOTO1 CHEXOUT,APPARM,CTFDDEF,LINDFT,1,=C'TOG'                          
         DROP  RF                                                               
DRDFEND  EQU   *                                                                
         XR    RF,RF               END OF FULL LINE                             
         IC    RF,CTFDLEN          BUMP TO THE NEXT ELEMENT                     
         AR    R3,RF                                                            
DRFULMTX EQU   *                   END OF FULL/EMPTY SELECT.                    
*                                                                               
         LA    R4,LINLEN(R4)       DONE A LINE, POINT TO NEXT                   
         B     DRLINTOP                                                         
DRLINEND EQU   *                   END OF SCREEN LINE LOOP.                     
*                                                                               
DISRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A PROFILE FIELD RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CTUREC                                                   
         OI    CTUSTAT,X'80'       LOGICAL DELETE                               
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
DELRECX  B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED PROFILE FIELD RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,CTUREC                                                   
         NI    CTUSTAT,255-X'80'    UNSET DELETE                                
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS + INITIALISE FOR LIST/SELECT  *         
***********************************************************************         
VALSEL   LA    R2,APRECKEY         SET UP KEY                                   
         XC    CTUKEY(CTUDATA-CTUREC),CTUKEY                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
*                                                                               
         MVI   SVLSYS,0                                                         
         XC    SVLPRG,SVLPRG                                                    
         MVI   SVLLNG,C'A'         A => ALL LANGUAGES (DEFAULT)                 
*                                                                               
         MVI   FVMINL,0                                                         
         GOTO1 AVALSYS,LSTSYSH     VALIDATE SYSTEM NAME                         
         BNE   VALSELX             NOT VALID                                    
         CLI   FVILEN,0            ENTER ANYTHING?                              
         BE    VS010               NO, SO CHECK LANGUAGE                        
         L     R3,APPARM           R3=A(SYSLST ENTRY)                           
         USING SYSLSTD,R3                                                       
         MVC   CTUKSYS,SYSLUSLT    USER PROFILE LETTER FROM SYSTAB              
         MVC   SVLSYS,SYSLUSLT     SAVE IT FOR LIST COMPARISONS                 
         DROP  R3                                                               
*                                                                               
VS010    MVI   FVMINL,0            GET THE START PROGRAM, IF ENTERED            
         GOTO1 AFVAL,LSTPRGH                                                    
         BNE   VS040               NOTHING ENTERED, CHECK LANGUAGE              
         MVC   CTUKPROG+1(2),FVIFLD                                             
         CLI   FVILEN,3                                                         
         BNE   VS020                                                            
         MVC   CTUKPROG,FVIFLD                                                  
VS020    MVC   SVLPRG,CTUKPROG                                                  
*                                                                               
VS040    MVI   FVMINL,0            VALIDATE LANGUAGE FIELD                      
         GOTO1 AFVAL,LSTLNGH                                                    
         BL    VS050               NOTHING ENTERED, ASSUME ALL LANGS            
         GOTO1 AVALLNG,LSTLNGH     VALIDATE LANGUAGE NAME                       
         BNE   VALSELX                                                          
         MVC   CTUKLANG,APWORK                                                  
         MVC   SVLLNG,APWORK                                                    
*                                                                               
         L     R1,APPARM           A(LANGUAGE TABLE ENTRY)                      
         MVI   LSTLNG,C' '         DISPLAY FULL NAME.                           
         MVC   LSTLNG+1(L'LSTLNG-1),LSTLNG                                      
         MVC   LSTLNG(L'LANGFUL),LANGFUL-LANGTABD(R1)                           
*                                                                               
VS050    LA    R0,LSTACTH          ADDR OF FIRST LIST LINE GOES...              
         ST    R0,APPARM           ...INTO APPARM+0(4)                          
         LA    R1,LSTACT2H                                                      
         SR    R1,R0               LENGTH OF LIST LINE GOES...                  
         STH   R1,APPARM+6         ...INTO APPARM+6                             
         MVC   FVMSGNO,=AL2(FVFOK) EVERYTHING IS OK                             
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
GETSEL   LA    R2,IOKEY                                                         
         MVC   CTUKEY,APRECKEY                                                  
*                                                                               
         TM    APINDS,APILRERD           TEST SEQUENCE BROKEN                   
         BZ    GS010                                                            
         GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GS020                                                            
         B     GETSELN                                                          
*                                                                               
GS010    TM    APINDS,APILNSEQ           TEST READ OR READ HIGH                 
         BNZ   GS020                                                            
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GS020    LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         BNE   GETSELN                                                          
         L     R2,AIOAREA1                                                      
         CLI   CTUKTYP,CTUKTYPQ          UNTIL CHANGE OF TYPE...                
         BNE   GETSELN                                                          
         OC    CTUKAGY,CTUKAGY           FIELD RECORDS HAVE 2X'00'              
         BZ    GS030                                                            
*                                                                               
         CLC   SAVESYS,CTUKSYS           SAME SYSTEM?                           
         BNE   GS030                                                            
         CLC   SAVEPRG,CTUKPROG          PROGRAM?                               
         BNE   GS030                                                            
         CLC   SAVELNG,CTUKLANG          LANGUAGE?                              
         BE    GS020                                                            
*                                                                               
GS030    MVC   SAVESYS,CTUKSYS           SAVE SYSTEM                            
         MVC   SAVEPRG,CTUKPROG          PROGRAM                                
         MVC   SAVELNG,CTUKLANG          LANGUAGE                               
*                                                                               
         CLI   SVLSYS,0                  LIST FILTER?                           
         BE    GS040                                                            
         CLC   SVLSYS,CTUKSYS            SAME SYSTEM?                           
         BNE   GS020                     . NO                                   
*                                                                               
GS040    OC    SVLPRG,SVLPRG             PROGRAM FILTER?                        
         BZ    GS050                     . NO, CHECK LANGUAGE                   
         CLI   SVLSYS,0                  SYSTEM FILTER?                         
         BNE   GS050                     . YES, PROGRAM IS START AT             
         CLC   SVLPRG,CTUKPROG           . NO, PROGRAM IS EXACT FILTER          
         BNE   GS020                                                            
*                                                                               
GS050    CLI   SVLLNG,C'A'               ALL LANGUAGES?                         
         BE    GETSELY                   . YES, THEN DONE                       
         OC    SVLPRG,SVLPRG             PROGRAM FILTER?                        
         BNZ   GETSELY                   . YES, LANGUAGE IS START AT            
         CLC   SVLLNG,CTUKLANG           . NO, LANGUAGE IS EXACT FILTER         
         BNE   GS020                                                            
*                                                                               
GETSELY  MVC   APRECKEY(L'CTUKEY),CTUKEY                                        
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
*                                                                               
GETSELN  MVI   APMODE,APMEOFS            SET NO MORE RECORDS TO COME            
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM           APPARM=A(LINE TO BE FILLED)                  
         USING LISTD,R4                                                         
         XC    LISTSYS,LISTSYS                                                  
         XC    LISTPRG,LISTPRG                                                  
         XC    LISTDESC,LISTDESC                                                
*                                                                               
         L     RE,ASYSLST          NEED TO FIND SYSTEM LONG NAME                
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE                                                       
DISSEL0  CLI   SYSLNUM,0                                                        
         BE    DISSEL2             EOT SO DONT DISPLAY SYSTEM                   
         CLC   CTUKSYS,SYSLUSLT                                                 
         BE    DISSEL1             FOUND THE ENTRY SO DO.                       
         LA    RE,SYSLLEN(RE)                                                   
         B     DISSEL0                                                          
*                                                                               
DISSEL1  MVC   LISTSYS,SYSLNAME                                                 
*                                                                               
DISSEL2  MVC   LISTPRG,CTUKPROG                                                 
*                                                                               
         GOTO1 ADISLNG,CTUKLANG    DISPLAY THE LANGUAGE SHORT NAME              
         MVC   LISTLNG,APWORK                                                   
*                                                                               
         LA    R3,CTUDATA          GET THE DESCRIPTION ELEMENT                  
         USING CTDSCD,R3           (AFTER THE ACTIVITY ELEMENT)                 
         XR    RF,RF                                                            
         IC    RF,CTDSCLEN                                                      
         AR    R3,RF               R3=A(ELEMENT AFTER ACTIVITY)                 
*                                                                               
         CLI   CTDSCEL,CTDSCELQ    DOES THE DESC ELEMENT EXIST?                 
         BE    DISSEL3             YES                                          
         B     DISSELX             NO, SO LEAVE                                 
DISSEL3  XR    RF,RF                                                            
         IC    RF,CTDSCLEN                                                      
         XR    RE,RE                                                            
         LA    RE,CTDSC-CTDSCD(RE) RE=L'FIXED PORTION                           
         SR    RF,RE               RF=L'DESCRIPTION                             
         BCTR  RF,0                RF=EXECUTE LENGTH                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LISTDESC(0),CTDSC                                                
*                                                                               
         DROP  RE,R3,R4                                                         
*                                                                               
DISSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   GENTABH                                                          
       ++INCLUDE CTGENE7D                                                       
         ORG   GENTABH                                                          
       ++INCLUDE CTGENC7D                                                       
         ORG   GENTABH                                                          
*******************************************************************             
* SAVED STORAGE IN THE TWA SAVEAREA                                             
*******************************************************************             
         ORG   SAVOVER                                                          
SAVESYS  DS    CL1                 NEED TO SAVE SYSTEM PASS-TO-PASS             
SAVEPRG  DS    CL3                 NEED TO SAVE PROG   PASS-TO-PASS             
SAVELNG  DS    XL1                 NEED TO SAVE LANG   PASS-TO-PASS             
SVLSYS   DS    CL1                 SAVE SYSTEM FOR LIST FILTER                  
SVLPRG   DS    CL3                 SAVE PROGRAM FOR LIST FILTER                 
SVLLNG   DS    XL1                 SAVE LANGUAGE FOR LIST FILTER                
         ORG                                                                    
         EJECT                                                                  
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTACTH DS    XL8                                                              
LISTACT  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN)                                                    
LISTSYS  DS    CL7                 SYSTEM                                       
         DS    CL2                                                              
LISTPRG  DS    CL3                 PROGRAM NAME                                 
         DS    CL6                                                              
LISTLNG  DS    CL3                 LANGUAGE SHORT NAME                          
         DS    CL6                                                              
LISTDESC DS    CL30                PROGRAM DESCRIPTION.                         
         ORG   LISTLIN+L'LISTLIN                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
         EJECT                                                                  
LOCALD   DSECT                     ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
DEFAULT  DS    XL4                                                              
SAVEKEY  DS    XL(L'CTUKEY)                                                     
SELKEY   DS    0XL32                                                            
SELSYS   DS    XL1                 MESSAGE SYSTEM                               
         ORG   SELKEY+L'SELKEY                                                  
LOCALX   EQU   *                                                                
         SPACE 1                                                                
LINED    DSECT                                                                  
LINNUMH  DS    CL8                                                              
LINNUM   DS    CL2                                                              
LINDSCH  DS    CL8                                                              
LINDSC   DS    CL30                                                             
LINTYPH  DS    CL8                                                              
LINTYP   DS    CL2                                                              
LINVALH  DS    CL8                                                              
LINVAL   DS    CL20                                                             
LINDFTH  DS    CL8                                                              
LINDFT   DS    CL5                                                              
LINLEN   EQU   *-LINED                                                          
         SPACE 1                                                                
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023CTGEN19   09/02/05'                                      
         END                                                                    
