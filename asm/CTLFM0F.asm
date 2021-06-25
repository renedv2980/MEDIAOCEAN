*          DATA SET CTLFM0F    AT LEVEL 047 AS OF 07/13/09                      
*PHASE TA020FA                                                                  
CTLFM0F  TITLE '-  CONTROL FILE MAINT - DEMO FORMULA RECORDS'                   
*****************************************************************               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*  ******* THIS MODULE HAS BEEN REPLACED BY CTSFM22 ********                    
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*****************************************************************               
*                                                                               
*****************************************************************               
*NOTE: TO ACCESS THE 4BYTE VS 3BYTE DEMOS USE FE.. AND MA..                     
*   INSTEAD OF W AND M AS THE SEX PREFIX. (SUPPORTED IN DEMOVAL)                
*   E.G. YFE6-14, YFE9-17                                                       
*                                                                               
*****************************************************************               
CTLFM0F  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKDX-WORKD,**LFMF**,RA,RR=R7                                   
         USING WORKD,RC            RC=A(TEMP W/S)                               
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTGREC,R4           R4=A(RECORD)                                 
*                                                                               
         L     RF,APARM                                                         
         L     RE,12(RF)           GET ADDRESS OF DEMOCON FROM COMFACS          
         USING COMFACSD,RE                                                      
         L     RF,CCALLOV                                                       
         LA    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 (RF),(R1),0         GET A(DEMOCON) FROM CALLOV                   
         MVC   VDEMOCON,DMCB       SAVE AWAY FOR LATER                          
         DROP  RE                                                               
*                                                                               
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
*                                                                               
KEYVAL   XC    CTGKEY,CTGKEY                                                    
         MVI   CTGKTYP,C'G'                                                     
*                                  VALIDATE FILE TYPE                           
         GOTO1 AFVAL,DEMFILEH      ------------------                           
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5           LOOK-UP FIELD IN TABLE                       
         BCTR  R1,0                                                             
         LA    RE,FILETAB                                                       
KEYV2    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   4(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'FILETAB(RE)                                                 
         B     KEYV2                                                            
         CLC   FLD(L'DEMFILE),4(RE)                                             
         BE    *+14                                                             
         MVC   DEMFILE,4(RE)       DISPLAY FULL NAME                            
         OI    DEMFILEH+6,X'80'                                                 
         MVC   CTGKFILE,0(RE)                                                   
         MVC   SVDBFILE,1(RE)                                                   
*                                  VALIDATE MEDIA CODE                          
         GOTO1 AFVAL,DEMSUBFH      -------------------                          
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         LA    RE,MEDTAB                                                        
KEYV3    CLI   0(RE),0             TEST E-O-T                                   
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'MEDTAB(RE)                                                  
         B     KEYV3                                                            
         CLC   DEMSUBF,1(RE)                                                    
         BE    *+14                                                             
         MVC   DEMSUBF,1(RE)                                                    
         OI    DEMSUBFH+6,X'80'                                                 
         MVC   CTGKMED,0(RE)                                                    
*                                  VALIDATE SOURCE CODE                         
         GOTO1 AFVAL,DEMSRCH       --------------------                         
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5           LOOK-UP SOURCE IN TABLE                      
         BCTR  R1,0                                                             
         LA    RE,SRCTAB                                                        
KEYV4    CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'SRCTAB(RE)                                                  
         B     KEYV4                                                            
         CLC   DEMSRC(L'SRCTAB-1),1(RE)                                         
         BE    *+14                                                             
         MVC   DEMSRC(L'SRCTAB-1),1(RE)                                         
         OI    DEMSRCH+6,X'80'                                                  
         MVC   CTGKSRC,0(RE)                                                    
*                                  ENSURE FILE/MEDIA/SOURCE VALID               
         LA    RE,FMSTAB                                                        
KEYV4A   CLI   0(RE),0                                                          
         BE    EIIF                                                             
         CLC   CTGKFILE(3),0(RE)                                                
         BE    *+12                                                             
         LA    RE,L'FMSTAB(RE)                                                  
         B     KEYV4A                                                           
*                                  VALIDATE START BOOK (MMM/YY)                 
         GOTO1 AFVAL,DEMSTRTH      ----------------------------                 
         BNZ   KEYV4D                                                           
         CLI   ACTN,DISPLAY        IF ACTN=DISPLAY AND                          
         BE    KEYV4Y              FIELD IS BLANK, USE LATEST BOOK              
         B     EXIT                                                             
KEYV4D   CLI   CTGKMED,C'N'        NETWORK                                      
         BE    KEYV4N                                                           
         CLI   CTGKMED,C'W'        WEEKLY                                       
         BE    KEYV4N                                                           
         CLI   CTGKMED,C'I'        EIN (ESTIMATED IMPS)                         
         BE    KEYV4N                                                           
         CLI   CTGKMED,C'V'        EVN (ESTIMATED VPH)                          
         BE    KEYV4N                                                           
         CLC   CTGKFILE(2),=C'CH'  CABLE HISPANIC IS A WEEKLY FILE              
         BE    KEYV4N                                                           
         GOTO1 VDATVAL,DMCB,(2,FLD),WORK                                        
         OC    0(4,R1),0(R1)       TEST IF DATE VALID                           
         BZ    EIIF                                                             
*&&DO                                                                           
         PACK  DUB,WORK(2)         CONVERT TO BINARY YYMM IN KEY                
         CVB   R1,DUB                                                           
         STC   R1,CTGKSTRT                                                      
         PACK  DUB,WORK+2(2)                                                    
         CVB   R1,DUB                                                           
         STC   R1,CTGKSTRT+1                                                    
*&&                                                                             
         GOTO1 VDATCON,DMCB,(0,WORK),(3,CTGKSTRT),0                             
         B     KEYV4X                                                           
*                                  VALIDATE NETWORK BOOK                        
KEYV4N   TM    FLDH+4,X'08'        TEST BOOK NUMERIC                            
         BZ    EIIF                                                             
         CLI   FLDH+5,4            AND 4 BYTES LONG                             
         BNE   EIIF                                                             
         PACK  DUB,FLD(2)          GET YEAR                                     
         CVB   R0,DUB                                                           
         CHI   R0,27                                                            
         BH    *+8                                                              
         AHI   R0,100              ADD X'64' TO ADJUST FOR Y2K                  
*        CH    R0,=H'70'           TEST GR 1970                                 
*        BL    EIIF                                                             
         STC   R0,CTGKSTRT         SET YEAR                                     
         PACK  DUB,FLD+2(2)        GET NETWORK WEEK                             
         CVB   R0,DUB                                                           
         LTR   R0,R0               TEST 1 THRU 53                               
         BZ    EIIF                                                             
         CH    R0,=H'53'                                                        
         BH    EIIF                                                             
         STC   R0,CTGKSTRT+1       SET WEEK                                     
KEYV4X   XC    CTGKSTRT,=X'FFFF'                                                
KEYV4Y   MVC   CTGKAGY,=X'FFFF'                                                 
         MVI   CTGKCODE,X'FF'                                                   
         MVC   WORK(2),CTGKSTRT    SAVE BOOK FOR READ HIGH                      
*                                                                               
         EJECT                                                                  
*                                  VALIDATE DEMO CODE                           
KEYV6    GOTO1 AFVAL,DEMDEMOH      ------------------                           
         BZ    EXIT                                                             
         CLI   FLDH+5,2                                                         
         BL    EFTS                                                             
*                                  TEST FIRST CHR. FOR ALPHA                    
         CLI   FLD,C'A'                                                         
         BL    EIIF                                                             
         CLI   FLD,C'Z'                                                         
         BH    EIIF                                                             
         MVC   CTGKDEMO(1),FLD                                                  
         CLI   FLD+1,C'0'          ALPHA OR NUMERIC                             
         BNL   KEYV6N                                                           
*                                  OTHER CHRS. MUST BE ALPHA                    
         L     RF,APARM                                                         
         L     RE,12(RF)           GET ADDRESS OF DEMOVAL FRPM COMFACS          
         USING COMFACSD,RE                                                      
         L     RF,CDEMOVAL                                                      
         XC    TEMP,TEMP           TEMP USED FOR DBLOCK                         
         USING DEDBLOCK,R1                                                      
         LA    R1,TEMP                                                          
         MVC   DBFILE,SVDBFILE     FILE                                         
         MVC   DBSELMED,CTGKMED    MEDIA                                        
         MVC   DBSELSRC,CTGKSRC    SOURCE                                       
         MVI   DBDEMTYP,C'4'       SET FOR EXPANDED DEMOS                       
         ST    RE,DBCOMFCS         A(COMFACS)                                   
*                                                                               
         MVC   DUB(1),DEMDEMO                                                   
         MVI   DEMDEMO,C'R'        PASS 'R' TO DEMOVAL                          
         GOTO1 (RF),DMCB,(1,DEMDEMOH),(1,WORK),(0,TEMP)                         
         MVC   DEMDEMO(1),DUB                                                   
         CLI   4(R1),0             ERROR                                        
         BE    EIIF                                                             
*                                  CONVERT SEX 0 AND 1 TO 1-256                 
         CLI   WORK+2,1                                                         
         BNE   *+12                                                             
         OI    WORK+3,X'80'                                                     
         MVI   WORK+2,0                                                         
*                                                                               
         CLI   WORK+2,0                                                         
         BE    *+10                                                             
         MVC   CTGKCODE(1),WORK+2                                               
         MVC   CTGKDEMO+1(1),WORK+3                                             
         B     KEYV6S                                                           
*                                  OTHER CHRS. MUST BE NUMERIC                  
KEYV6N   CLI   FLDH+5,4                                                         
         BH    EFTL                                                             
         ZIC   R1,FLDH+5                                                        
         SH    R1,=H'2'                                                         
         MVC   DUB,=8C'0'                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),FLD+1                                                     
         CLC   DUB,=8C'0'                                                       
         BNE   EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD+1(0)                                                     
         CVB   R1,DUB              VALUE MUST BE BETWEEN 1-255                  
         ST    R1,DUB                                                           
         OC    DUB(3),DUB                                                       
         BNZ   EIIF                                                             
         MVC   CTGKDEMO+1(1),DUB+3                                              
KEYV6S   MVC   WORK+2(2),CTGKDEMO  SAVE DEMO FOR READ HIGH                      
*                                                                               
*                                  READ RECORD                                  
KEYV7    MVC   KEY,CTGKEY          -----------                                  
         MVC   KEYNEXT,KEY                                                      
         LA    R1,DEMFILEH                                                      
         ST    R1,FADR                                                          
*                                                                               
         CLI   ACTN,CHANGE         IF ACTN=CHANGE AND KEY NEQ LKEY              
         BNE   *+18                SET ACTION TO DISPLAY                        
         CLC   KEY,LKEY                                                         
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU FOR UPDATABLE ACTIONS                
         GOTO1 AREAD                                                            
         BZ    EXIT                                                             
*                                                                               
         TM    DMCB+8,X'10'        CHECK FOR NOT FOUND                          
         BZ    KEYV9                                                            
         CLI   ACTN,ADD            VALID FOR ADD                                
         BE    DATAVAL                                                          
         CLI   ACTN,CHANGE         NOT VALID FOR CHANGE                         
         BE    ERNF                                                             
*                                                                               
*                                  READ FOR NEXT BOOK (DISPLAY ONLY)            
         CLC   KEY(21),CTGKEY      ---------------------------------            
         BNE   ERNF                                                             
         CLC   CTGKDEMO,WORK+2     SAME DEMO                                    
         BE    KEYV9                                                            
         BH    *+14                                                             
         MVC   CTGKDEMO,WORK+2     RESTORE DEMO CODE                            
         B     KEYV7               GO BACK TO READ RECORD                       
         MVC   CTGKDEMO,=X'FFFF'                                                
         B     KEYV7               GO BACK TO READ RECORD                       
*                                                                               
KEYV9    CLI   ACTN,ADD            FOUND NOT VALID FOR ADD                      
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        DELETED RECORDS MAY ONLY BE DSPLYD           
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY                                                     
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         CLC   WORK(2),CTGKSTRT    WAS GIVEN BOOK ON THE NOSE                   
         BE    DISPREC                                                          
         MVC   WORK(2),CTGKSTRT    REPLACE GIVEN BOOK WITH REAL BOOK            
         XC    WORK(2),=X'FFFF'                                                 
         XC    DEMSTRT,DEMSTRT     CLEAR FIELD BEFORE REDISPLAYING              
         OI    DEMSTRTH+6,X'80'                                                 
         CLI   CTGKMED,C'H'        HISPANIC?                                    
         BE    *+8                                                              
         CLI   CTGKMED,C'N'        NETWORK                                      
         BE    *+8                                                              
         CLI   CTGKMED,C'W'        WEEKLY                                       
         BE    *+8                                                              
         CLI   CTGKMED,C'V'        ESTIMATED VPH                                
         BNE   KEYV9D                                                           
         ZIC   RF,WORK+0                                                        
         CVD   RF,DUB                                                           
         UNPK  DEMSTRT+0(2),DUB                                                 
         OI    DEMSTRT+1,X'F0'                                                  
         ZIC   RF,WORK+1                                                        
         CVD   RF,DUB                                                           
         UNPK  DEMSTRT+2(2),DUB                                                 
         OI    DEMSTRT+3,X'F0'                                                  
         B     DISPREC                                                          
KEYV9D   GOTO1 VDATCON,DMCB,(3,WORK),(6,DEMSTRT)                                
         B     DISPREC                                                          
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DISPREC  TWAXC DEMDESCH            CLEAR DOWN TWA                               
*                                                                               
         GOTO1 VHEXOUT,DMCB,CTGKCODE,DEMLKCD,1,=C'TOG'                          
         CLC   =F'2',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DEMLKCDH+6,X'80'    XMIT                                         
*                                                                               
         LA    R5,CTGDATA          R5=A(FIRST ELEMENT)                          
*                                                                               
DISP2    CLI   0(R5),0                                                          
         BE    DISPEND                                                          
         CLI   0(R5),X'02'                                                      
         BE    DISP4                                                            
         CLI   0(R5),X'03'                                                      
         BE    DISP5                                                            
         CLI   0(R5),X'05'                                                      
         BE    DISP12                                                           
*                                  BUMP TO NEXT ELEMENT                         
DISP3    ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP2                                                            
*                                  DISPLAY DESCRIPTION                          
DISP4    ZIC   R1,1(R5)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     DISP3                                                            
         MVC   DEMDESC(0),2(R5)                                                 
*                                  DISPLAY FIELD PRECISION                      
DISP5    LA    R1,PRECTAB          R1=A(PRECISION TABLE)                        
         TM    3(R5),X'20'         TEST IF EQUATED FORMULA                      
         BZ    DISP6                                                            
         MVC   DEMPREC(4),=C'USE=' YES - OUTPUT USE=MODIFIER                    
         MVC   DEMPREC+4(1),2(R5)                                               
         B     DISP3                                                            
DISP6    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),2(R5)                                                    
         BE    *+12                                                             
         LA    R1,L'PRECTAB(R1)                                                 
         B     DISP6                                                            
         MVC   DEMPREC(L'PRECTAB-1),1(R1)                                       
*                                  DISPLAY DIRECT XFER OPTION                   
         MVC   DEMDIR,=C'YES'                                                   
         TM    3(R5),X'80'                                                      
         BZ    *+10                                                             
         MVC   DEMDIR,=C'NO '                                                   
         MVC   DEMNDX,=C'NO '                                                   
         TM    3(R5),X'40'                                                      
         BZ    *+10                                                             
         MVC   DEMNDX,=C'YES'                                                   
*                                  DISPLAY OPTIONS                              
         LA    RE,OPTMACS                                                       
         CLI   CTGKDEMO+1,0                                                     
         BE    *+8                                                              
         LA    RE,OPTDEMS                                                       
DISP7    CLI   0(RE),X'FF'                                                      
         BE    DISP9                                                            
         CLI   0(RE),0                                                          
         BE    DISP8                                                            
         ZIC   R1,0(RE)                                                         
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    3(R5),0                                                          
         BNZ   DISP8                                                            
         LA    RE,L'OPTMACS(RE)                                                 
         B     DISP7                                                            
DISP8    MVC   DEMOPT(L'OPTMACS-1),1(RE)                                        
DISP9    B     DISP3                                                            
*                                  DISPLAY FORMULA                              
DISP12   LA    R1,DEMFORMH         R1=A(FIRST TWA LINE)                         
         ZIC   RE,2(R5)            RE=INPUT FIELD LINE NUMBER                   
         SR    RF,RF                                                            
DISP14   TM    1(R1),X'20'         SKIP PROTS                                   
         BO    *+12                                                             
         BCT   RE,*+8              DO FOR NUMBER OF LINES                       
         B     *+14                                                             
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         B     DISP14                                                           
         IC    RF,1(R5)                                                         
         SH    RF,=H'3'                                                         
         STC   RF,5(R1)            STORE LENGTH IN HEADER                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     DISP3                                                            
         MVC   8(0,R1),3(R5)                                                    
*                                  SET NEXT ACTION & EXIT                       
DISPEND  BAS   RE,DISFORMA         BUT FIRST DISPLAY ALPHA FORMULA              
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         TM    CTGSTAT,X'80'                                                    
         BZ    *+12                                                             
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
         LA    R1,DEMDESCH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKDEL+OKCHA                                                
         B     EXIT                                                             
         EJECT                                                                  
* ADD/CHANGE RECORD                                                             
*                                                                               
DATAVAL  MVI   TEMP,0              BUILD BASIC RECORD                           
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
*                                  BUILD DESCRIPTION ELEMENT                    
         GOTO1 AFVAL,DEMDESCH                                                   
         BZ    DATAV2                                                           
         ZIC   R1,FLDH+5                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+2(0),FLD                                                    
         LA    R1,2(R1)                                                         
         STC   R1,TEMP+1                                                        
         MVI   TEMP,X'02'                                                       
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  BUILD FIELD PRECISION ELEMENT                
DATAV2   GOTO1 AFVAL,DEMPRECH                                                   
         BZ    EXIT                                                             
         CLI   FLDH+5,5            VALIDATE FOR USE=MODIFIER                    
         BNE   DATAV3                                                           
         CLC   FLD(3),=C'USE'                                                   
         BNE   DATAV3                                                           
         TM    FLD+4,X'40'         MODIFIER MUST BE VALID ALPHA                 
         BZ    EIIF                                                             
         MVC   TEMP+2(1),FLD+4                                                  
         MVI   DIROPT,X'20'        SET EQUATED MODIFIER BIT                     
         B     DATAV7                                                           
DATAV3   ZIC   R1,FLDH+5           LOOK-UP PRECISION IN TABLE                   
         BCTR  R1,0                                                             
         LA    RE,PRECTAB                                                       
DATAV4   CLI   0(RE),0                                                          
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),FLD                                                      
         BE    *+12                                                             
         LA    RE,L'PRECTAB(RE)                                                 
         B     DATAV4                                                           
         CLC   DEMPREC(L'PRECTAB-1),1(RE)                                       
         BE    *+14                                                             
         MVC   DEMPREC(L'PRECTAB-1),1(RE)                                       
         OI    DEMPRECH+6,X'80'                                                 
         MVC   TEMP+2(1),0(RE)                                                  
         MVC   PRECISN,0(RE)                                                    
*                                  VALIDATE DIRECT XFER OPTION                  
         CLI   DEMDIRH+5,0                                                      
         BNE   *+18                                                             
         MVC   DEMDIR,=C'YES'                                                   
         MVI   DEMDIRH+5,3                                                      
         OI    DEMDIRH+6,X'80'                                                  
         GOTO1 AFVAL,DEMDIRH                                                    
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         MVI   DIROPT,0                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=C'YES'      OPTIONS ARE YES/NO                           
         BE    DATAV5                                                           
         OI    DIROPT,X'80'                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=C'NO '                                                   
         BNE   EIIF                                                             
         TM    PRECISN,X'20'                                                    
         BNZ   EIIF                                                             
*                                  VALIDATE INDEX DEMO OPTION                   
DATAV5   CLI   DEMNDXH+5,0                                                      
         BNE   *+18                                                             
         MVC   DEMNDX,=C'NO '                                                   
         MVI   DEMNDXH+5,2                                                      
         OI    DEMNDXH+6,X'80'                                                  
         GOTO1 AFVAL,DEMNDXH                                                    
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=C'NO '      OPTIONS ARE YES/NO                           
         BE    DATAV6                                                           
         OI    DIROPT,X'40'                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=C'YES'                                                   
         BNE   EIIF                                                             
*                                  VALDATE OPTIONS                              
DATAV6   GOTO1 AFVAL,DEMOPTH                                                    
         BE    DATAV7                                                           
         LA    RE,OPTMACS                                                       
         CLI   CTGKDEMO+1,0        TEST MACRO DEMO                              
         BE    *+8                                                              
         LA    RE,OPTDEMS                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
DATAV6A  CLI   0(RE),X'FF'         TEST E-O-T                                   
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),1(RE)                                                     
         BE    *+12                                                             
         LA    RE,L'OPTMACS(RE)                                                 
         B     DATAV6A                                                          
         XC    DEMOPT,DEMOPT                                                    
         MVC   DEMOPT(L'OPTMACS-1),1(RE)                                        
         OI    DEMOPTH+6,X'80'                                                  
         OC    DIROPT,0(RE)                                                     
*                                                                               
DATAV7   MVC   TEMP+3(1),DIROPT                                                 
         MVI   TEMP+1,4                                                         
         MVI   TEMP,X'03'                                                       
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  TEST IF FORMULA INPUT                        
         GOTO1 AFVAL,DEMFORMH                                                   
         BNZ   DATAV8                                                           
         TM    DIROPT,X'80'        IF DIRECT XFER=NO                            
         BO    EXIT                FORMULA MUST BE INPUT                        
         B     DATAVEND                                                         
*                                  FORMULA VALIDATION                           
DATAV8   TM    DIROPT,X'20'        TEST IF EQUATED MODIFIER                     
         BNZ   EIIF                YES - FORMULA IS NOT ALLOWED                 
         BAS   RE,VALFORM                                                       
         CLI   FERN,X'FF'                                                       
         BNE   EXIT                                                             
         GOTO1 APUTEL              ADD POLISH FORMULA ELEMENT                   
         BZ    EXIT                                                             
*                                  BUILD INPUT FORMULA ELEMENTS                 
DATAV9   LA    R5,DEMFORMH         R5=A(FIRST INPUT FIELD)                      
         SR    R6,R6               R6=INPUT LINE NUMBER                         
DATAV10  SR    R1,R1                                                            
         ICM   R1,1,5(R5)          R1=INPUT FIELD LENGTH                        
         BZ    DATAV12                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+3(0),8(R5)                                                  
         LA    R6,1(R6)                                                         
         STC   R6,TEMP+2           SET LINE NUMBER                              
         LA    R1,3(R1)                                                         
         STC   R1,TEMP+1                                                        
         MVI   TEMP,X'05'                                                       
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
DATAV12  ZIC   R1,0(R5)            BUMP TO NEXT TWA FIELD                       
         AR    R5,R1                                                            
         TM    1(R5),X'20'         SKIP PROTS                                   
         BO    DATAV12                                                          
         CLI   0(R5),9             TEST E-O-T                                   
         BH    DATAV10                                                          
         BAS   RE,DISFORMA         DISPLAY ALPHA FORMULA                        
*                                  ADD/WRITE RECORD                             
DATAVEND LA    R1,DEMFILEH                                                      
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         LA    R1,BASACTNH         SET CURSOR & EXIT                            
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE DEMO FORMULA & BUILD POLISH ELEMENT IF STRING VALID.                 
*                                                                               
* ON EXIT FERN=X'FE' WITH MESSAGE SET ON ERROR.                                 
*                                                                               
VALFORM  NTR1                                                                   
         LA    R1,DEMFORMH         R1=A(FIRST TWA FORMULA LINE)                 
         ST    R1,FADR                                                          
         LA    R5,IADR             R5=A(LIST OF FLD HRD ADDRESSES)              
         LA    R6,ISTRING          R6=A(CONVERTED INPUT STRING)                 
         SR    R7,R7               R7=L'CONVERTED INPUT STRING                  
         SR    R8,R8               R8=N'INPUT TWA LINES                         
         MVI   PREVCLAS,0          INIT                                         
         MVI   CURRCLAS,0                                                       
*                                                                               
VALF2    L     R1,FADR                                                          
         ST    R1,0(R5)            SET A(FLD HDR) IN TABLE                      
         LA    R5,4(R5)                                                         
         LA    R8,1(R8)            BUMP TWA LINE COUNT                          
         GOTO1 AFVAL               EXTRACT FIELD FROM TWA                       
         BZ    VALF4                                                            
         STC   R8,0(R6)            SET TWA LINE NUMBER IN STRING                
         ZIC   R1,FLDH+5                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R6),FLD         MOVE INPUT TO CONVERTED STRING               
         LA    R6,1(R1,R6)         BUMP TO NEXT SLOT                            
         LA    R7,1(R1,R7)         BUMP CONVERTED STRING LENGTH                 
VALF4    L     R1,FADR             BUMP TO NEXT TWA FIELD                       
         ZIC   RE,0(R1)                                                         
         AR    R1,RE                                                            
         ST    R1,FADR                                                          
         TM    1(R1),X'20'         IGNORE PROTS                                 
         BO    VALF4                                                            
         CLI   0(R1),9             TEST E-O-T                                   
         BH    VALF2                                                            
         LTR   R7,R7               YES - ANY INPUT                              
         BNZ   VALF6                                                            
         MVC   FADR,IADR                                                        
         B     EMIF                NO - ERROR                                   
*                                  CONVERT INPUT STRING TO POLISH               
VALF6    MVI   0(R6),X'FF'         SET END OF CONVERTED STRING                  
         LA    R6,ISTRING          R6=A(INPUT STRING)                           
         LA    R8,TEMP             R8=A(INTERMEDIATE STRING)                    
         MVI   PARENLEV,0          INITIALIZE W/S VALUES                        
         MVI   LASTON,1                                                         
         MVI   ODNUM,0                                                          
*                                                                               
VALF8    CLI   0(R6),16            TEST IF NEW TWA LINE                         
         BH    VALF10                                                           
         ZIC   R1,0(R6)            YES - SET FADR FOR ERROR ROUTINES            
         SLL   R1,2                                                             
         LA    R1,IADR-4(R1)                                                    
         MVC   FADR,0(R1)                                                       
         MVI   FNDX,1                                                           
         LA    R6,1(R6)            BUMP TO NEXT                                 
*                                                                               
VALF10   CLI   0(R6),X'FF'         TEST IF END OF INPUT STRING                  
         BE    VALF44                                                           
         MVC   PREVCLAS,CURRCLAS   FOR PSUEDO CHECKING                          
*                                                                               
         LA    R1,ONTAB            TEST IF AN OPERATION                         
VALF12   CLI   0(R1),0                                                          
         BE    VALF18                                                           
         CLC   0(1,R1),0(R6)                                                    
         BE    *+12                                                             
         LA    R1,L'ONTAB(R1)                                                   
         B     VALF12                                                           
         MVC   CURRCLAS,1(R1)                                                   
*                                                                               
         TM    1(R1),X'80'         TEST PARENTHESES                             
         BO    VALF14                                                           
         CLI   LASTON,0            TEST IF OPERAND PROCESSED                    
         BNE   VALFE1                                                           
         TM    1(R1),X'20'         TEST IF A PSEUDO OPERATION/AND               
         BZ    *+16                                                             
         TM    PREVCLAS,X'20'      CANNOT HAVE TWO IN A ROW                     
         BO    VALFED                                                           
         B     VALF17                                                           
         MVC   LASTON,0(R6)        YES - SAVE OPERATION                         
         B     VALF17                                                           
*                                  HANDLE OPEN PARENTHESIS                      
VALF14   TM    1(R1),X'40'                                                      
         BZ    VALF16                                                           
         CLI   LASTON,0            TEST IF OPERATION BEFORE OPEN                
         BE    VALFE2                                                           
         MVI   LASTON,1            SET START OF STRING                          
         ZIC   R1,PARENLEV         BUMP PARENTHESIS LEVEL                       
         LA    R1,1(R1)                                                         
         STC   R1,PARENLEV                                                      
         B     VALF17                                                           
*                                  HANDLE CLOSE PARENTHESIS                     
VALF16   SR    R1,R1                                                            
         ICM   R1,1,PARENLEV                                                    
         BZ    VALFE3              MUST HAVE PROCESSED OPEN                     
         BCTR  R1,0                                                             
         STC   R1,PARENLEV                                                      
         MVI   LASTON,0                                                         
*                                  MOVE OPERATION TO STRING                     
VALF17   MVC   0(1,R8),0(R6)                                                    
         LA    R8,1(R8)                                                         
         LA    R6,1(R6)                                                         
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     VALF8                                                            
*                                  VALIDATE OPERAND (ANNN,P OR ANNN.P)          
VALF18   CLI   LASTON,1                                                         
         BNE   *+8                                                              
         MVI   LASTON,0            RESET START OF STRING                        
         XC    WORK,WORK                                                        
         LA    R1,WORK+1                                                        
         SR    RE,RE                                                            
VALF20   CLI   0(R6),C'.'          TEST FOR PRECISION INDICATORS                
         BE    VALF24                                                           
         CLI   0(R6),C','                                                       
         BE    VALF24                                                           
         CLI   0(R6),C'9'          TEST FOR OPERATIONS/SPECIALS                 
         BH    VALF30                                                           
         CLI   0(R6),C'A'                                                       
         BL    VALF30                                                           
*                                  VALIDATE ANNN                                
VALF22   CLI   WORK,0              TEST IF VALIDATED                            
         BNE   VALF26              YES - MUST BE PRECISION INDIC                
         LTR   RE,RE               FIRST CHARACTER CAN BE ANYTHING              
         BZ    VALF28                                                           
         CH    RE,=H'5'            FIELD CAN'T BE LONGER THAN 5 BYTES           
         BNH   *+16                                                             
         TM    WORK+1,X'F0'                                                     
         BO    VALFEB                                                           
         B     VALFE5                                                           
         TM    0(R6),X'F0'         OTHER CHARACTERS MUST BE NUMERIC             
         BNO   VALFE5                                                           
         B     VALF28                                                           
*                                  VALIDATE PRECISION MODIFIER                  
VALF24   TM    WORK+1,X'F0'        CONSTANTS CAN'T HAVE PREC MODIFIER           
         BO    VALFEA                                                           
         CLI   WORK,0              ONLY 1 MODIFIER REQUIRED                     
         BNE   VALFE5                                                           
         CH    RE,=H'2'            ENSURE OPERAND VALID                         
         BL    VALFE5                                                           
         STC   RE,WORK             SET L'OPERAND                                
         SR    RE,RE                                                            
         LA    R1,WORK+10          MODIFIER GOES INTO WORK+10                   
         B     VALF28                                                           
*                                  VALIDATE PRECISION (0-4)                     
VALF26   CH    RE,=H'1'                                                         
         BNE   VALFE5                                                           
         CLI   0(R6),C'0'                                                       
         BL    VALFE6                                                           
         CLI   0(R6),C'4'                                                       
         BH    VALFE6                                                           
*                                  BUMP TO NEXT STRING CHARACTER                
VALF28   MVC   0(1,R1),0(R6)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         LA    R6,1(R6)                                                         
         ZIC   RF,FNDX                                                          
         LA    RF,1(RF)                                                         
         STC   RF,FNDX                                                          
         B     VALF20                                                           
*                                  END OF OPERAND LOCATED                       
VALF30   CLI   WORK,0              TEST IF PRECISION MODIFIER FOUND             
         BNE   VALF32                                                           
         TM    WORK+1,X'F0'        TEST IF A CONSTANT FIELD                     
         BNO   VALF31                                                           
         STC   RE,WORK             YES - VALIDATE NUMERIC 1 THRU 65535          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+1(0)                                                    
         CVB   R1,DUB                                                           
         ST    R1,DUB                                                           
         OC    DUB(4),DUB                                                       
         BZ    VALFEB                                                           
         OC    DUB(2),DUB                                                       
         BNZ   VALFEB                                                           
         MVI   DUB,X'FF'           SPECIAL MODIFIER FOR CONSTANTS               
         MVC   DUB+1(2),DUB+2                                                   
         B     VALF34                                                           
*                                                                               
VALF31   CH    RE,=H'2'            NO - ENSURE OPERAND LENGTH VALID             
         BL    VALFE5                                                           
         STC   RE,WORK                                                          
*                                  CONVERT OPERAND INTO INTERNAL FMT            
VALF32   ZIC   RE,WORK                                                          
         SH    RE,=H'2'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK+2(0)                                                    
         CVB   R1,DUB                                                           
         ST    R1,DUB                                                           
         OC    DUB(3),DUB                                                       
         BNZ   VALFE7                                                           
         MVC   DUB(1),WORK+1                                                    
         MVC   DUB+1(1),DUB+3                                                   
         TM    PRECISN,X'20'       TEST IF FORMULA PRECISION ADJUSTMNT          
         BZ    *+14                                                             
         CLC   DUB(2),CTGKDEMO                                                  
         BNE   VALFEC                                                           
         MVI   DUB+2,X'FF'         SET NO PRECISION ADJUSTMENTS                 
         OC    WORK+10(2),WORK+10  TEST IF PRECISION INPUT                      
         BZ    VALF34                                                           
         MVC   DUB+2(1),WORK+11                                                 
         NI    DUB+2,X'0F'                                                      
         CLI   WORK+10,C'.'                                                     
         BNE   *+12                                                             
         OI    DUB+2,X'80'         X'8N' = N DECIMAL PLACES                     
         B     *+8                                                              
         OI    DUB+2,X'40'         X'4N' = N INTEGER PLACES                     
*                                  ALLOCATE NUMBER TO OPERAND                   
VALF34   LA    RE,ODSTACK          RE=A(OPERAND STACK)                          
         SR    R1,R1                                                            
         ICM   R1,1,ODNUM          R1=N'ENTRIES IN STACK                        
         BZ    VALF38                                                           
         LR    RF,R1                                                            
*                                  TEST IF ALREADY ALLOCATED A NUMBER           
VALF36   CLC   0(3,RE),DUB                                                      
         BE    VALF40                                                           
         LA    RE,4(RE)                                                         
         BCT   RF,VALF36                                                        
*                                  ASSIGN NEW NUMBER TO OPERAND                 
VALF38   LA    R1,1(R1)                                                         
         STC   R1,ODNUM                                                         
         MVC   0(3,RE),DUB                                                      
         STC   R1,3(RE)                                                         
*                                  NOW PROCESS OPERAND                          
VALF40   MVC   DUB+3(1),3(RE)                                                   
         MVC   0(1,R8),DUB+3       SET OPERAND NUMBER IN OUTPUT STRING          
         LA    R8,1(R8)                                                         
         MVI   LASTON,0                                                         
         B     VALF8                                                            
*                                  HANDLE END OF INPUT STRING                   
VALF44   CLI   LASTON,0            TEST LAST OPERAND PROCESSED                  
         BNE   VALFE8                                                           
         CLI   PARENLEV,0          TEST ALL PARENTHESES PAIRED                  
         BNE   VALFE3                                                           
         MVI   0(R8),C'='          SET END-OF-FORMULA                           
         MVI   1(R8),X'FF'                                                      
*                                  CONVERT STRING TO POLISH FORMAT              
         LA    R6,TEMP             R6=A(INTERMEDIATE STRING)                    
         LA    R8,OSTRING          R8=A(OUTPUT STRING)                          
         SR    R1,R1               R1=CURRENT PAREN LEVEL                       
         XC    ONSTACK,ONSTACK                                                  
*                                                                               
VALF45   CLI   0(R6),X'FF'         TEST END-OF-STRING                           
         BE    VALF48                                                           
         CLI   0(R6),C'='          TEST END-OF-FORMULA                          
         BE    VALF46D                                                          
*                                  DEAL WITH OPERANDS                           
         LA    RE,ONTAB            CHECK FOR PSUEDO OPERATOR                    
VAL45A   CLI   0(RE),0                                                          
         BE    VAL45B                                                           
         CLC   0(1,R6),0(RE)       TRY TO MATCH OPERATORS                       
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     VAL45A                                                           
*                                                                               
         CLI   1(RE),X'20'         PSUEDOS ARE LIKE OPERANDS                    
         BE    *+8                                                              
VAL45B   CLI   0(R6),X'40'         TRUE OPERATOR                                
         BH    VALF46A                                                          
*                                                                               
         LA    RE,ONTAB            CHECK FOR PSUEDO OPERATOR                    
VAL45C   CLI   0(RE),0               IN NEXT POSTION                            
         BE    VAL45D                                                           
         CLC   1(1,R6),0(RE)       TRY TO MATCH OPERATORS                       
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     VAL45C                                                           
         CLI   1(RE),X'20'         DELAY SAVED OPERATOR IF PSUEDO               
         BE    VALF46MV                                                         
*                                                                               
VAL45D   CLI   LASTON,0            TEST IF OPERATION SAVED                      
         BE    VALF46MV            NO - OUTPUT OPERAND                          
         MVC   0(1,R8),0(R6)       YES - OUTPUT OPERAND/OPERATION               
         LA    R8,1(R8)                                                         
         MVC   0(1,R8),LASTON                                                   
         MVI   LASTON,0                                                         
         B     VALF4686                                                         
*                                  DEAL WITH OPEN PAREN                         
VALF46A  CLI   0(R6),C'('                                                       
         BNE   VALF46B                                                          
         LA    RE,ONSTACK(R1)      RE=A(SAVE OPERATION STACK ENTRY)             
         LA    R1,1(R1)            BUMP PAREN LEVEL                             
         CLI   LASTON,0            TEST IF OPERATION SAVED                      
         BE    *+10                                                             
         MVC   0(1,RE),LASTON      YES - SAVE IN STACK                          
         MVI   LASTON,0                                                         
         B     VALF466                                                          
*                                  DEAL WITH CLOSE PAREN                        
VALF46B  CLI   0(R6),C')'                                                       
         BNE   VALF46C                                                          
         BCTR  R1,0                DECREMENT PAREN LEVEL                        
         LA    RE,ONSTACK(R1)      RE=A(SAVED OPERATION STACK ENTRY)            
         CLI   0(RE),0             TEST IF OPERATION SAVED                      
         BE    VALF466                                                          
         MVC   0(1,R8),0(RE)       YES - OUTPUT SAVED OPERATION                 
         MVI   0(RE),0                                                          
         B     VALF4686                                                         
*                                  DEAL WITH OPERATIONS                         
VALF46C  CLI   LASTON,0            TEST IF OPERATION SAVED                      
         BE    *+14                                                             
         MVC   0(1,R8),LASTON      YES - OUTPUT SAVED OPERATION                 
         LA    R8,1(R8)                                                         
         MVC   LASTON,0(R6)        SAVE OPERATION IN TEMPORARY SAVE             
         B     VALF466                                                          
*                                  DEAL WITH EQUALS SIGN                        
VALF46D  CLI   LASTON,0            TEST IF OPERATION SAVED                      
         BE    *+14                                                             
         MVC   0(1,R8),LASTON      YES - OUTPUT SAVED OPERATION                 
         LA    R8,1(R8)                                                         
*                                  MOVE TO OUTPUT STRING & BUMP PTRS            
VALF46MV MVC   0(1,R8),0(R6)                                                    
VALF4686 LA    R8,1(R8)                                                         
VALF466  LA    R6,1(R6)                                                         
         B     VALF45                                                           
*                                  BUILD POLISH ELEMENT                         
VALF48   LA    R1,TEMP+2                                                        
         XC    TEMP,TEMP                                                        
*                                  MOVE OPERANDS TO ELEMENT                     
         LA    RE,ODSTACK                                                       
         ZIC   R0,ODNUM                                                         
         SR    RF,RF                                                            
VALF50   MVC   0(3,R1),0(RE)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,VALF50                                                        
*                                  MOVE POLISH STRING TO ELEMENT                
         LA    RE,OSTRING                                                       
         SR    R8,RE                                                            
         LA    RF,2(R8,RF)                                                      
         CH    RF,=H'255'          TEST DATA WILL FIT IN ELEMENT                
         BH    VALFE9                                                           
         STC   RF,TEMP+1                                                        
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),OSTRING                                                  
         MVI   TEMP,X'04'                                                       
         MVI   FERN,X'FF'          SET VALID EXIT                               
         B     EXIT                                                             
         EJECT                                                                  
*              HANDLE VALIDATION ERRORS                                         
*                                                                               
VALFE1   LA    R1,VALERR1                                                       
         B     VALERR                                                           
VALFE2   LA    R1,VALERR2                                                       
         B     VALERR                                                           
VALFE3   LA    R1,VALERR3                                                       
         B     VALERR                                                           
VALFE4   LA    R1,VALERR4                                                       
         B     VALERR                                                           
VALFE5   LA    R1,VALERR5                                                       
         B     VALERR                                                           
VALFE6   LA    R1,VALERR6                                                       
         B     VALERR                                                           
VALFE7   LA    R1,VALERR7                                                       
         B     VALERR                                                           
VALFE8   LA    R1,VALERR8                                                       
         B     VALERR                                                           
VALFE9   LA    R1,VALERR9                                                       
         B     VALERR                                                           
VALFEA   LA    R1,VALERRA                                                       
         B     VALERR                                                           
VALFEB   LA    R1,VALERRB                                                       
         B     VALERR                                                           
VALFEC   LA    R1,VALERRC                                                       
         B     VALERR                                                           
VALFED   LA    R1,VALERRD                                                       
         B     VALERR                                                           
*                                  FORMAT MESSAGE                               
VALERR   MVC   BASHDR,0(R1)                                                     
         CLI   FNDX,0                                                           
         BE    VALERRX                                                          
         LA    R1,BASHDR+L'BASHDR-1                                             
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(8,R1),=C'NEAR COL'                                             
         ZIC   RE,FNDX                                                          
         CVD   RE,DUB                                                           
         UNPK  11(2,R1),DUB                                                     
         OI    12(R1),X'F0'                                                     
*                                                                               
VALERRX  OI    BASHDRH+6,X'80'                                                  
         MVI   FERN,X'FE'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        DISPLAY THE ALPHA FORMULA FIELD(S)                                     
*                                                                               
DISFORMA NTR1                                                                   
         LA    R8,FORALPHA                                                      
         LA    RE,DEMFORAH         CLEAR 6 ALPHA FORMULA FIELDS                 
         LA    RF,6                & TURN ON TRANSMIT BITS                      
         SR    R6,R6                                                            
         IC    R6,0(RE)                                                         
         SH    R6,=H'9'                                                         
         LR    R7,R6                                                            
         B     DA060               1ST FIELD                                    
DA040    SR    R7,R7                                                            
         IC    R7,0(RE)            LAST 5 FIELDS MUST BE TESTED                 
         AR    RE,R7                                                            
         IC    R7,0(RE)                                                         
         SH    R7,=H'9'                                                         
         CR    R7,R6               CHECK FOR EQUAL LENGTH TO DEMFORAH           
         BNE   DA040                                                            
DA060    EX    R6,*+8                                                           
         B     *+10                                                             
         XC    8(0,RE),8(RE)       CLEAR FIELD                                  
         OI    6(RE),X'80'         TURN ON TRANSMIT BIT                         
         ST    RE,0(R8)            SAVE ADDRESS OF FIELD                        
         LA    R7,8(R7)                                                         
         AR    R7,RE                                                            
         ST    R7,4(R8)            SAVE END ADDRESS OF FIELD                    
         LA    R8,8(R8)                                                         
         BCT   RF,DA040                                                         
         XC    0(4,R8),0(R8)                                                    
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,DEMFORMH+5     LENGTH OF FORMULA I/P (1ST LINE)             
         BZ    EXIT                                                             
*                                  SET UP DBLOCK FOR DEMOCON                    
         L     RF,APARM                                                         
         L     RE,12(RF)           A(COMFACS)                                   
         XC    TEMP,TEMP           TEMP USED FOR DBLOCK                         
         USING DEDBLOCK,R1                                                      
         LA    R1,TEMP                                                          
         MVC   DBFILE,SVDBFILE     FILE                                         
         MVC   DBSELMED,CTGKMED    MEDIA                                        
         MVC   DBSELSRC,CTGKSRC    SOURCE                                       
         CLI   DBSELMED,C'D'                                                    
         BNE   *+8                                                              
         MVI   DBSELMED,C'T'                                                    
         ST    RE,DBCOMFCS         A(COMFACS)                                   
*                                                                               
         EJECT                                                                  
*                                  MOVE I/P LINES TO FLD (MAX.=240)             
         LA    R6,FLD                                                           
         LA    R1,DEMFORMH                                                      
         LA    R5,4                                                             
         SR    RE,RE                                                            
         SR    R7,R7                                                            
DA120    ICM   RE,1,5(R1)          LENGTH OF I/P                                
         BZ    DA160                                                            
         AR    R7,RE               R7 - TOTAL LENGTH OF I/P                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),8(R1)       MOVE LINE TO FLD                             
         AR    R6,RE                                                            
         LA    R6,1(R6)                                                         
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         BCT   R5,DA120                                                         
*                                                                               
DA160    MVI   0(R6),X'FF'         SET END-OF-DATA                              
         STC   R7,FLDH+5           TOTAL LENGTH OF I/P                          
*                                                                               
         LA    R8,FORALPHA         ADDRESSES OF 6 ALPHA FIELDS                  
         L     RF,0(R8)                                                         
         LA    R5,8(RF)            R5 - O/P                                     
         LA    R6,FLD              R6 - I/P                                     
*                                                                               
*                                  TRANSFER I/P TO ALPHA FORMULA                
DA300    CLI   0(R6),X'FF'                                                      
         BE    DA900                                                            
         CLI   0(R6),C'A'          LOOKING FOR ANNN                             
         BL    DA320                                                            
         CLI   0(R6),C'Z'                                                       
         BNH   DA400                                                            
*                                                                               
DA320    MVC   0(1,R5),0(R6)       MOVE TO O/P                                  
         CLI   0(R6),C','                                                       
         BE    DA330                                                            
         CLI   0(R6),C'.'                                                       
         BE    DA330                                                            
         CLI   0(R6),C'('                                                       
         BE    DA330                                                            
         CLI   0(R6),C'A'                                                       
         BNL   DA330                                                            
         STM   R5,R6,SAVEADDR      STORE ADDRESSES OF LAST OPERATOR             
DA330    LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
*                                  TEST FOR END OF O/P FIELD                    
         L     RE,4(R8)                                                         
         CR    RE,R5                                                            
         BNL   DA300               RETURN TO SCAN                               
*                                                                               
         EJECT                                                                  
DA340    BCTR  R5,0                                                             
         C     R5,SAVEADDR                                                      
         BE    DA380                                                            
         MVI   0(R5),C' '          BLANK OUT END OF LINE                        
         B     DA340                                                            
DA380    L     R6,SAVEADDR+4       BACKUP I/P FIELD                             
         LA    R6,1(R6)                                                         
         LA    R8,8(R8)                                                         
         OC    0(4,R8),0(R4)                                                    
         BZ    EXIT                                                             
         L     RF,0(R8)                                                         
         LA    R5,8(RF)                                                         
         B     DA300               RETURN TO SCAN                               
*                                                                               
*                                  AN, - ANN, - ANNN,                           
DA400    EQU   *                   TEST FOR END OF O/P FIELD                    
         L     RE,4(R8)                                                         
         SH    RE,=H'9'                                                         
         CR    RE,R5                                                            
         BNL   DA410               OK                                           
         LA    R8,8(R8)                                                         
         OC    0(4,R8),0(R8)       ADDRESS OF NEXT O/P FIELD                    
         BZ    EXIT                                                             
         L     RF,0(R8)                                                         
         LA    R5,8(RF)                                                         
*                                                                               
DA410    LA    RE,2(R6)                                                         
         LA    RF,3                                                             
         SR    R1,R1                                                            
DA420    CLI   0(RE),C'0'          SCAN FOR ANY NON-NUMERIC                     
         BL    DA440                                                            
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         BCT   RF,DA420                                                         
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
*                                  CONVERT NNN TO BINARY                        
DA440    MVC   DUB,=8C'0'                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   DUB(0),1(R6)                                                     
         CLC   DUB,=8C'0'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R6)                                                      
         CVB   R1,DUB                                                           
         MVI   WORK+0,X'00'        WORK(3) - I/P TO DEMOCON                     
         MVI   WORK+1,C'R'         PASS 'R' TO DEMOCON                          
         STC   R1,WORK+2                                                        
         LR    R6,RE               R6 - ADDRESS OF COMMA                        
         XC    WORK+4(16),WORK+4                                                
         GOTO1 VDEMOCON,DMCB,(1,WORK),(6,WORK+4),(0,TEMP)                       
*                                                                               
         LA    RE,WORK+5           SUPPRESS 'R'                                 
         LA    RF,8                                                             
DA480    CLI   0(RE),X'00'                                                      
         BE    DA300               RETURN TO SCAN                               
         CLI   0(RE),C' '                                                       
         BE    DA300               RETURN TO SCAN                               
         CLI   0(RE),C'+'          CHANGE A65+ TO A65P                          
         BNE   *+8                                                              
         MVI   0(RE),C'P'                                                       
         MVC   0(1,R5),0(RE)       MOVE TO O/P                                  
         LA    R5,1(R5)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,DA480                                                         
         DC    H'0'                                                             
*                                                                               
DA900    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
*                                  TABLE OF VALID FILES                         
FILETAB  DS    0CL14                                                            
         DC    C'T',C'TP ',CL10'TPT'                                            
         DC    C'T',C'TP ',CL10'DPT'                                            
         DC    C'C',C'TP ',CL10'COUNTY'                                         
         DC    C'C',C'PAV',CL10'CABLE'                                          
         DC    C'P',C'PAV',CL10'PAV'                                            
         DC    C'M',C'NAD',CL10'MOVIE'                                          
         DC    C'N',C'NAD',CL10'NAD'                                            
         DC    C'E',C'EVN',CL10'ESTIMATE'                                       
         DC    C'I',C'INV',CL10'INV'                                            
         DC    C'I',C'IAG',CL10'IAG'                                            
         DC    C'O',C'OPT',CL10'OPTIMUM'                                        
         DC    C'A',C'AE ',CL10'AUD ESTIM'                                      
         DC    X'00'                                                            
*                                  TABLE OF VALID SUB-FILES                     
MEDTAB   DS    0CL9                                                             
         DC    C'T',CL8'USTV'                                                   
         DC    C'C',CL8'CANTV'                                                  
         DC    C'R',CL8'RADIO'                                                  
         DC    C'D',CL8'DPT'                                                    
         DC    C'N',CL8'NETWORK'                                                
         DC    C'O',CL8'OVERNITE'                                               
         DC    C'H',CL8'NHI'                                                    
         DC    C'P',CL8'MPA'                                                    
         DC    C'V',CL8'VPH'                                                    
         DC    C'I',CL8'IMP'                                                    
         DC    C'U',CL8'UPGRADE'                                                
         DC    C'W',CL8'WEEKLY'                                                 
         DC    C'A',CL8'A'                                                      
         DC    X'00'                                                            
*                                  TABLE OF VALID SOURCE CODES                  
SRCTAB   DS    0CL9                                                             
         DC    C'A',CL8'ARB'                                                    
         DC    C'N',CL8'NSI'                                                    
         DC    C'F',CL8'FUS'                                                    
         DC    C'S',CL8'SRC'                                                    
         DC    C'M',CL8'BBR'                                                    
         DC    C'M',CL8'MFX'                                                    
         DC    C'I',CL8'IDX'       OPTIMUM PROGRAM INDEX                        
         DC    C'Q',CL8'TVQ'                                                    
         DC    C'R',CL8'RAR'       RADAR NET RADIO                              
         DC    C'G',CL8'G'         IAG FACTORS                                  
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
*                                  VALID FILE/MEDIA/SOURCE COMBINATIONS         
FMSTAB   DS    0CL3                                                             
         DC    C'CNN'                                                           
         DC    C'CHN'             CABLE NHTI FILE                               
         DC    C'CUN'             COUNTY COVERAGE - NSI US                      
         DC    C'TON'             NSI OVERNITES - USTV                          
         DC    C'TTN'                                                           
         DC    C'TTF'             FUSION                                        
         DC    C'TTM'             MEDIAFAX TPT                                  
         DC    C'TTA'                                                           
         DC    C'TTS'                                                           
         DC    C'TRA'                                                           
         DC    C'TRN'                                                           
         DC    C'TRM'                                                           
         DC    C'TRR'              RADAR RADIO                                  
         DC    C'TCN'                                                           
         DC    C'TCA'                                                           
         DC    C'TWA'              BBM TV WEEKLY                                
         DC    C'TDN'                                                           
         DC    C'TDA'                                                           
         DC    C'MNN'              MOVIEGOER                                    
         DC    C'PTM'             MEDIAFAX PAV                                  
         DC    C'PTN'                                                           
         DC    C'PTA'                                                           
         DC    C'PNN'                                                           
         DC    C'TPN'                                                           
         DC    C'TPA'                                                           
         DC    C'NHN'                                                           
         DC    C'NNN'                                                           
         DC    C'EVN'                                                           
         DC    C'EIN'                                                           
         DC    C'IUA'                                                           
         DC    C'IUN'                                                           
         DC    C'TVQ'                                                           
         DC    C'TWN'                                                           
         DC    C'OPI'                                                           
         DC    C'ANN'                                                           
         DC    C'IAG'                                                           
         DC    X'00'                                                            
*                                  TABLE OF VALID FIELD PRECISIONS              
PRECTAB  DS    0CL11                                                            
         DC    X'44',CL10'(0000)'                                               
         DC    X'43',CL10'THOUSANDS'                                            
         DC    X'42',CL10'HUNDREDS'                                             
         DC    X'41',CL10'TENS'                                                 
         DC    X'40',CL10'INTEGER'                                              
         DC    X'81',CL10'1DECIMAL'                                             
         DC    X'82',CL10'2DECIMAL'                                             
         DC    X'83',CL10'3DECIMAL'                                             
         DC    X'84',CL10'4DECIMAL'                                             
         DC    X'20',CL10'FORMULA'                                              
         DC    X'64',CL10'F(0000)'                                              
         DC    X'63',CL10'FTHOUSANDS'                                           
         DC    X'62',CL10'FHUNDREDS'                                            
         DC    X'61',CL10'FTENS'                                                
         DC    X'60',CL10'FINTEGER'                                             
         DC    X'A1',CL10'F1DECIMAL'                                            
         DC    X'A2',CL10'F2DECIMAL'                                            
         DC    X'A3',CL10'F3DECIMAL'                                            
         DC    X'A4',CL10'F4DECIMAL'                                            
         DC    X'00'                                                            
*                                  TABLE OF VALID OPERATIONS                    
ONTAB    DS    0XL2                                                             
         DC    C'+',X'00'                                                       
         DC    C'-',X'00'                                                       
         DC    C'*',X'00'                                                       
         DC    C'/',X'00'                                                       
         DC    C'(',X'C0'                                                       
         DC    C')',X'80'                                                       
         DC    C'''',X'20'                                                      
         DC    C'"',X'20'                                                       
         DC    C'!',X'20'                                                       
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
OPTMACS  DS    0XL10                                                            
         DC    X'10',C'DEFAULT  '                                               
         DC    X'00',C'OVERRIDE '                                               
         DC    X'FF'                                                            
OPTDEMS  DS    0XL10                                                            
         DC    X'10',C'NOVALUE  '                                               
         DC    X'FF'                                                            
*                                  TABLE OF ERROR MESSAGES                      
VALERR1  DC    CL60'OPERATOR NOT PRECEEDED BY OPERAND'                          
VALERR2  DC    CL60'PARENTHESIS NOT PRECEEDED BY OPERATION'                     
VALERR3  DC    CL60'UNPAIRED PARENTHESIS'                                       
VALERR4  DC    CL60'PARENTHESIS NOT PRECEEDED BY OPERAND'                       
VALERR5  DC    CL60'INVALID OPERAND FORMAT S/B ANNN(,N) OR ANNN(.N)'            
VALERR6  DC    CL60'INVALID PRECISION MODIFIER S/B 0 THRU 4'                    
VALERR7  DC    CL60'INVALID DEMO NUMBER'                                        
VALERR8  DC    CL60'MISSING OPERAND'                                            
VALERR9  DC    CL60'FORMULA TOO LONG'                                           
VALERRA  DC    CL60'PRECISION MODIFIER NOT ALLOWED AFTER CONSTANT'              
VALERRB  DC    CL60'INVALID CONSTANT VALUE S/B 1 THRU 65535'                    
VALERRC  DC    CL60'OUTPUT PRECISION=FORMULA INCOMPATABLE'                      
VALERRD  DC    CL60'PSUEDO OP PRECEEDED BY PSUEDO OP'                           
         SPACE 1                                                                
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
WORKD    DSECT                                                                  
VDEMOCON DS    V                   ADDRESS OF DEMOCON                           
FORALPHA DS    12A,A               ADDRESSES OF 6 ALPHA FORMULA FIELDS          
SAVEADDR DS    2A                  SAVE ADDRESSES IN DISFORMA ROUTINE           
SVDBFILE DS    CL3                                                              
ODNUM    DS    X                                                                
PRECISN  DS    X                                                                
ODSTACK  DS    64CL4                                                            
ONSTACK  DS    CL32                                                             
LASTON   DS    X                                                                
PARENLEV DS    X                                                                
DIROPT   DS    X                                                                
PREVCLAS DS    X                                                                
CURRCLAS DS    X                                                                
IADR     DS    6A                                                               
ISTRING  DS    6CL80                                                            
OSTRING  DS    CL256                                                            
WORKDX   EQU   *                                                                
         SPACE 1                                                                
* CTLFMACTNS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMF0D                                                                      
       ++INCLUDE CTLFMF0D                                                       
* COMFACS                                                                       
       ++INCLUDE DDCOMFACS                                                      
* DEDEBLOCK                                                                     
DEDBLOCK DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047CTLFM0F   07/13/09'                                      
         END                                                                    
