*          DATA SET CTLFM20    AT LEVEL 020 AS OF 05/01/02                      
*PHASE TA0220A,*                                                                
*INCLUDE SCINKEY                                                                
*                                                                               
*                                                                               
         TITLE 'CTLFM20 - CONTROL FILE MAINT - ACCESS RECORDS'                  
CTLFM20  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**LF20**,RA,RR=RE,CLEAR=YES                          
         USING WORKD,RC            RC=A(TEMP W/S)                               
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         ST    RE,RELO                                                          
         L     R0,=V(SCINKEY)                                                   
         AR    R0,RE                                                            
         ST    R0,VSCINKEY                                                      
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LA    R4,IOAREA                                                        
         USING CT5REC,R4           R4=A(IO)                                     
         EJECT                                                                  
* VALIDATE KEY FIELDS AND BUILD KEY                                             
*                                                                               
KEYVAL   XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
*                                                                               
         GOTO1 AFVAL,SYSALPHH                                                   
         BZ    EXIT                                                             
         CLI   FLDH+5,2                                                         
         BL    EFTS                                                             
         MVC   CT5KALPH,FLD                                                     
*                                                                               
         MVC   KEY,CT5KEY                                                       
         MVC   KEYNEXT,KEY                                                      
         CLI   ACTN,CHANGE                                                      
         BNE   KEYV2                                                            
         CLC   KEY,LKEY                                                         
         BE    KEYV2                                                            
         MVI   ACTN,DISPLAY                                                     
*                                                                               
KEYV2    CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET READ FOR UPDATE                          
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST RECORD NOT FOUND                        
         BZ    *+16                                                             
         CLI   ACTN,ADD            NOT FOUND ONLY VALID FOR ADD                 
         BE    DATAVAL                                                          
         B     ERNF                                                             
         CLI   ACTN,ADD            FOUND NOT VALID FOR ADD                      
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        TEST RECORD DELETED                          
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY        DELETED REC CAN ONLY BE DISPLAYED            
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
* DISPLAY ACCESS RECORD                                                         
*                                                                               
DISPREC  TWAXC SYSCTRYH            CLEAR UNPROTS IN TWA                         
         XC    SYSUSNM,SYSUSNM                                                  
         OI    SYSUSNMH+6,X'80'                                                 
         XC    SYSGRNM,SYSGRNM                                                  
         OI    SYSGRNMH+6,X'80'                                                 
         XC    SYSHONM,SYSHONM                                                  
         OI    SYSHONMH+6,X'80'                                                 
*&&UK                                                                           
         XC    SYSASAG,SYSASAG                                                  
         OI    SYSASAGH+6,X'80'                                                 
*&&                                                                             
         LA    R5,CT5DATA                                                       
*                                                                               
DISP2    CLI   0(R5),0             E-O-R                                        
         BE    DISP6                                                            
         CLI   0(R5),CTDSCELQ      USER-ID                                      
         BE    DISPID                                                           
         CLI   0(R5),CTSYSELQ      SYSTEM                                       
         BE    DISPSS                                                           
         CLI   0(R5),CTAGDELQ      AGENCY GROUP DETAILS                         
         BE    DISPAD                                                           
         CLI   0(R5),CTSEAELQ      SECURITY AGENCY ID                           
         BE    DISPSA                                                           
*&&UK                                                                           
         CLI   0(R5),CTAGLELQ      ASSOC AGENCY LIST                            
         BE    DISPAA                                                           
*&&                                                                             
*                                                                               
DISP4    SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DISP2                                                            
*                                                                               
DISP6    CLI   SYSCNT,0                                                         
         BE    DISPX                                                            
         ZIC   R0,SYSCNT                                                        
         GOTO1 VSCINKEY,DMCB,(2,SYSACCSH),(20,BLOCKI),(R0)                      
*                                                                               
DISPX    TM    CT5STAT,X'80'       OK TO RESTORE IF DELETED                     
         BZ    *+12                                                             
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
         LA    R1,SYSCTRYH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKDEL+OKCHA                                                
         B     EXIT                                                             
         EJECT                                                                  
         USING CTDSCD,R5                                                        
DISPID   GOTO1 GETID,CTDSC         DISPLAY PRINCIPAL ID                         
         MVC   SYSUSER,USERID                                                   
         MVC   SYSUSNM,USERNM                                                   
         B     DISP4                                                            
         SPACE 1                                                                
         USING CTAGDD,R5                                                        
DISPAD   OC    CTAGDAG,CTAGDAG     DISPLAY AGENCY GROUP                         
         BZ    DISPAD1             IF PRESENT                                   
         GOTO1 GETID,CTAGDAG                                                    
         MVC   SYSGRUP,USERID                                                   
         MVC   SYSGRNM,USERNM                                                   
DISPAD1  OC    CTAGDHG,CTAGDHG     DISPLAY HOLDING GROUP                        
         BZ    DISPAD2             IF PRESENT                                   
         GOTO1 GETID,CTAGDHG                                                    
         MVC   SYSHOLD,USERID                                                   
         MVC   SYSHONM,USERNM                                                   
DISPAD2  MVI   SYSDDSA,C' '        DISPLAY DDS ACCESS LEVEL                     
         CLI   CTAGDDA,0                                                        
         BE    DISPAD3                                                          
         TM    CTAGDDA,CTAGDDAY    BYPASS PASSWORD SECURITY                     
         BNO   *+12                                                             
         MVI   SYSDDSA,C'Y'                                                     
         B     DISPAD3                                                          
         TM    CTAGDDA,CTAGDDAN    NO DDS TERMINAL ACCESS                       
         BNO   *+12                                                             
         MVI   SYSDDSA,C'N'                                                     
         B     DISPAD3                                                          
         TM    CTAGDDA,CTAGDDAP    NO DDS AGENCY PASSWORD ACCESS                
         BNO   DISPAD3                                                          
         MVI   SYSDDSA,C'P'                                                     
DISPAD3  MVC   SYSTYPE,CTAGDTY     DISPLAY CLIENT TYPE                          
         CLI   CTAGDLEN,CTAGDL2Q                                                
         BL    DISP4                                                            
         L     R1,VCTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         CLC   CTRYCODE,CTAGDCTY                                                
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
         MVC   SYSCTRY,CTRYNAM     DISPLAY COUNTRY NAME                         
         B     DISP4                                                            
         DROP  R1                                                               
         SPACE 1                                                                
         USING CTSYSEL,R5                                                       
DISPSS   GOTO1 GETSE,CTSYSSE       FORMAT SYSTEM BLOCK ENTRY                    
         USING SELISTD,R1                                                       
         ZIC   RE,SYSCNT                                                        
         LA    R0,1(RE)                                                         
         STC   R0,SYSCNT                                                        
         LA    RF,20                                                            
         MR    RE,RE                                                            
         LA    R6,BLOCKI(RF)                                                    
         MVI   0(R6),C' '                                                       
         MVC   1(19,R6),0(R6)                                                   
         MVC   0(L'SENAME,R6),SENAME                                            
         LA    R6,L'SENAME-1(R6)                                                
         CLI   0(R6),C' '                                                       
         BNE   *+8                                                              
         BCT   R6,*-8                                                           
*                                  TEST IF BINARY AGENCY KEY                    
         GOTO1 GETSYS,DMCB,CTSYSNUM                                             
         ICM   R1,15,0(R1)         GET A(SYSLST ENTRY)                          
         BNZ   *+6                                                              
         DC    H'00'                                                            
         USING SYSLSTD,R1                                                       
         CLI   SYSLIND1,X'40'                                                   
         BNE   DISPSS10                                                         
         MVI   1(R6),C'='          IF SO DISPLAY VALUE                          
         GOTO1 VHEXOUT,DMCB,CTSYSAGB,2(R6),1,=C'TOG'                            
         DROP  R1                                                               
*                                                                               
DISPSS10 CLI   CTSYSNUM,X'02'      SPOT SYSTEM?                                 
         BNE   DISP4                                                            
*                                                                               
         OI    SYSCABLH+6,X'80'                                                 
         MVI   SYSCABL,C'N'                                                     
         TM    CTSYSIND,CTSYSNCA                                                
         BZ    *+8                                                              
         MVI   SYSCABL,C'Y'                                                     
         B     DISP4                                                            
         SPACE 1                                                                
*                                                                               
         USING CTSEAD,R5                                                        
DISPSA   MVC   SYSSECA,CTSEAAID    DISPLAY SECURITY AGENCY ID                   
         B     DISP4                                                            
         DROP  R5                                                               
*&&UK                                                                           
         USING CTAGLD,R5                                                        
DISPAA   ZIC   R0,CTAGLLEN         DISPLAY ASSOCIATED AGENCY LIST               
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         SRL   R0,1                                                             
         LA    RF,CTAGLAID                                                      
         LA    R1,SYSASAG                                                       
DISPAA1  MVC   0(L'CTAGLAID,R1),0(RF)                                           
         LA    RF,L'CTAGLAID(RF)                                                
         LA    R1,L'CTAGLAID(R1)                                                
         BCT   R0,*+8                                                           
         B     DISP4                                                            
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         B     DISPAA1                                                          
         DROP  R5                                                               
*&&                                                                             
         EJECT                                                                  
* CHANGE/ADD ACCESS RECORD                                                      
*                                                                               
DATAVAL  CLI   ACTN,ADD                                                         
         BNE   DVCHA                                                            
*                                                                               
         MVI   TEMP,0              ADD FUNCTION                                 
         GOTO1 ABLDREC                                                          
         B     DATAV1                                                           
*                                                                               
DVCHA    LA    R5,CT5DATA          AND STRIP DOWN RECORD                        
DVCHA10  CLI   0(R5),0                                                          
         BE    DVCHAX                                                           
         CLI   0(R5),X'01'                                                      
         BE    DVCHA30                                                          
         CLI   0(R5),X'02'                                                      
         BE    DVCHA30                                                          
         CLI   0(R5),CTAGDELQ                                                   
         BE    DVCHA30                                                          
         CLI   0(R5),CTAGLELQ                                                   
         BE    DVCHA30                                                          
         CLI   0(R5),CTSEAELQ                                                   
         BE    DVCHA30                                                          
         CLI   0(R5),CTSYSELQ                                                   
         BNE   DVCHA20                                                          
         MVI   0(R5),X'FF'         MARK SYSTEM ELEMENT                          
*                                  GET NEXT ELEMENT                             
DVCHA20  ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     DVCHA10                                                          
*                                                                               
DVCHA30  SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R5)                                                       
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE  '),((R0),CT5REC),0,0                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         B     DVCHA10                                                          
*                                                                               
DVCHAX   B     DATAV1                                                           
         EJECT                                                                  
*                                                                               
DATAV1   MVI   CTRY,0                                                           
         XC    CURR,CURR                                                        
         GOTO1 AFVAL,SYSCTRYH      VALIDATE COUNTRY                             
         BZ    EXIT                                                             
         L     R1,VCTRY                                                         
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1         R1=A(COUNTRY TABLE)                          
         ZIC   R7,FLDH+5                                                        
         BCTR  R7,0                R7=L'INPUT-1                                 
DATAV2   CLI   FLDH+5,L'CTRYSHR                                                 
         BH    DATAV4                                                           
         EX    R7,*+8              MATCH ON SHORT NAME                          
         BE    DATAV5                                                           
         CLC   CTRYSHR(0),FLD                                                   
DATAV4   EX    R7,*+8                                                           
         BE    DATAV5                                                           
         CLC   CTRYNAM(0),FLD      MATCH ON LONG NAME                           
         BXLE  R1,RE,DATAV2                                                     
         B     EIIF                                                             
DATAV5   MVC   CTRY,CTRYCODE       SAVE COUNTRY CODE                            
         CLC   CTRYNAM,SYSCTRY                                                  
         BE    *+14                                                             
         MVC   SYSCTRY(L'CTRYNAM),CTRYNAM                                       
         OI    SYSCTRYH+6,X'80'                                                 
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
DATAV6   XC    SYSUSNM,SYSUSNM                                                  
         OI    SYSUSNMH+6,X'80'                                                 
         GOTO1 AFVAL,SYSUSERH      VALIDATE PRINCIPAL ID                        
         BZ    EXIT                                                             
         GOTO1 VALID                                                            
         CLI   FERN,X'FF'          TEST FOR ERRORS                              
         BNE   EXIT                                                             
         MVC   SYSUSNM,USERNM                                                   
         XC    TEMP,TEMP           ADD ID NUMBER ELEMENT                        
         MVI   TEMP,CTDSCELQ                                                    
         MVI   TEMP+1,4                                                         
         MVC   TEMP+2(2),USERNO                                                 
         GOTO1 APUTEL                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SYSGRNM,SYSGRNM                                                  
         OI    SYSGRNMH+6,X'80'                                                 
         GOTO1 AFVAL,SYSGRUPH      VALIDATE AGENCY GROUP                        
*&&UK*&& BZ    EXIT                                                             
*&&US*&& BZ    DATAV7                                                           
         GOTO1 VALID                                                            
         CLI   FERN,X'FF'          TEST FOR ERRORS                              
         BNE   EXIT                                                             
         MVC   SYSGRNM,USERNM                                                   
         MVC   GRUPID,USERNO                                                    
*                                                                               
DATAV7   XC    SYSHONM,SYSHONM                                                  
         OI    SYSHONMH+6,X'80'                                                 
         GOTO1 AFVAL,SYSHOLDH      VALIDATE HOLDING COMPANY                     
*&&UK*&& BZ    EXIT                                                             
*&&US*&& BZ    DATAV9                                                           
         GOTO1 VALID                                                            
         CLI   FERN,X'FF'          TEST FOR ERRORS                              
         BNE   EXIT                                                             
         MVC   SYSHONM,USERNM                                                   
         MVC   HOLDID,USERNO                                                    
*                                                                               
DATAV9   GOTO1 AFVAL,SYSTYPEH                                                   
         BZ    EXIT                                                             
         MVC   CLTYPE,FLD                                                       
*                                                                               
         XC    DDSACC,DDSACC       VALIDATE DDS ACCESS LEVEL FIELD              
         GOTO1 AFVAL,SYSDDSAH                                                   
         BZ    DATAV9A                                                          
         CLI   FLD,C'Y'            BYPASS PASSWORD SECURITY ACCESS              
         BNE   *+12                                                             
         OI    DDSACC,CTAGDDAY                                                  
         B     DATAV9A                                                          
         CLI   FLD,C'N'            NO DDS TERMINAL ACCESS                       
         BNE   *+12                                                             
         OI    DDSACC,CTAGDDAN                                                  
         B     DATAV9A                                                          
         CLI   FLD,C'P'            NO DDS AGENCY PASSWORD                       
         BNE   EIIF                                                             
         OI    DDSACC,CTAGDDAP                                                  
         B     DATAV9A                                                          
*                                                                               
DATAV9A  LA    R1,TEMP                                                          
         USING CTAGDD,R1                                                        
         XC    CTAGDEL(CTAGDL2Q),CTAGDEL                                        
         MVI   CTAGDEL,CTAGDELQ                                                 
         MVI   CTAGDLEN,CTAGDL2Q                                                
         MVC   CTAGDHG,HOLDID                                                   
         MVC   CTAGDAG,GRUPID                                                   
         MVC   CTAGDTY,CLTYPE                                                   
         MVC   CTAGDCTY,CTRY                                                    
         MVC   CTAGDCUR,CURR                                                    
         MVC   CTAGDDA,DDSACC                                                   
         GOTO1 APUTEL                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
         LA    R7,SYSACCSH         VALIDATE SYSTEM ACCESS LIST                  
         LA    R8,2                                                             
         XC    OVSYS,OVSYS                                                      
         B     DATAV10                                                          
*                                                                               
DATAV8   SR    R0,R0               BUMP TO NEXT TWA FIELD                       
         IC    R0,0(R7)                                                         
         AR    R7,R0                                                            
         TM    1(R7),X'20'         TEST PROTECTED                               
         BNZ   DATAV8                                                           
*                                                                               
DATAV10  LR    R1,R7               VALIDATE NEXT SCAN FIELD                     
         GOTO1 AFVAL                                                            
         BZ    DATAV14                                                          
         MVI   FNDX,0                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCKI)                                   
         CLI   4(R1),0                                                          
         BE    EIIF                                                             
         LA    R5,BLOCKI                                                        
         MVC   FLDCNT,4(R1)                                                     
         MVI   FNDX,1                                                           
*                                                                               
DATAV12  CLC   FNDX,FLDCNT                                                      
         BH    DATAV14                                                          
         CLI   0(R5),L'SENAME                                                   
         BH    EIIF                                                             
*                                                                               
         GOTO1 VALSE,12(R5)                                                     
         BZ    EIIF                                                             
         USING SELISTD,R1                                                       
         ZIC   RE,SEOVSYS                                                       
         LA    RE,OVSYS(RE)                                                     
         CLI   0(RE),0             ENSURE SYSTEM NOT DUPLICATE                  
         MVC   0(1,RE),SEOVSYS                                                  
         BNE   EDIF                                                             
         MVC   SYSNUM,SEOVSYS                                                   
         MVC   SYSSE,SESYS                                                      
         DROP  R1                                                               
         LA    R6,TEMP                                                          
         XC    TEMP,TEMP                                                        
         USING CTSYSD,R6                                                        
         MVI   CTSYSEL,X'21'                                                    
         MVI   CTSYSLEN,CTSYSPGM-CTSYSD                                         
         MVC   CTSYSNUM,SYSNUM                                                  
         CLI   ACTN,ADD                                                         
         BE    DATAV12E                                                         
         GOTO1 VHELLO,DMCB,(C'G',=C'CTFILE  '),(X'FF',(R4)),(1,SYSNUM)          
         CLI   12(R1),X'06'                                                     
         BE    DATAV12E                                                         
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R1,12(R1)                                                        
         LA    R1,0(R1)                                                         
         ZIC   RF,1(R1)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R1)                                                    
         MVI   CTSYSEL,X'21'                                                    
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE  '),(X'FF',(R4)),(1,SYSNUM)          
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
DATAV12E MVC   CTSYSSE,SYSSE                                                    
         MVI   CTSYSIND,CTSYSINF   NEW FORMAT                                   
*                                                                               
         CLI   CTSYSNUM,2          SPOT SYSTEM?                                 
         BNE   *+16                                                             
         CLI   SYSCABL,C'Y'        YES, USES AMS CABLE DATA?                    
         BNE   *+8                                                              
         OI    CTSYSIND,CTSYSNCA        YES, FLAG IT IN RECORD                  
*                                                                               
         XC    CTSYSAGB,CTSYSAGB                                                
*&&UK                                                                           
         CLI   CTSYSNUM,MEDSEQ     MEDIA SYSTEM                                 
         BNE   *+14                                                             
         MVC   MEDSE,CTSYSSE       SAVE SYSTEM SE NUMBER                        
         B     DATAV12D                                                         
         CLI   CTSYSNUM,ACCSEQ     ACCOUNTING SYSTEM                            
         BNE   DATAV12D                                                         
         MVC   ACCSE,CTSYSSE       SAVE SYSTEM SE NUMBER                        
*&&                                                                             
*                                                                               
DATAV12D EQU   *                                                                
         GOTO1 GETSYS,DMCB,CTSYSNUM                                             
         ICM   R1,15,0(R1)         CHECK IF BINARY ID REQUIRED                  
         BNZ   *+6                                                              
         DC    H'00'                                                            
         USING SYSLSTD,R1                                                       
         CLI   SYSLIND1,X'40'                                                   
         BNE   DATAV12B                                                         
         DROP  R1                                                               
*                                                                               
         CLI   1(R5),2                                                          
         BE    DATAV12A                                                         
         B     EIIF                                                             
*                                                                               
DATAV12B CLI   1(R5),0                                                          
         BE    DATAV12C                                                         
         B     EIIF                                                             
*                                                                               
DATAV12A GOTO1 VHEXIN,DMCB,22(R5),CTSYSAGB,2                                    
         OC    12(4,R1),12(R1)                                                  
         BZ    EIIF                                                             
*&&US                                                                           
         CLI   CTSYSNUM,X'07'      AVOID US TALENT SYSTEM                       
         BE    DATAV12C                                                         
         MVC   WORK(1),CTSYSSE                                                  
         MVC   WORK+1(1),CTSYSAGB                                               
         GOTO1 CHKAGY                                                           
         CLI   WORK,0                                                           
         BNE   EIIF                                                             
*&&                                                                             
DATAV12C GOTO1 APUTEL                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         ZIC   R1,FNDX             BUMP TO NEXT SCANNER FIELD                   
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R5,L'BLOCKI(R5)                                                  
         B     DATAV12                                                          
*                                                                               
DATAV14  BCT   R8,DATAV8           DO FOR NUMBER OF SYSTEM FIELDS               
         LA    R1,SYSACCSH                                                      
         ST    R1,FADR                                                          
         OC    OVSYS,OVSYS         ENSURE A SYSTEM WAS SPECIFIED                
         BZ    EMIF                                                             
         EJECT                                                                  
DATAVA   EQU   *                                                                
*                                  DELETE REMAINING SYSTEM ELEMENTS             
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE  '),(X'FF',CT5REC),0,0               
         CLI   12(R1),X'00'                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
*&&UK                                                                           
*                                  READ ASSOCIATED AGENCIES LIST                
         GOTO1 AFVAL,SYSASAGH                                                   
         BZ    DATAVB                                                           
         MVI   FNDX,0              INITIALISE INDEX TO SCAN FIELD               
         GOTO1 VSCANNER,DMCB,FLDH,(20,BLOCKI)                                   
         CLI   4(R1),0                                                          
         BE    ETMI                                                             
         LA    R6,BLOCKI                                                        
         CLI   4(R1),CTAGLMAX                                                   
         BH    ETMI                                                             
         MVC   FLDCNT,4(R1)                                                     
         LA    R7,TEMP                                                          
         USING CTAGLD,R7                                                        
         MVI   CTAGLEL,CTAGLELQ                                                 
         LA    R7,CTAGLAID                                                      
         MVI   FNDX,1                                                           
*                                                                               
DATAVA1  CLC   FNDX,FLDCNT         PROCESS EACH FIELD IN SCAN LIST              
         BH    DATAVA7                                                          
         CLI   1(R6),0             VALID SINGLE FIELD                           
         BNE   EIIF                                                             
         CLI   0(R6),L'CTAGLAID    VALID FIELD LENGTH                           
         BL    EFTS                                                             
         BH    EFTL                                                             
*                                                                               
         LR    R0,R7               TEST ID NOT ALREADY INPUT                    
         LA    R7,TEMP                                                          
         LA    R7,2(R7)                                                         
DATAVA8  CR    R7,R0                                                            
         BNL   DATAVA9                                                          
         CLC   0(L'CTAGLAID,R7),12(R6)                                          
         BE    EDIF                                                             
         LA    R7,L'CTAGLAID(R7)                                                
         B     DATAVA8                                                          
DATAVA9  CLC   12(L'CTAGLAID,R6),CT5KALPH                                       
         BE    DATAVAB             ASSOCIATED WITH ITSELF                       
         LA    R4,IOAREA2          GET SYSTEM ACCESS RECORD FOR ID              
         ST    R4,AREC                                                          
         MVC   CT5KEY,KEY                                                       
         MVC   CT5KALPH,12(R6)                                                  
         MVC   KEY,CT5KEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         MVC   KEY,KEYNEXT                                                      
         TM    DMCB+8,X'10'        TEST NOT FOUND                               
         BNZ   ERNF                                                             
         TM    DMCB+8,X'02'        TEST DELETED                                 
         BNZ   ERID                                                             
         MVI   ACCFLAG,X'00'       SET SYSTEM TYPE FLAGS                        
         MVI   MEDFLAG,X'00'                                                    
         LA    R5,CT5DATA                                                       
         EJECT                                                                  
*                                  VALIDATE ID                                  
DATAVA3  CLI   0(R5),0             E-O-R                                        
         BE    DATAVA6                                                          
         CLI   0(R5),CTAGLELQ      INVALID IF ASSOC LIST PRESENT                
         BE    EISS                ?                                            
         CLI   0(R5),CTSYSELQ      SYSTEM AUTHORIZ ELEMENT                      
         BNE   DATAVA5                                                          
         USING CTSYSD,R5                                                        
         CLI   CTSYSNUM,X'06'      VALID IF SAME ACCOUNT SYSTEM                 
         BNE   DATAVA4                                                          
*        CLC   CTSYSSE,ACCSE       THIS TEST IGNORED NOW                        
*        BNE   EISS                                                             
         MVC   ACCFLAG,ACCSE       FLAG FOUND OK                                
         B     DATAVA5                                                          
*                                                                               
DATAVA4  CLI   CTSYSNUM,X'04'      VALID IF SAME MEDIA SYSTEM                   
         BNE   DATAVA5                                                          
*        CLC   CTSYSSE,MEDSE       THIS TEST IGNORED ALSO                       
*        BNE   EISS                                                             
         MVC   MEDFLAG,MEDSE       FLAG FOUND OK                                
*                                                                               
DATAVA5  ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     DATAVA3                                                          
         DROP  R5                                                               
*                                                                               
DATAVA6  CLC   ACCFLAG,ACCSE                                                    
*        BNE   EISS                THIS TEST IGNORED NOW                        
         CLC   MEDFLAG,MEDSE                                                    
*        BNE   EISS                THIS TEST IGNORED ALSO                       
DATAVAB  LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         MVC   0(L'CTAGLAID,R7),12(R6)                                          
         LA    R7,L'CTAGLAID(R7)                                                
         LA    R6,L'BLOCKI(R6)                                                  
         ZIC   RF,FNDX             BUMP TO NEXT SCANNER FIELD                   
         LA    RF,1(RF)                                                         
         STC   RF,FNDX                                                          
         B     DATAVA1                                                          
*                                                                               
DATAVA7  LA    R7,TEMP                                                          
         ZIC   RF,FLDCNT                                                        
         SLL   RF,1                                                             
         LA    RF,2(RF)                                                         
         STC   RF,CTAGLLEN                                                      
         MVI   FNDX,0                                                           
         GOTO1 APUTEL                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  R7                                                               
*&&                                                                             
         EJECT                                                                  
DATAVB   EQU   *                   VALIDATE SECURITY AGENCY ALPHA ID            
         GOTO1 AFVAL,SYSSECAH                                                   
         BZ    DATAVBX                                                          
         CLI   FLDH+5,2                                                         
         BL    EFTS                                                             
         CLC   CT5KALPH,FLD                                                     
         BE    EIIF                                                             
         LA    R4,IOAREA2          GET SYSTEM ACCESS RECORD FOR ID              
         ST    R4,AREC                                                          
         MVC   CT5KEY,KEY                                                       
         MVC   CT5KALPH,FLD                                                     
         MVC   KEY,CT5KEY                                                       
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         MVC   KEY,KEYNEXT                                                      
         TM    DMCB+8,X'10'        TEST NOT FOUND                               
         BNZ   ERNF                                                             
         TM    DMCB+8,X'02'        TEST DELETED                                 
         BNZ   ERID                                                             
         LA    R5,CT5DATA                                                       
*                                  VALIDATE ID                                  
DATAVB1  CLI   0(R5),0             E-O-R                                        
         BE    DATAVB2                                                          
         CLI   0(R5),CTSEAELQ      INVALID IF SECURITY AGENCY PRESENT           
         BE    EIIF                                                             
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     DATAVB1                                                          
*                                                                               
DATAVB2  LA    R4,IOAREA                                                        
         ST    R4,AREC                                                          
         XC    TEMP,TEMP           ADD SECURITY AGENCY ID ELEMENT               
         LA    R5,TEMP                                                          
         USING CTSEAD,R5                                                        
         MVI   CTSEAEL,CTSEAELQ                                                 
         MVI   CTSEALEN,CTSEALNQ                                                
         MVC   CTSEAAID,FLD                                                     
         GOTO1 APUTEL                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
DATAVBX  B     DATAVC                                                           
         EJECT                                                                  
*                                                                               
DATAVC   GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         LA    R1,SYSALPHH         POSN TO 1ST KEY FLD & SET OK                 
         ST    R1,FADR                                                          
         MVI   FNDX,0                                                           
         MVI   FERN,X'FF'                                                       
         L     RF,AWRITE           WRITE OR ADD ACCESS RECORD                   
         CLI   ACTN,CHANGE                                                      
         BE    *+8                                                              
         L     RF,AADD                                                          
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   NACTN,OKDEL+OKCHA   SET NEXT ACTION & EXIT                       
         MVI   FERN,X'FF'                                                       
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* LOCATE SELIST ENTRY FOR SYSTEM NUMBER POINTED TO BY R1                        
*                                                                               
GETSE    LR    R0,RE                                                            
         MVC   WORK(1),0(R1)                                                    
         L     R1,AFACLIST                                                      
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         CLC   SESYS-SELISTD(L'SESYS,R1),WORK                                   
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
GETSEX   LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* LOCATE SELIST ENTRY FOR SYSTEM NAME                                           
*                                                                               
VALSE    LR    R0,RE                                                            
         MVC   WORK(L'SENAME),0(R1)                                             
         L     R1,AFACLIST                                                      
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         MVI   FERN,X'FF'                                                       
         CLC   SENAME-SELISTD(L'SENAME,R1),WORK                                 
         BE    *+10                                                             
         BXLE  R1,RE,*-10                                                       
         SR    R1,R1                                                            
VALSEX   LR    RE,R0                                                            
         LTR   R1,R1                                                            
         BR    RE                                                               
         SPACE 2                                                                
* LOCATE SYSTEM LIST ENTRY FOR SYSTEM NUMBER POINTED TO BY R1                   
*                                                                               
GETSYS   LR    R0,RE                                                            
         SR    RF,RF                                                            
         L     RF,0(R1)                                                         
         ICM   RF,1,0(RF)          RF=SYSTEM NUMBER                             
         L     RE,ASYSTBL          SYSLST ADDRESS                               
         USING SYSLSTD,RE                                                       
*                                                                               
GSYSL1   CLI   SYSLNUM,0           END LIST                                     
         BE    GSYSNF                                                           
         CLM   RF,1,SYSLNUM        MATCH NUMBER                                 
         BE    GSYSX                                                            
         LA    RE,SYSLLEN(RE)      BUMP NEXT                                    
         B     GSYSL1                                                           
*                                                                               
GSYSNF   SR    RE,RE                                                            
*                                                                               
GSYSX    ST    RE,0(R1)                                                         
         LTR   RE,RE                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
* GET ID INFO FROM ID RECORDS                                                   
* R1 POINTS TO ID NUMBER                                                        
*                                                                               
GETID    NTR1  ,                                                                
         XC    USERVAL(USERVALL),USERVAL                                        
         MVC   USERNO,0(R1)                                                     
         MVC   USERID(3),=C'ID='                                                
         SR    R0,R0                                                            
         ICM   R0,3,USERNO                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  USERID+3(5),DUB                                                  
         MVI   USERNM,C'?'                                                      
         MVC   USERNM+1(L'USERNM-1),USERNM                                      
         LA    R6,IOAREA2          DISPLAY PRINCIPAL ID                         
         USING CTIREC,R6                                                        
         ST    R6,AREC                                                          
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USERNO                                               
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD                                                            
         ST    R4,AREC                                                          
         MVC   KEY,KEYNEXT                                                      
         TM    DMCB+8,X'FF'-X'92'                                               
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   DMCB+8,0                                                         
         BNE   GETIDX                                                           
*                                                                               
         LA    R1,CTIDATA                                                       
         SR    R0,R0                                                            
GETID2   CLI   0(R1),0                                                          
         BE    GETIDX                                                           
         CLI   0(R1),CTDSCELQ                                                   
         BNE   *+14                                                             
         MVC   USERID,CTDSC-CTDSCD(R1)                                          
         B     GETID4                                                           
         CLI   0(R1),CTAGYELQ                                                   
         BNE   *+14                                                             
         MVC   USERAL,CTAGYID-CTAGYD(R1)                                        
         B     GETID4                                                           
         CLI   0(R1),CTDSTELQ                                                   
         BNE   *+14                                                             
         MVC   USERNM,CTDSTNAM-CTDSTD(R1)                                       
         B     GETID4                                                           
*                                                                               
GETID4   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GETID2                                                           
*                                                                               
GETIDX   B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE ID INFO FROM ID RECORDS                                              
* ID IN TWA FIELD                                                               
*                                                                               
VALID    NTR1  ,                                                                
         XC    USERVAL(USERVALL),USERVAL                                        
         MVC   USERID,FLD                                                       
         CLI   FLDH+5,3                                                         
         BL    EFTS                                                             
         LA    R6,IOAREA2                                                       
         ST    R6,AREC                                                          
         USING CTIREC,R6                                                        
         XC    CTIKEY,CTIKEY       BUILD KEY OF ID RECORD                       
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,USERID                                                    
         MVC   KEY,CTIKEY                                                       
         GOTO1 AREAD               READ ID RECORD                               
         BZ    EIIO                                                             
         CLI   DMCB+8,0                                                         
         BNE   ERNF                CHECK RECORD FOUND/OK                        
         ST    R4,AREC                                                          
         MVC   KEY,KEYNEXT                                                      
*                                                                               
         LA    R1,CTIDATA                                                       
         SR    R0,R0                                                            
VALID2   CLI   0(R1),0                                                          
         BE    VALIDX                                                           
         CLI   0(R1),CTAGYELQ      TEST AGENCY ALPHA ID ELEMENT                 
         BNE   *+14                                                             
         MVC   USERAL,CTAGYID-CTAGYD(R1)                                        
         B     VALID4                                                           
         CLI   0(R1),CTDSCELQ      TEST AGENCY ID NUMBER                        
         BNE   *+14                                                             
         MVC   USERNO,CTDSC-CTDSCD(R1)                                          
         B     VALID4                                                           
         CLI   0(R1),CTDSTELQ                                                   
         BNE   *+14                                                             
         MVC   USERNM,CTDSTNAM-CTDSTD(R1)                                       
         B     VALID4                                                           
*                                                                               
VALID4   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VALID2                                                           
*                                                                               
VALIDX   MVI   FERN,X'FF'          SET ID VALID                                 
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* CHECK AGENCY BINARY NUMBER IS UNIQUE FOR THE SPECIFIED SYSTEM                 
* CALLED ON UPDATE OF RECORD SYSTEM ELEMENTS                                    
* WORK+0(1)=CTSYSSE SYSTEM NUMBER                                               
* WORK+1(1)=CTSYSAGB AGENCY BINARY CODE                                         
*                                                                               
CHKAGY   NTR1  ,                                                                
         CLI   WORK+1,0                                                         
         BE    CHAGOKX             IGNORE NULL AGENCY CODE                      
         LA    R6,IOAREA2                                                       
         ST    R6,AREC                                                          
         USING CT5REC,R6                                                        
         XC    CT5KEY,CT5KEY       BUILD KEY OF ACCESS RECORD                   
         MVI   CT5KTYP,C'5'                                                     
         MVC   KEY,CT5KEY                                                       
         GOTO1 AREADHI             READ FIRST RECORD                            
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   DMCB+8,0                                                         
         BNE   CHAGOKX             CHECK EOF                                    
         B     CHAGL2                                                           
*                                                                               
CHAGL1   MVC   KEY,CT5KEY                                                       
         CLI   KEY,C'5'                                                         
         BNE   CHAGOKX             CHECK EOF                                    
         GOTO1 ARSEQ               READ NEXT RECORD                             
         BNE   *+6                                                              
         DC    H'00'                                                            
         TM    DMCB+8,X'80'                                                     
         BNZ   CHAGOKX             CHECK EOF                                    
         B     CHAGL2                                                           
*                                                                               
CHAGL2   CLC   CT5KEY,KEYNEXT                                                   
         BE    CHAGL1              AVOID CURRENT ACCESS RECORD                  
         LA    R1,CT5DATA          COMPARE DATA IN SYSTEM ELEMENTS              
         SR    R0,R0                                                            
CHAGL2A  CLI   0(R1),0             END OF RECORD                                
         BE    CHAGL2X                                                          
         CLI   0(R1),CTSYSELQ      FIND SYSTEM ELEMENTS                         
         BNE   CHAGL2B                                                          
         USING CTSYSD,R1                                                        
         CLC   WORK(1),CTSYSSE     CHECK SAME SYSTEM #                          
         BNE   CHAGL2B                                                          
         CLC   WORK+1(1),CTSYSAGB  COMPARE BINARY CODE                          
         BE    CHAGERRX            EXIT TO FLAG CODE EXISTS                     
         DROP  R1                                                               
         B     CHAGL2B                                                          
*                                                                               
CHAGL2B  IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     CHAGL2A                                                          
*                                                                               
CHAGL2X  B     CHAGL1                                                           
*                                                                               
CHAGERRX MVI   WORK,1              FLAG CODE FOUND                              
         B     CHAGX                                                            
*                                                                               
CHAGOKX  MVI   WORK,0              SET CC TO FLAG 'NZ' CODE NOT FOUND           
         B     CHAGX                                                            
*                                                                               
CHAGX    ST    R4,AREC                                                          
         MVC   KEY,KEYNEXT                                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
MEDSEQ   EQU   X'04'               MEDIA SYTEM CODE NUMBER                      
ACCSEQ   EQU   X'06'               ACCOUNTING SYSTEM CODE NUMBER                
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL W/S **                              
RELO     DS    A                                                                
VSCINKEY DS    V                                                                
CTRY     DS    XL1                                                              
CURR     DS    CL3                                                              
HOLDID   DS    XL2                                                              
GRUPID   DS    XL2                                                              
CLTYPE   DS    CL1                                                              
DDSACC   DS    XL1                 DDS ACCESS LEVEL FLAG                        
SYSCNT   DS    X                                                                
FLDCNT   DS    X                                                                
USERVAL  DS    0C                  USER ID VALUES                               
USERID   DS    CL10                USER-ID CODE                                 
USERNM   DS    CL33                USER-ID NAME                                 
USERNO   DS    CL2                 USER-ID NUMBER                               
USERAL   DS    CL2                 USER-ID ALPHA-ID                             
USERVALL EQU   *-USERVAL                                                        
SYSNUM   DS    XL1                                                              
SYSSE    DS    XL1                                                              
ACCSE    DS    CL1                                                              
MEDSE    DS    CL1                                                              
ACCFLAG  DS    CL1                                                              
MEDFLAG  DS    CL1                                                              
*                                                                               
OVSYS    DS    XL256                                                            
BLOCKI   DS    20CL32              SCAN BLOCK                                   
         ORG   BLOCKI                                                           
BLOCKO   DS    20CL20              UNSCAN BLOCK                                 
         ORG                                                                    
*                                                                               
IOAREA2  DS    1024C                                                            
WORKX    EQU   *                                                                
         SPACE 1                                                                
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
         SPACE 1                                                                
* CTLFMACTNS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMDFD                                                                      
       ++INCLUDE CTLFMDFD                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
       SPACE   1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020CTLFM20   05/01/02'                                      
         END                                                                    
