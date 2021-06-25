*          DATA SET CTLFM0C    AT LEVEL 003 AS OF 08/22/00                      
*PHASE TA020CA                                                                  
*INCLUDE EXPRESS                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'CTLFM0C - CONTROL FILE MAINT - PROGRAM PROFILES'                
CTLFM0C  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**LFMC**                                               
         USING WRKD,RC             RC=A(TEMP W/S)                               
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R2,ATWA                                                          
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         LR    R3,R2                                                            
         USING LFMSAVE,R3          R3=A(TWA SAVE W/S)                           
         L     R4,AREC                                                          
         USING CTUREC,R4           R4=A(IO)                                     
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
*                                                                               
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKEY,C'U'                                                      
         LA    R1,FLDSYSH          VALIDATE SYSTEM                              
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
*                                                                               
         LA    R5,DQUPROF          SEE IF IT'S A SPECIAL FOR $DQU               
KEYV0    CLI   0(R5),X'FF'                                                      
         BE    KEYV1               NO MATCH -- CHECK SYSLST                     
         CLC   0(2,R5),FLD                                                      
         BE    *+12                                                             
         LA    R5,3(R5)                                                         
         B     KEYV0                                                            
         MVC   CTUKSYS,2(R5)       SAVE SPECIAL SYSTEM CODE                     
         B     KEYV6                                                            
*                                                                               
KEYV1    ZIC   R6,FLDH+5                                                        
         BCTR  R6,0                                                             
         L     R5,ASYSTBL                                                       
         USING SYSLSTD,R5          R5=A(SYSTEMS TABLE)                          
*                                                                               
KEYV2    CLI   SYSLNUM,0           END OF LIST                                  
         BE    EIIF                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),SYSLNAME     COMPARE INPUT WITH TABLE                     
         BE    KEYV4                                                            
         LA    R5,SYSLLEN(R5)                                                   
         B     KEYV2                                                            
*                                                                               
KEYV4    XC    FLDSYS,FLDSYS       DISPLAY FULL SYSTEM NAME                     
         MVC   FLDSYS(L'SYSLNAME),SYSLNAME                                      
         OI    FLDSYSH+6,X'80'                                                  
         CLI   SYSLUSLT,C' '       MUST HAVE A KEY LETTER                       
         BE    EIIF                                                             
         MVC   CTUKSYS,SYSLUSLT    MOVE SYSTEM TO KEY (LETTER)                  
         DROP  R5                                                               
*                                                                               
KEYV6    LA    R1,FLDPRGH          VALIDATE PROGRAM                             
         GOTO1 AFVAL                                                            
         BZ    EXIT                                                             
         CLI   FLDH+5,2            2 CHARS MEANS OFFLINE PROG NUM               
         BL    EFTS                                                             
         MVC   CTUKPROG+1(2),FLD   MOVE PRG TO KEY                              
         BE    KEYV8                                                            
         MVC   CTUKPROG,FLD        MOVE PRG TO KEY                              
* NOTE - PAGE# VALIDATION GOES HERE                                             
*                                                                               
KEYV8    DS    0H                                                               
*                                                                               
KEYVA    MVC   KEY,CTUKEY          VALIDATE ACTION                              
         MVC   KEYNEXT,KEY                                                      
         CLI   ACTN,CHANGE                                                      
         BNE   KEYVC                                                            
         CLC   KEY,LKEY                                                         
         BE    KEYVC                                                            
         MVI   ACTN,DISPLAY        SET ACTN TO DISP IF KEYS CHANGED             
*                                                                               
KEYVC    CLI   ACTN,COPY                                                        
         BNE   *+8                                                              
         MVI   ACTN,ADD            COPY TREATED AS AN ADD                       
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST FOR N/F                                 
         BZ    *+16                                                             
         CLI   ACTN,ADD            N/F ONLY VALID FOR ADD                       
         BE    DATAVAL                                                          
         B     ERNF                                                             
         CLI   ACTN,ADD            F NOT VALID FOR ADD                          
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        DELETED RECORD CAN ONLY BE DISPLYD           
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY                                                     
         BNE   EIAS                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DISPREC  LA    R1,FLDDSCH          ERASE UNPROTS                                
         GOTO1 ACLEAR                                                           
         LA    R5,CTUDATA                                                       
*                                                                               
DISP2    CLI   0(R5),0                                                          
         BE    DISPEND                                                          
         CLI   0(R5),X'02'         DESCRIPTION                                  
         BE    DISPDSC                                                          
         CLI   0(R5),X'70'         FIELD DEFINITION                             
         BE    DISPFLD                                                          
*                                                                               
DISP4    SR    R6,R6               BUMP TO NEXT ELEMENT                         
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     DISP2                                                            
*                                                                               
DISPEND  TM    CTUSTAT,X'80'       SET NEXT ACTIONS                             
         BO    *+12                                                             
         MVI   NACTN,OKDEL+OKCHA+OKCOPY                                         
         B     *+8                                                              
         MVI   NACTN,OKRES                                                      
         LA    R1,FLDDSCH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'          ENSURE SAFE RETURN                           
         MVI   FNDX,0                                                           
         B     EXIT                                                             
         SPACE 1                                                                
* DISPLAY DESCRIPTION ELEMENT                                                   
*                                                                               
DISPDSC  SR    R6,R6                                                            
         IC    R6,1(R5)                                                         
         SH    R6,=H'3'            R6=L'DESC-1                                  
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   FLDDSC(0),2(R5)                                                  
         LA    R6,1(R6)                                                         
         STC   R6,FLDDSCH+7        SET LENGTH                                   
         OI    FLDDSCH+6,X'80'     AND TRANSMIT                                 
         B     DISP4                                                            
         EJECT                                                                  
* DISPLAY FIELD DEFINITION ELEMENTS                                             
*                                                                               
DISPFLD  DS    0H                                                               
         USING CTFDD,R5                                                         
         SR    R6,R6                                                            
         IC    R6,CTFDNUM          PICK-UP FIELD#                               
*                                  POINT R6 TO SCREEN LINE                      
         BCTR  R6,0                                                             
         LA    R7,LINLEN                                                        
         STH   R7,HALF                                                          
         MH    R6,HALF                                                          
         LA    R6,FLDFNUMH(R6)                                                  
         USING LINED,R6                                                         
*                                  FIELD TYPE                                   
         MVC   LINTYP(1),CTFDTYPE                                               
         MVI   LINTYP+1,0                                                       
         TM    CTFDOTHR,X'80'      DDS FLAG                                     
         BZ    *+8                                                              
         MVI   LINTYP+1,C'*'                                                    
         MVI   LINTYPH+7,2                                                      
         OI    LINTYPH+6,X'80'                                                  
*                                  FIELD VALUES                                 
         MVC   LINVAL,CTFDLIST                                                  
         MVI   LINVALH+7,20                                                     
         OI    LINVALH+6,X'80'                                                  
*                                  FIELD DESCRIPTION                            
         SR    R7,R7                                                            
         IC    R7,CTFDLEN                                                       
         SH    R7,=H'27'                                                        
         BM    DISPFLD2                                                         
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   LINDSC(0),CTFDDESC                                               
         LA    R7,1(R7)                                                         
         STC   R7,LINDSCH+7                                                     
         OI    LINDSCH+6,X'80'                                                  
*                                  DEFAULT VALUE                                
DISPFLD2 DS    0H                                                               
         SR    R1,R1                                                            
         TM    CTFDOTHR,X'40'      DEFAULT INPUT ?                              
         BO    DISPFLDX                                                         
         CLI   CTFDTYPE,C'C'       CHAR                                         
         BNE   DISPFLD4                                                         
         MVC   LINDFT(1),CTFDDEF                                                
         LA    R1,1                                                             
         B     DISPFLDX                                                         
*                                                                               
DISPFLD4 CLI   CTFDTYPE,C'X'       HEX                                          
         BNE   DISPFLD6                                                         
         GOTO1 VHEXOUT,DMCB,CTFDDEF,LINDFT,1,=C'TOG'                            
         LA    R1,2                                                             
         B     DISPFLDX                                                         
*                                                                               
DISPFLD6 CLI   CTFDTYPE,C'N'       NUM                                          
         BNE   DISPFLDX                                                         
         EDIT  (B1,CTFDDEF),(3,LINDFT),ALIGN=LEFT,ZERO=NOBLANK                  
         OI   LINDFT,X'F0'                                                      
         LR    R1,R0                                                            
         B     DISPFLDX                                                         
*                                                                               
DISPFLDX STC   R1,LINDFTH+7        SET LENGTH                                   
         OI    LINDFTH+6,X'80'     AND TRANSMIT                                 
         B     DISP4                                                            
         DROP  R5                                                               
         DROP  R6                                                               
         EJECT                                                                  
* ADD/CHANGE RECORD                                                             
*                                                                               
DATAVAL  CLI   ACTN,CHANGE                                                      
         BE    DATAV2                                                           
         MVC   ACTN,ACTN2                                                       
         CLI   ACTN,ADD                                                         
         BE    DATAV0                                                           
*                                  COPY FUNCTION                                
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY,LKEY            RESTORE COPY-TO KEY & READ RECORD            
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0            MUST BE OK                                   
         BNE   EIIO                                                             
         MVI   TEMP,X'01'          DELETE AND ADD ACTIVITY ELEMENT              
         GOTO1 ADELEL                                                           
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         MVC   CTUKEY,KEYSAVE      SET COPY-TO KEY                              
         MVC   KEY,CTUKEY                                                       
         B     DATAVW              GO AND ADD RECORD                            
*                                                                               
DATAV0   DS    0H                                                               
         MVI   TEMP,0              IF ADD BUILD KEY+ACTIVITY                    
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         B     DATAV4                                                           
*                                                                               
DATAV2   MVI   TEMP,X'01'          IF CHANGE DELETE ACTIVITY/DESC               
         GOTO1 ADELEL                                                           
         MVI   TEMP,X'02'                                                       
         GOTO1 ADELEL                                                           
         GOTO1 ABLDACT             AND ADD NEW ACTIVITY                         
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  BUILD DESCRIPTION ELEMENT                    
DATAV4   LA    R1,FLDDSCH                                                       
         GOTO1 AFVAL                                                            
         BZ    DATAV6              OPTIONAL INPUT FIELD                         
         SR    R5,R5                                                            
         IC    R5,FLDH+5                                                        
         BCTR  R5,0                                                             
         XC    TEMP,TEMP                                                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   TEMP+2(0),FLD                                                    
         LA    R5,3(R5)                                                         
         STC   R5,TEMP+1           L'ELEMENT                                    
         MVI   TEMP,X'02'          CODE                                         
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                  VALIDATE AND BUILD DEFINITION ELS            
DATAV6   LA    R7,FLDFNUMH         POINT TO FIRST INPUT LINE                    
         USING LINED,R7                                                         
*                                                                               
DATAV8   CLI   0(R7),0             END OF SCREEN ?                              
         BE    DATAVV                                                           
         XC    WRKD(WRKLEN),WRKD                                                
         PACK  DUB,LINNUM          GET SEQUENCE NUMBER                          
         CVB   R1,DUB                                                           
         STC   R1,SEQUENCE                                                      
         MVI   BYTE,0                                                           
         LA    R1,LINDSCH          DESC INPUT ?                                 
         GOTO1 AFVAL                                                            
         BZ    DATAVA                                                           
         OI    BYTE,X'80'          SET DESCRIPTION INPUT                        
         MVC   DESC,FLD            SAVE DESCRIPTION                             
         MVC   DESCLEN,FLDH+5      AND LENGTH                                   
         CLC   DESC(3),=C'++D'     SPECIAL ACTION TO DELETE ELEMENT             
         BNE   DATAVA                                                           
         CLI   ACTN,CHANGE                                                      
         BNE   EIIF                                                             
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE '),(X'70',(R4)),(1,SEQUENCE)         
         B     DATAVT                                                           
*                                                                               
DATAVA   LA    R1,LINTYPH          TYPE                                         
         GOTO1 AFVAL                                                            
         BNZ   DATAVC                                                           
         TM    BYTE,X'80'          IF N/I AND DESC INPUT GIVE MI/F              
         BO    EMIF                                                             
         B     DATAVE                                                           
*                                                                               
DATAVC   CLI   FLDH+5,2                                                         
         BH    EFTL                                                             
         LA    R1,LINDSCH                                                       
         ST    R1,FADR                                                          
         TM    BYTE,X'80'          IF I AND DESC N/I GIVE MI/F AT DESC          
         BZ    EMIF                                                             
         OI    BYTE,X'40'          SET TYPE INPUT                               
         MVC   DTYPE,FLD           AND SAVE IT                                  
*                                                                               
DATAVE   LA    R1,LINVALH          ACCEPTABLE VALUES                            
         GOTO1 AFVAL                                                            
         BNZ   DATAVG                                                           
         TM    BYTE,X'80'          IF N/I AND DESC INPUT GIVE MI/F              
         BO    EMIF                                                             
         B     DATAVI                                                           
*                                                                               
DATAVG   LA    R1,LINDSCH                                                       
         ST    R1,FADR                                                          
         TM    BYTE,X'80'          IF I AND DESC N/I GIVE MI/F AT DESC          
         BZ    EMIF                                                             
         OI    BYTE,X'20'          SET VALUES INPUT                             
         MVC   VALUES,FLD          AND SAVE                                     
*                                                                               
DATAVI   LA    R1,LINDFTH          DEFAULT VALUE                                
         OI    DOTHR,X'40'         SET NO DEFAULT INPUT                         
         GOTO1 AFVAL                                                            
         BZ    DATAVK                                                           
         CLI   FLDH+5,3                                                         
         BH    EFTL                                                             
         LA    R1,LINDSCH                                                       
         ST    R1,FADR                                                          
         TM    BYTE,X'80'          IF I AND DESC N/I GIVE MI/F AT DESC          
         BZ    EMIF                                                             
         OI    BYTE,X'10'          SET DEFAULT INPUT                            
         MVC   DEFAULT,FLD         AND SAVE                                     
         NI    DOTHR,X'BF'         SET DEFAULT INPUT                            
*                                                                               
DATAVK   CLI   BYTE,0              DO NOTHING IF NOTHING INPUT                  
         BE    DATAVT                                                           
*                                  VALIDATE TYPE/VALUES AND OPTIONAL            
*                                  DEFAULT VALUE                                
         SR    R5,R5                                                            
         TM    BYTE,X'10'                                                       
         BZ    *+8                                                              
         LA    R5,DEFAULT                                                       
*                                  PASS INPUT TO EXPRESS                        
         GOTO1 =V(EXPRESS),DMCB,DTYPE,VALUES,(R5),VSCANNER,RR=RB                
         CLI   DMCB,0              OK                                           
         BE    DATAVP                                                           
         LA    R1,LINTYPH          GET A(INVALID FLD HDR)                       
         CLI   DMCB,1                                                           
         BE    DATAVM                                                           
         LA    R1,LINVALH                                                       
         CLI   DMCB,2                                                           
         BE    DATAVM                                                           
         LA    R1,LINDFTH                                                       
*                                                                               
DATAVM   ST    R1,FADR             SET A(FLD IN ERROR)                          
         L     R1,DMCB+16                                                       
         MVC   BASHDR(60),0(R1)    SET RETURNED ERROR MSG IN TWA                
         MVI   FERN,X'FE'          SPECIAL ERROR RETURN                         
         OI    BASHDRH+6,X'80'                                                  
         B     EXIT                                                             
*                                                                               
DATAVP   MVC   DEFAULT,DMCB+4      INPUT IS OK BUILD AN ELEMENT                 
         LA    R1,LINTYPH                                                       
         ST    R1,FADR                                                          
         CLI   LINTYPH+5,1                                                      
         BE    *+16                                                             
         CLI   LINTYP+1,C'*'       CHECK FOR DDS FLAG                           
         BNE   EIIF                                                             
         OI    DOTHR,X'80'                                                      
         CLI   ACTN,CHANGE                                                      
         BNE   DATAVR              DELETE OLD IF CHANGED                        
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE '),(X'70',(R4)),(1,SEQUENCE)         
*                                                                               
DATAVR   XC    TEMP,TEMP                                                        
         LA    R5,TEMP                                                          
         USING CTFDD,R5                                                         
         MVC   CTFDNUM,SEQUENCE                                                 
         MVC   CTFDTYPE,DTYPE                                                   
         MVC   CTFDOTHR,DOTHR                                                   
         MVC   CTFDLIST,VALUES                                                  
         TM    BYTE,X'10'                                                       
         BZ    *+10                                                             
         MVC   CTFDDEF,DEFAULT                                                  
         MVC   CTFDDESC(30),DESC                                                
         SR    R1,R1                                                            
         IC    R1,DESCLEN                                                       
         LA    R1,CTFDDESC-CTFDD(R1)                                            
         STC   R1,CTFDLEN                                                       
         MVI   CTFDEL,X'70'                                                     
         GOTO1 APUTEL              ADD ELEMENT TO REC                           
         BZ    EXIT                                                             
*                                                                               
DATAVT   LA    R7,LINLEN(R7)       BUMP TO NEXT INPUT LINE                      
         B     DATAV8                                                           
*                                                                               
DATAVV   LA    R1,FLDSYSH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         CLI   ACTN,ADD            ADD RECORD                                   
         BNE   DATAVX                                                           
*                                                                               
DATAVW   GOTO1 AADD                                                             
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     DATAVZ                                                           
*                                                                               
DATAVX   GOTO1 AWRITE              CHANGE RECORD                                
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                                                               
DATAVZ   MVI   NACTN,OKDEL+OKCHA+OKCOPY                                         
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
       ++INCLUDE SRDQUPROF                                                      
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
WRKD     DSECT                                                                  
DESCLEN  DS    X                                                                
DESC     DS    CL30                                                             
DTYPE    DS    CL2                                                              
DOTHR    DS    CL1                                                              
VALUES   DS    CL20                                                             
DEFAULT  DS    CL4                                                              
SEQUENCE DS    X                                                                
WRKLEN   EQU   *-WRKD                                                           
HALF     DS    H                                                                
BYTE     DS    C                                                                
WRKX     EQU   *                                                                
         SPACE 1                                                                
* DSECT TO COVER SCREEN LINE                                                    
*                                                                               
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
         EJECT                                                                  
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* CTLFMACTNS                                                                    
       ++INCLUDE CTLFMACTNS                                                     
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFMF3D                                                                      
       ++INCLUDE CTLFMF3D                                                       
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTLFM0C   08/22/00'                                      
         END                                                                    
