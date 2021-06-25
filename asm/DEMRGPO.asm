*          DATA SET DEMRGPO    AT LEVEL 009 AS OF 04/16/08                      
*PHASE DEMRGOA                                                                  
         TITLE 'DEMRGO - DEMO FILE MERGE - OUTPUT PHASE'                        
DEMRGO   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*DEMRGO*                                                       
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
         USING DPRINT,R7           R7=A(PRINTER CSECT)                          
         L     R9,ASREC                                                         
         USING INTERD,R9           R9=A(INTERIM RECORD)                         
*                                                                               
*                                  HONOR READFILE= CONTROL CARD TO MAKE         
*                                   SURE WE READ THE RIGHT BLOODY FILE          
         CLI   RDFLFLAG,C'P'                                                    
         BNE   *+20                                                             
         MVC   FILENAME,=C'PAVFIL '                                             
         MVC   DIRNAME,=C'PAVDIR '                                              
         B     BRTAB                                                            
         CLI   RDFLFLAG,C'D'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'DEMFIL '                                             
         MVC   DIRNAME,=C'DEMDIR '                                              
BRTAB    DS    0H                                                               
*                                                                               
         B     *+4(R1)                                                          
         B     CNV2                PROCESS A RECORD                             
         B     CNVFMRG             LAST TIME HOOK                               
*                                                                               
CNV2     DS    0H                                                               
         CLC   =AL2((INTDATA-INTERD)+23),INTRECLN   PASSIVE KEY?                
         BE    CNV7                YES                                          
         CLI   INTRTYP,PRCODEQU    RATINGS RECORDS                              
         BE    CNV3                 MERGE THEM                                  
         B     CNV7                 OTHERWISE COPY INPUT                        
*                                                                               
*          DATA SET DECNVTON   AT LEVEL 203 AS OF 06/03/04                      
*                                                                               
CNVXH    DS    0H                                                               
         LHI   R0,1                                                             
         J     CNVXCR                                                           
                                                                                
CNVXL    DS    0H                                                               
         LHI   R0,-1                                                            
         J     CNVXCR                                                           
                                                                                
CNVXE    DS    0H                                                               
         SR    R0,R0                                                            
         J     CNVXCR                                                           
                                                                                
CNVXCR   DS    0H                                                               
         AR    R0,RB                                                            
         CR    R0,RB                                                            
         J     CNVX                                                             
         EJECT                                                                  
* LAST TIME HOOK - MERGE TO EOF                                                 
*                                                                               
CNVFMRG  BAS   RE,LSTMINOR                                                      
         B     CNVX                EXIT TO DEMCNV                               
         SPACE 3                                                                
* WRITE ALL REMAINING MINOR KEYS FOR A MAJOR KEY TO OUTPUT TAPE                 
*                                                                               
LSTMINOR NTR1                                                                   
*                                                                               
LSTMIN10 CLI   FILEKEY,X'FF'                                                    
         BE    CNVX                                                             
*                                                                               
         L     RE,ADEMOREC                                                      
         USING PRKEY,RE                                                         
         LA    RE,4(RE)                                                         
         CLI   PRKEY,X'FF'         EOF - JUST END IT                            
         BE    CNVX                                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,PRRLEN         GET THE RECORD LENGTH                        
         LA    R1,4(R1)            AND ADJUST                                   
         L     RF,AOREC                                                         
         XC    0(4,RF),0(RF)                                                    
         STCM  R1,3,0(RF)          SAVE IT IN OUTPUT AREA                       
         SHI   R1,4                                                             
         LA    RF,4(RF)            ADJUST OUTPUT POINTER AND MOVE REC           
         MOVE  ((RF),(R1)),(RE)                                                 
         DROP  RE                                                               
*                                                                               
         BAS   RE,PUTTAPE          VALIDATE RECORD AND SEND TO OUTPUT           
*                                                                               
         MVI   MYDMCB,X'20'        SET FOR READSEQ                              
         GOTOR =A(READDFIL)        READ A RECORD FROM THE FILE                  
         L     RE,ADEMOREC                                                      
         LA    RE,4(RE)                                                         
         MVC   FILEKEY,0(RE)                                                    
         B     LSTMIN10                                                         
*                                                                               
CNVX     XMOD1                                                                  
         EJECT                                                                  
* BUILD DEMOGRAPHIC RECORDS                                                     
*                                                                               
CNV3     LA    R6,THISKEY                                                       
         MVC   THISKEY(PRRLEN-PRKEY),INTKEY                                     
         CLC   MAJKEY,THISKEY                                                   
         BE    CNV4                                                             
         CLI   FILEKEY,0                                                        
         BE    *+8                                                              
         BAS   RE,LSTMINOR                                                      
*                                                                               
         MVC   MAJKEY,THISKEY                                                   
         GOTOR =A(READDIR)                                                      
         MVC   FILEKEY,THISKEY                                                  
         BNL   *+14                                                             
         MVC   FILEKEY,=X'FFFFFFFFFFFFFFFFFFFF'                                 
         B     CNV4                                                             
         XC    DEMOREC(24),DEMOREC                                              
         MVC   DEMOREC+4(L'MAJKEY),MAJKEY                                       
         MVI   MYDMCB,X'10'                                                     
         GOTOR =A(READDFIL)                                                     
         L     RF,ADEMOREC                                                      
         LA    RF,4(RF)                                                         
         MVC   FILEKEY,0(RF)       SET FOR NEXT PASS                            
*                                                                               
CNV4     CLC   THISKEY,FILEKEY     THIS RECORD LESS THAN FILE                   
         BL    CNV7                 INSERT IT                                   
         BH    COPYFILE                                                         
*                                  EQUAL: READ PAST CURRENT FILE RECORD         
         MVI   MYDMCB,X'20'        SET FOR READSEQ                              
         GOTOR =A(READDFIL)                                                     
         L     RF,ADEMOREC                                                      
         LA    RF,4(RF)                                                         
         MVC   FILEKEY,0(RF)       SET FOR NEXT PASS                            
         B     CNV7                                                             
*                                                                               
COPYFILE L     RE,ADEMOREC                                                      
         USING PRKEY,RE                                                         
         LA    RE,4(RE)                                                         
         CLI   PRKEY,X'FF'         EOF - JUST END IT                            
         BE    COPYFIL2                                                         
         SR    R1,R1                                                            
         ICM   R1,3,PRRLEN         GET THE RECORD LENGTH                        
         LA    R1,4(R1)            AND ADJUST                                   
         L     RF,AOREC                                                         
         XC    0(4,RF),0(RF)                                                    
         STCM  R1,3,0(RF)          SAVE IT IN OUTPUT AREA                       
         SHI   R1,4                                                             
         LA    RF,4(RF)            ADJUST OUTPUT POINTER AND MOVE REC           
         MOVE  ((RF),(R1)),(RE)                                                 
         DROP  RE                                                               
*                                                                               
         BAS   RE,PUTTAPE          VALIDATE RECORD AND SEND TO OUTPUT           
*                                                                               
         MVI   MYDMCB,X'20'        SET FOR READSEQ                              
         GOTOR =A(READDFIL)        READ A RECORD FROM THE FILE                  
*                                                                               
COPYFIL2 L     RE,ADEMOREC                                                      
         LA    RE,4(RE)                                                         
         MVC   FILEKEY,0(RE)                                                    
         B     CNV3                                                             
*                                                                               
CNV7     XC    THISKEY,THISKEY                                                  
         L     R5,AOREC                                                         
         XC    0(4,R5),0(R5)                                                    
         ICM   R1,3,INTRECLN       SREC'S TOTAL LENGTH                          
         SHI   R1,50               MINUS 50 BYTES SORT DATA                     
         STCM  R1,3,0(R5)                                                       
         LA    R5,4(R5)            POINT PAST LENGTH IN OUTPUT                  
         MOVE  ((R5),(R1)),INTDATA                                              
*                                  PUT RECORD TO TAPE & SAVE VALUES             
         BAS   RE,PUTTAPE          VALIDATE RECORD AND SEND TO OUTPUT           
         MVC   PREVKEY,THISKEY                                                  
         B     CNVX                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD INTEGRITY AND PUT TO OUTPUT TAPE                              
*                                                                               
PUTTAPE  NTR1                                                                   
         L     R1,AOREC            ENFORCE EOR INDICATOR                        
         LA    R2,4(R1)                                                         
         USING PRKEY,R2                                                         
         CLC   =AL2(23+4),0(R1)    DON'T VALIDATE PASSIVE KEYS                  
         BE    PUTTAPEX            JUST PUT THEM TO TAPE                        
         LH    R4,PRRLEN                                                        
         AR    R4,R2                                                            
         SHI   R4,1                                                             
         MVI   0(R4),X'00'                                                      
         LA    R4,1(R4)                                                         
         SR    R4,R2                                                            
         AHI   R4,4                                                             
         STCM  R4,3,0(R1)                                                       
*                                                                               
         LA    R3,PRFRSTEL         CHECK RECORD INTEGRITY                       
PUTTAP10 CLI   0(R3),0                                                          
         BE    PUTTAP20                                                         
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     PUTTAP10                                                         
*                                                                               
PUTTAP20 SR    R3,R2                                                            
         AHI   R3,1                                                             
         CH    R3,PRRLEN                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PUTTAPEX GOTO1 APUTTAPE                                                         
         B     CNVX                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*  THIS ROUTINE READS THE DEMO DIRECTORY FOR THE MAJOR KEY                      
* ENTRY = THISKEY =DRKEY                                                        
* EXIT  = DEMKEY SET TO KEY OF DEMDIR FOUND                                     
*       CC=EQUAL IF KEY FOUND                                                   
*       CC=LOW   IF NO MATCHING KEY FOUND                                       
***********************************************************************         
*  READ FOR MAJOR KEY                                                           
READDIR  NTR1                                                                   
         LA    R6,THISKEY                                                       
* BUILD SEARCH DEMDIR KEY                                                       
         XC    DEMKEYSV,DEMKEYSV                                                
         XC    DEMKEY,DEMKEY                                                    
         MVC   DEMKEY,MAJKEY                                                    
         MVC   DEMKEYSV,DEMKEY                                                  
                                                                                
         L     RE,AOREC                                                         
         AHI   RE,4                                                             
         ST    RE,ADEMOREC                                                      
                                                                                
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',DIRNAME,DEMKEY,ADEMOREC                 
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RE,ADEMOREC                                                      
         MVC   SVDA,PRNDXDA-PRKMAJOR(RE)                                        
         MVC   SVSTATUS,PRKSTAT-PRKMAJOR(RE)                                    
                                                                                
         CLC   0(L'PRKMAJOR,RE),DEMKEY                                          
         BNE   READDIRN            MAJOR KEY NOT IN DIRECTORY                   
                                                                                
READDIRY J     CNVXE                                                            
READDIRN J     CNVXL                                                            
         EJECT                                                                  
***********************************************************************         
*  THIS ROUTINE READS THE DEMO FILE  FOR THE MINOR KEYS                         
* ENTRY = DEMKEY IS MAJOR KEY                                                   
*         DMCB(0)=X'10' = READHI                                                
*         DMCB(0)=X'20' = READSEQ                                               
*                                                                               
* EXIT  = ADEMOREC=ADDRESS OF DEMO RECORD                                       
*       DMKYIXDA=DISK ADDRESS OF RECORD - UPDATED BY READS                      
*       CC=EQUAL IF REC FOUND                                                   
*       CC=LOW   IF NO MATCHING REC FOUND OR ERROR FROM READ                    
*       CC=HIGH  END OF FILE CONDITION                                          
***********************************************************************         
READDFIL NTR1                                                                   
* GET DEMO RECORD AND READ FOR MINOR KEY                                        
         LA    RF,DEMOREC          ADEMOREC ALWAYS SET TO POINT TO              
         AHI   RF,4                 RECORD (BEYOND THE 4-BYTE RDW)              
         ST    RF,ADEMOREC                                                      
         MVC   PRRSTAT-PRKEY(,RF),SVSTATUS                                      
*                                                                               
         XC    DMCB,DMCB                                                        
         XC    MYDMCB+4(4),MYDMCB+4                                             
         CLI   MYDMCB,X'10'                                                     
         JE    READDF10                                                         
         CLI   MYDMCB,X'20'                                                     
         JE    READDF20                                                         
         DC    H'0'               IF NOT SET-SOMETHING IS AMISS                 
*                                                                               
READDF10 GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),FILENAME,SVDA,ADEMOREC              
         J     READDF30                                                         
READDF20 GOTO1 VDATAMGR,DMCB,(0,=C'DMRSEQ'),FILENAME,SVDA,ADEMOREC              
*                                                                               
READDF30 CLI   DMCB+8,0            WAS THE READ SUCCESSFUL?                     
         BE    READDF40            YEP                                          
         CLI   DMCB+8,X'80'        DID WE REACH EOF?                            
         BE    READFILH            YEP                                          
         B     READFILL                                                         
READDF40 L     RE,ADEMOREC                                                      
         LR    RF,RE                                                            
         USING PRKEY,RE                                                         
         SHI   RF,4                        SET RECORD 4-BYTE RDW                
         SR    R0,R0                                                            
         ICM   R0,3,PRRLEN                                                      
         AHI   R0,4                                                             
         XC    0(4,RF),0(RF)                                                    
         STCM  R0,3,0(RF)                                                       
         ST    RF,ADEMOREC                 POINT TO 4 RDW                       
         CLC   DEMKEY(L'PRKMAJOR),0(RE)    SAME MAJOR KEY?                      
         BNE   READFILL                    MAJOR KEY NOT FOUND                  
         B     READFILE                    RECORD FOUND                         
         DROP  RE                                                               
*                                                                               
READFILE J     CNVXE                                                            
READFILL J     CNVXL                                                            
READFILH J     CNVXH                                                            
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                  THIS TIME VALUES                             
ADEMOREC DS    A                   A(RECORD FROM DEMO FILE)                     
MYDMCB   DS    6F                                                               
SVDA     DS    AL4                                                              
SVSTATUS DS    X                                                                
*                                                                               
FILENAME DS    CL7                 FILE NAME                                    
DIRNAME  DS    CL7                 DATAMGR DEMO DIRECTORY                       
DEMKEY   DS    XL24                KEY                                          
DEMKEYSV DS    XL24                                                             
*                                                                               
MAJKEY   DC    XL(PRKSTAT-PRKEY)'00'                                            
FILEKEY  DC    XL(PRRLEN-PRKEY)'00'                                             
THISKEY  DC    XL20'00'                                                         
*                                  LAST TIME VALUES                             
PREVKEY  DC    XL20'00'                                                         
*                                                                               
DEMOREC  DS    XL2000              EXISTING DEMO RECORD                         
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
         EJECT                                                                  
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DEMRGPO   04/16/08'                                      
         END                                                                    
