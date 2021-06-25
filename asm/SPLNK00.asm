*          DATA SET SPLNK00    AT LEVEL 058 AS OF 09/12/17                      
*PHASE T21E00A                                                                  
SPLNK00  TITLE '- SPOT SYSTEM DDLINK INTERFACE'                                 
SPLNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKLON,**SL00**,RR=RE,CLEAR=YES                                 
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
                                                                                
         ST    RB,BASEADDR                                                      
         ST    RE,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,AFAPARM                                                       
                                                                                
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
                                                                                
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         L     RF,ACOMFACS         LOAD SUPPORT ROUTINES                        
         L     RF,CCALLOV-COMFACSD(RF)                                          
         LA    R1,DMCB                                                          
         GOTOR (RF),(R1),('P#ROUTS1',0),0,0                                     
         MVC   AROUTS1,0(R1)       SET A(ROUTINE OVERLAY 1)                     
         GOTOR (RF),(R1),('P#ROUTS2',0),0,0                                     
         MVC   AROUTS2,0(R1)       SET A(ROUTINE OVERLAY 2)                     
                                                                                
         LAY   R2,LINKW                                                         
         USING LINKW,R2                                                         
         USING RUNPARMD,DDRUNP     RUNPARMD EMULATION                           
         USING RUNFACSD,DDRUNF     RUNFACS EMULATION                            
         USING LP_D,DDLINKC        DDLINK CONTROL BLOCK                         
         USING FALINKD,FALINKC     FALINK CONTROL BLOCK                         
         USING WRKIOD,DDWRKIOC     WRKIO CONTROL BLOCK                          
                                                                                
         LA    R0,LP_D             SET A(LP_D) IN GLOBAL W/S                    
         ST    R0,ALP                                                           
         GOTOR (#WRKINI,AWRKINI)   INITIALIZE WORKING STORAGE                   
                                                                                
         LA    R0,LNKINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R0,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
         MVC   FALASWCH,VSWITCH    A(SWITCH)                                    
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R0,FAMSGBLK         A(MESSAGE BLOCK)                             
         ST    R0,FALAMSG                                                       
         LA    R0,FACON            A(CONTROL FIELD BUFFER)                      
         ST    R0,FALACON                                                       
         LAY   R0,SVFALINK                                                      
         ST    R0,FALASVE                                                       
         MVC   FALAPGS,=AL4(FALATMS)                                            
         MVI   FALUNCIS,32         SET OVERRIDE NUMBER OF C/IS TO 32            
                                                                                
         MVC   RSVRSAVE,ATIA       FOR DDLINK USE                               
         LA    R0,DDWRKIOC                                                      
         ST    R0,RWRKBLK1         SET A(WRKIO CONTROL BLOCK)                   
         MVC   RCOMFACS,ACOMFACS                                                
         MVC   RWRKIO,VWRKIO                                                    
         MVC   RRUNIT,VRUNIT                                                    
         LA    R0,RUNFACSD                                                      
         STCM  R0,7,RUNPARUN       SET A(RUNFACS) IN RUNPARMS                   
                                                                                
         MVI   LP_AIND1,LP_AICOM+LP_AIUPL+LP_AIRLP                              
         MVC   LP_AGY,TWAAGY       SET AGENCY ALPHA CODE                        
         L     RF,AFAPARM                                                       
         MVC   LP_AGYB,0(RF)       SET AGENCY BINARY VALUE                      
         MVC   LP_USRID,TWAUSRID   SET USER-ID                                  
         MVC   LP_ACCS,TWAACCS     SET LIMIT ACCESS BYTES                       
         MVC   LP_AUIR1,AROUTS1    SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUTS2    SET A(INDEX ROUTINES 2)                      
         MVC   LP_ATSAR,VTSAR      SET A(TSAR)                                  
         LA    R0,TWAD                                                          
         ST    R0,LP_ATWA          SET A(TWA)                                   
         LAY   R0,SVDDLINK                                                      
         ST    R0,LP_ASAVE         SET A(DDLINK SAVE AREA)                      
         LAY   R0,SVSECRET         SET A(SECRET CONTROL BLOCK)                  
         ST    R0,LP_ASECD                                                      
****                                                                            
* SEE SPLNK01                                                                   
****     LA    R0,DDNDX                                                         
****     ST    R0,LP_ANDX          SET A(MASTER MAP INDEX)                      
         MVC   LP_ATIOB,ATIOB      SET A(TIOB)                                  
         MVC   LP_ACOM,ACOMFACS    SET A(COMFACS)                               
         LAY   R0,DDLINKW                                                       
         ST    R0,LP_AWORK         SET A(DDLINK WORK AREA)                      
         LAY   R0,WMP                                                           
         ST    R0,LP_AUWMP         SET A(WMP)                                   
         LHI   R0,L'WMP                                                         
         STH   R0,LP_WMPL                                                       
         MVC   LP_RECL,=H'10240'   SET SIZE OF WORKER FILE TO 10240             
         LA    R0,RUNPARMD                                                      
         ST    R0,LP_ARUNP         SET A(DUMMY RUNPARMS)                        
         LA    R0,FALINKD                                                       
         ST    R0,LP_AFBLK         SET A(FALINKD)                               
         MVC   LP_AFALK,VFALINK    SET A(FALINK)                                
         LA    R0,WORKD                                                         
         ST    R0,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         LAY   R0,SVSERVER                                                      
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         LA    R0,TWAD                                                          
         ST    R0,LP_BLKS+((B#TWAD-1)*L'LP_BLKS)                                
         LHI   R0,SVRETURN-TWAD                                                 
         STCM  R0,3,LP_DSAVE       SET DISP. TO SERVER RETURN AREA              
                                                                                
         MVC   WRKIACOM,ACOMFACS                                                
         L     R0,ATIA                                                          
         AHI   R0,4096             WORKER BUFFER AT TIA+4K                      
         ST    R0,WRKIABUF                                                      
         LAY   R0,WRKIOA                                                        
         ST    R0,WRKIAREC                                                      
                                                                                
         GOTOR (#GETAGY,AGETAGY),TWAAGY                                         
                                                                                
         MVI   LP_FLAG,0           FOR TESTING (PATCH HERE)                     
         BAS   RE,CHKGLOB                                                       
*                                                                               
         GOTOR VDDLINK,LP_D        PASS CONTROL TO DDLINK                       
         OI    LNKSERVH+1,X'01'                                                 
         OI    LNKSERVH+6,X'80'                                                 
         OI    LNKINPH+6,X'C0'                                                  
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
CHKGLOB  NTR1                                                                   
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,GLVXLENQ,GLVXCTL                     
         TM    DMCB+8,X'10'        TEST NOT FOUND                               
         JO    EXIT                                                             
*                                                                               
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         CLC   GLVXFRSY(6),=C'STRTRA' IS THIS FROM SPOT/TRAFFIC                 
         JNE   EXIT                   NO                                        
         DROP  R1                                                               
                                                                                
* RETRIEVE TRAFFIC INTERFACE ELEM                                               
                                                                                
         GOTO1 VGLOBBER,DMCB,=C'GETD',ELEM,24,GLVSPTRF                          
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         LA    R1,ELEM                                                          
         USING GLVTRFD,R1                                                       
* PATCH P06 BELOW TO NEO TO SEE UNCOMPRESSED DATA                               
         MVI   LNKINPH+5,13                                                     
         MVC   LNKINP(13),=C'P06 D=0XXXAAT'  MAPCODE 10A=AUTO/GEN/BLD           
         MVC   LNKINP+7(3),TRFROUT           MOVE ROUTINE VALUE                 
*                                                                               
CHKGLOB2 GOTOR VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
* SPLNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
         PRINT ON                                                               
       ++INCLUDE GEMAPEQUS                                                      
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLVSPTRF                                                     
       ++INCLUDE DDGLOBEQUS                                                     
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058SPLNK00   09/12/17'                                      
         END                                                                    
